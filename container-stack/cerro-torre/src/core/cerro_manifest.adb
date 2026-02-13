-------------------------------------------------------------------------------
--  Cerro_Manifest - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------

--  Parser implementation uses standard Ada patterns not compatible with SPARK
--  (functions with in out parameters, local subprogram bodies without specs)
pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Cerro_CTP_Lexer; use Cerro_CTP_Lexer;

package body Cerro_Manifest is

   ---------------------------------------------------------------------------
   --  Constants for validation
   ---------------------------------------------------------------------------

   Max_Package_Name_Length : constant := 64;

   ---------------------------------------------------------------------------
   --  Parser State
   ---------------------------------------------------------------------------

   type Parser_State is record
      Lexer       : Lexer_State;
      Current_Tok : Token;
      Has_Error   : Boolean;
      Error_Line  : Natural;
      Error_Msg   : Unbounded_String;
   end record;

   ---------------------------------------------------------------------------
   --  Forward declarations
   ---------------------------------------------------------------------------

   procedure Advance (P : in out Parser_State);
   procedure Skip_Value (P : in out Parser_State);
   procedure Skip_Unknown_Section (P : in out Parser_State);
   procedure Parse_Section (P : in out Parser_State; M : in out Manifest);
   procedure Parse_Metadata (P : in out Parser_State; M : in out Manifest);
   procedure Parse_Provenance (P : in out Parser_State; M : in out Manifest);
   procedure Parse_Dependencies (P : in out Parser_State; M : in out Manifest);
   procedure Parse_Build (P : in out Parser_State; M : in out Manifest);
   procedure Parse_Outputs (P : in out Parser_State; M : in out Manifest);
   procedure Parse_Attestations (P : in out Parser_State; M : in out Manifest);
   function Parse_String_Value (P : in out Parser_State) return Unbounded_String;
   function Parse_Integer_Value (P : in out Parser_State) return Integer;
   function Parse_String_List (P : in out Parser_State) return String_List;
   function Parse_Dependency_List (P : in out Parser_State) return Dependency_List;

   ---------------------------------------------------------------------------
   --  Is_Valid_Package_Name
   ---------------------------------------------------------------------------

   function Is_Valid_Package_Name (Name : String) return Boolean is
   begin
      --  Check length
      if Name'Length < 2 or Name'Length > Max_Package_Name_Length then
         return False;
      end if;

      --  First character must be lowercase letter
      if Name (Name'First) not in 'a' .. 'z' then
         return False;
      end if;

      --  Check all characters
      for I in Name'Range loop
         declare
            C : constant Character := Name (I);
         begin
            --  Valid characters: a-z, 0-9, -, +, .
            if C not in 'a' .. 'z' | '0' .. '9' | '-' | '+' | '.' then
               return False;
            end if;

            --  Cannot start or end with -, +, or .
            if (I = Name'First or I = Name'Last) and
               C in '-' | '+' | '.'
            then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Is_Valid_Package_Name;

   ---------------------------------------------------------------------------
   --  Advance - Get next token, skip comments and newlines for values
   ---------------------------------------------------------------------------

   procedure Advance (P : in out Parser_State) is
   begin
      Next_Token (P.Lexer, P.Current_Tok);
   end Advance;

   ---------------------------------------------------------------------------
   --  Skip_Newlines - Skip newline tokens
   ---------------------------------------------------------------------------

   procedure Skip_Newlines (P : in out Parser_State) is
   begin
      while P.Current_Tok.Kind = Tok_Newline or
            P.Current_Tok.Kind = Tok_Comment
      loop
         Advance (P);
      end loop;
   end Skip_Newlines;

   ---------------------------------------------------------------------------
   --  Set_Error - Record a parse error
   ---------------------------------------------------------------------------

   procedure Set_Error (P : in out Parser_State; Msg : String) is
   begin
      P.Has_Error := True;
      P.Error_Line := P.Current_Tok.Line;
      P.Error_Msg := To_Unbounded_String (Msg);
   end Set_Error;

   ---------------------------------------------------------------------------
   --  Expect - Expect a specific token kind
   ---------------------------------------------------------------------------

   function Expect (P : in out Parser_State; Kind : Token_Kind) return Boolean is
   begin
      if P.Current_Tok.Kind /= Kind then
         Set_Error (P, "Expected " & Token_Kind'Image (Kind) &
                       " but got " & Token_Kind'Image (P.Current_Tok.Kind));
         return False;
      end if;
      return True;
   end Expect;

   ---------------------------------------------------------------------------
   --  Parse_String_Value - Parse a string value after =
   ---------------------------------------------------------------------------

   function Parse_String_Value (P : in out Parser_State) return Unbounded_String is
   begin
      if P.Current_Tok.Kind = Tok_String then
         declare
            Result : constant Unbounded_String := P.Current_Tok.Value;
         begin
            Advance (P);
            return Result;
         end;
      else
         Set_Error (P, "Expected string value");
         return Null_Unbounded_String;
      end if;
   end Parse_String_Value;

   ---------------------------------------------------------------------------
   --  Parse_Integer_Value - Parse an integer value after =
   ---------------------------------------------------------------------------

   function Parse_Integer_Value (P : in out Parser_State) return Integer is
   begin
      if P.Current_Tok.Kind = Tok_Integer then
         declare
            Val : constant String := To_String (P.Current_Tok.Value);
            Result : Integer;
         begin
            Result := Integer'Value (Val);
            Advance (P);
            return Result;
         exception
            when others =>
               Set_Error (P, "Invalid integer value");
               return 0;
         end;
      else
         Set_Error (P, "Expected integer value");
         return 0;
      end if;
   end Parse_Integer_Value;

   ---------------------------------------------------------------------------
   --  Parse_String_List - Parse ["item1", "item2", ...]
   ---------------------------------------------------------------------------

   function Parse_String_List (P : in out Parser_State) return String_List is
      Result : String_List := String_Vectors.Empty_Vector;
   begin
      --  Expect opening bracket
      if P.Current_Tok.Kind /= Tok_Lbracket then
         Set_Error (P, "Expected '[' for list");
         return Result;
      end if;
      Advance (P);
      Skip_Newlines (P);

      --  Parse list items
      while P.Current_Tok.Kind /= Tok_Rbracket and
            P.Current_Tok.Kind /= Tok_EOF
      loop
         if P.Current_Tok.Kind = Tok_String then
            String_Vectors.Append (Result, P.Current_Tok.Value);
            Advance (P);

            --  Skip optional comma and newlines
            if P.Current_Tok.Kind = Tok_Comma then
               Advance (P);
            end if;
            Skip_Newlines (P);
         else
            Set_Error (P, "Expected string in list");
            exit;
         end if;
      end loop;

      --  Expect closing bracket
      if P.Current_Tok.Kind = Tok_Rbracket then
         Advance (P);
      end if;

      return Result;
   end Parse_String_List;

   ---------------------------------------------------------------------------
   --  Parse_Dependency_Ref - Parse "pkg" or "pkg>=1.0" or "pkg@1.0"
   ---------------------------------------------------------------------------

   function Parse_Dependency_Ref (S : String) return Dependency_Reference is
      Result : Dependency_Reference;
      Pos    : Natural := 0;
   begin
      --  Find constraint operator position
      for I in S'Range loop
         if S (I) in '>' | '<' | '=' | '@' then
            Pos := I;
            exit;
         end if;
      end loop;

      if Pos = 0 then
         --  No constraint - any version
         Result.Name := To_Unbounded_String (S);
         Result.Constraint := (Kind => Any);
      else
         Result.Name := To_Unbounded_String (S (S'First .. Pos - 1));

         --  Parse constraint
         declare
            Constraint_Str : constant String := S (Pos .. S'Last);
         begin
            if Constraint_Str'Length > 0 then
               if Constraint_Str (Constraint_Str'First) = '@' then
                  --  Exact version: pkg@1.0
                  Result.Constraint := (Kind => Exact,
                     Bound => To_Unbounded_String (Constraint_Str (Constraint_Str'First + 1 .. Constraint_Str'Last)));
               elsif Constraint_Str'Length >= 2 and then
                     Constraint_Str (Constraint_Str'First .. Constraint_Str'First + 1) = ">="
               then
                  --  Minimum: pkg>=1.0
                  Result.Constraint := (Kind => Minimum,
                     Bound => To_Unbounded_String (Constraint_Str (Constraint_Str'First + 2 .. Constraint_Str'Last)));
               elsif Constraint_Str'Length >= 2 and then
                     Constraint_Str (Constraint_Str'First .. Constraint_Str'First + 1) = "<="
               then
                  --  Maximum: pkg<=1.0
                  Result.Constraint := (Kind => Maximum,
                     Bound => To_Unbounded_String (Constraint_Str (Constraint_Str'First + 2 .. Constraint_Str'Last)));
               else
                  Result.Constraint := (Kind => Any);
               end if;
            else
               Result.Constraint := (Kind => Any);
            end if;
         end;
      end if;

      return Result;
   end Parse_Dependency_Ref;

   ---------------------------------------------------------------------------
   --  Parse_Dependency_List - Parse list of dependency references
   ---------------------------------------------------------------------------

   function Parse_Dependency_List (P : in out Parser_State) return Dependency_List is
      Result : Dependency_List := Dependency_Vectors.Empty_Vector;
   begin
      --  Expect opening bracket
      if P.Current_Tok.Kind /= Tok_Lbracket then
         Set_Error (P, "Expected '[' for dependency list");
         return Result;
      end if;
      Advance (P);
      Skip_Newlines (P);

      --  Parse list items
      while P.Current_Tok.Kind /= Tok_Rbracket and
            P.Current_Tok.Kind /= Tok_EOF
      loop
         if P.Current_Tok.Kind = Tok_String then
            declare
               Dep : constant Dependency_Reference :=
                  Parse_Dependency_Ref (To_String (P.Current_Tok.Value));
            begin
               Dependency_Vectors.Append (Result, Dep);
            end;
            Advance (P);

            --  Skip optional comma and newlines
            if P.Current_Tok.Kind = Tok_Comma then
               Advance (P);
            end if;
            Skip_Newlines (P);
         else
            Set_Error (P, "Expected string in dependency list");
            exit;
         end if;
      end loop;

      --  Expect closing bracket
      if P.Current_Tok.Kind = Tok_Rbracket then
         Advance (P);
      end if;

      return Result;
   end Parse_Dependency_List;

   ---------------------------------------------------------------------------
   --  Parse_Hash_Value - Parse "sha256:abc123..." format
   ---------------------------------------------------------------------------

   function Parse_Hash_Value (S : String) return Hash_Value is
      Result    : Hash_Value;
      Colon_Pos : Natural := 0;
   begin
      --  Find colon separator
      for I in S'Range loop
         if S (I) = ':' then
            Colon_Pos := I;
            exit;
         end if;
      end loop;

      if Colon_Pos > 0 then
         declare
            Algo_Str : constant String := S (S'First .. Colon_Pos - 1);
         begin
            --  Determine algorithm
            if Algo_Str = "sha256" then
               Result.Algorithm := SHA256;
            elsif Algo_Str = "sha384" then
               Result.Algorithm := SHA384;
            elsif Algo_Str = "sha512" then
               Result.Algorithm := SHA512;
            elsif Algo_Str = "blake3" then
               Result.Algorithm := Blake3;
            elsif Algo_Str = "shake256" or Algo_Str = "shake3-256" then
               Result.Algorithm := Shake256;
            else
               Result.Algorithm := SHA256;  --  Default
            end if;

            --  Extract digest
            Result.Digest := To_Unbounded_String (S (Colon_Pos + 1 .. S'Last));
         end;
      else
         --  No algorithm prefix, assume SHA256
         Result.Algorithm := SHA256;
         Result.Digest := To_Unbounded_String (S);
      end if;

      return Result;
   end Parse_Hash_Value;

   ---------------------------------------------------------------------------
   --  Parse_Metadata - Parse [metadata] section
   ---------------------------------------------------------------------------

   procedure Parse_Metadata (P : in out Parser_State; M : in out Manifest) is
   begin
      --  Skip section header line
      Skip_Newlines (P);

      --  Parse key=value pairs until next section or EOF
      while P.Current_Tok.Kind = Tok_Identifier loop
         declare
            Key : constant String := To_String (P.Current_Tok.Value);
         begin
            Advance (P);  --  Skip key

            --  Expect =
            if P.Current_Tok.Kind /= Tok_Equals then
               Set_Error (P, "Expected '=' after key");
               return;
            end if;
            Advance (P);  --  Skip =

            --  Parse value based on key
            if Key = "name" then
               M.Metadata.Name := Parse_String_Value (P);
            elsif Key = "version" then
               declare
                  Ver_Str : constant String := To_String (Parse_String_Value (P));
               begin
                  M.Metadata.Version.Upstream := To_Unbounded_String (Ver_Str);
               end;
            elsif Key = "revision" then
               declare
                  Rev : constant Integer := Parse_Integer_Value (P);
               begin
                  if Rev > 0 then
                     M.Metadata.Version.Revision := Rev;
                  end if;
               end;
            elsif Key = "epoch" then
               M.Metadata.Version.Epoch := Natural (Parse_Integer_Value (P));
            elsif Key = "summary" then
               M.Metadata.Summary := Parse_String_Value (P);
            elsif Key = "description" then
               M.Metadata.Description := Parse_String_Value (P);
            elsif Key = "license" then
               M.Metadata.License := Parse_String_Value (P);
            elsif Key = "homepage" then
               M.Metadata.Homepage := Parse_String_Value (P);
            elsif Key = "maintainer" then
               M.Metadata.Maintainer := Parse_String_Value (P);
            else
               --  Skip unknown key
               Skip_Value (P);
            end if;

            Skip_Newlines (P);
         end;
      end loop;
   end Parse_Metadata;

   ---------------------------------------------------------------------------
   --  Parse_Provenance - Parse [provenance] section
   ---------------------------------------------------------------------------

   procedure Parse_Provenance (P : in out Parser_State; M : in out Manifest) is
   begin
      Skip_Newlines (P);

      while P.Current_Tok.Kind = Tok_Identifier loop
         declare
            Key : constant String := To_String (P.Current_Tok.Value);
         begin
            Advance (P);

            if P.Current_Tok.Kind /= Tok_Equals then
               Set_Error (P, "Expected '=' after key");
               return;
            end if;
            Advance (P);

            if Key = "upstream" then
               M.Provenance.Upstream_URL := Parse_String_Value (P);
            elsif Key = "upstream_hash" then
               declare
                  Hash_Str : constant String := To_String (Parse_String_Value (P));
               begin
                  M.Provenance.Upstream_Hash := Parse_Hash_Value (Hash_Str);
               end;
            elsif Key = "upstream_signature" then
               M.Provenance.Upstream_Signature := Parse_String_Value (P);
            elsif Key = "upstream_keyring" then
               M.Provenance.Upstream_Keyring := Parse_String_Value (P);
            elsif Key = "imported_from" then
               M.Provenance.Imported_From := Parse_String_Value (P);
            elsif Key = "patches" then
               M.Provenance.Patches := Parse_String_List (P);
            else
               --  Skip unknown key (including import_date for now)
               Skip_Value (P);
            end if;

            Skip_Newlines (P);
         end;
      end loop;
   end Parse_Provenance;

   ---------------------------------------------------------------------------
   --  Parse_Dependencies - Parse [dependencies] section
   ---------------------------------------------------------------------------

   procedure Parse_Dependencies (P : in out Parser_State; M : in out Manifest) is
   begin
      Skip_Newlines (P);

      while P.Current_Tok.Kind = Tok_Identifier loop
         declare
            Key : constant String := To_String (P.Current_Tok.Value);
         begin
            Advance (P);

            if P.Current_Tok.Kind /= Tok_Equals then
               Set_Error (P, "Expected '=' after key");
               return;
            end if;
            Advance (P);

            if Key = "runtime" then
               M.Dependencies.Runtime := Parse_Dependency_List (P);
            elsif Key = "build" then
               M.Dependencies.Build := Parse_Dependency_List (P);
            elsif Key = "check" then
               M.Dependencies.Check := Parse_Dependency_List (P);
            elsif Key = "optional" then
               M.Dependencies.Optional := Parse_Dependency_List (P);
            elsif Key = "conflicts" then
               M.Dependencies.Conflicts := Parse_Dependency_List (P);
            elsif Key = "provides" then
               M.Dependencies.Provides := Parse_Dependency_List (P);
            elsif Key = "replaces" then
               M.Dependencies.Replaces := Parse_Dependency_List (P);
            else
               --  Skip unknown key
               Skip_Value (P);
            end if;

            Skip_Newlines (P);
         end;
      end loop;
   end Parse_Dependencies;

   ---------------------------------------------------------------------------
   --  Parse_Build_System - Parse build system string to enum
   ---------------------------------------------------------------------------

   function Parse_Build_System (S : String) return Build_System is
   begin
      if S = "autoconf" then
         return Autoconf;
      elsif S = "cmake" then
         return CMake;
      elsif S = "meson" then
         return Meson;
      elsif S = "cargo" then
         return Cargo;
      elsif S = "alire" then
         return Alire;
      elsif S = "dune" then
         return Dune;
      elsif S = "mix" then
         return Mix;
      elsif S = "make" then
         return Make;
      else
         return Custom;
      end if;
   end Parse_Build_System;

   ---------------------------------------------------------------------------
   --  Parse_Build - Parse [build] section
   ---------------------------------------------------------------------------

   procedure Parse_Build (P : in out Parser_State; M : in out Manifest) is
   begin
      Skip_Newlines (P);

      while P.Current_Tok.Kind = Tok_Identifier loop
         declare
            Key : constant String := To_String (P.Current_Tok.Value);
         begin
            Advance (P);

            if P.Current_Tok.Kind /= Tok_Equals then
               Set_Error (P, "Expected '=' after key");
               return;
            end if;
            Advance (P);

            if Key = "system" then
               declare
                  Sys_Str : constant String := To_String (Parse_String_Value (P));
               begin
                  M.Build.System := Parse_Build_System (Sys_Str);
               end;
            elsif Key = "configure_flags" then
               M.Build.Configure_Flags := Parse_String_List (P);
            elsif Key = "build_flags" then
               M.Build.Build_Flags := Parse_String_List (P);
            elsif Key = "install_flags" then
               M.Build.Install_Flags := Parse_String_List (P);
            else
               --  Skip unknown key (subsections handled separately)
               Skip_Value (P);
            end if;

            Skip_Newlines (P);
         end;
      end loop;
   end Parse_Build;

   ---------------------------------------------------------------------------
   --  Parse_Outputs - Parse [outputs] section
   ---------------------------------------------------------------------------

   procedure Parse_Outputs (P : in out Parser_State; M : in out Manifest) is
   begin
      Skip_Newlines (P);

      while P.Current_Tok.Kind = Tok_Identifier loop
         declare
            Key : constant String := To_String (P.Current_Tok.Value);
         begin
            Advance (P);

            if P.Current_Tok.Kind /= Tok_Equals then
               Set_Error (P, "Expected '=' after key");
               return;
            end if;
            Advance (P);

            if Key = "primary" then
               M.Outputs.Primary := Parse_String_Value (P);
            elsif Key = "split" then
               M.Outputs.Split := Parse_String_List (P);
            else
               --  Skip unknown key
               Skip_Value (P);
            end if;

            Skip_Newlines (P);
         end;
      end loop;
   end Parse_Outputs;

   ---------------------------------------------------------------------------
   --  Parse_Attestation_Type - Parse attestation string to enum
   ---------------------------------------------------------------------------

   function Parse_Attestation_Type (S : String) return Attestation_Type is
   begin
      if S = "source-signature" then
         return Source_Signature;
      elsif S = "reproducible-build" then
         return Reproducible_Build;
      elsif S = "sbom-complete" then
         return SBOM_Complete;
      elsif S = "security-audit" then
         return Security_Audit;
      elsif S = "fuzz-tested" then
         return Fuzz_Tested;
      else
         return Source_Signature;  --  Default
      end if;
   end Parse_Attestation_Type;

   ---------------------------------------------------------------------------
   --  Parse_Attestation_List - Parse list of attestation types
   ---------------------------------------------------------------------------

   function Parse_Attestation_List (P : in out Parser_State) return Attestation_List is
      Strings : constant String_List := Parse_String_List (P);
      Result  : Attestation_List := Attestation_Vectors.Empty_Vector;
   begin
      for S of Strings loop
         Attestation_Vectors.Append (Result, Parse_Attestation_Type (To_String (S)));
      end loop;
      return Result;
   end Parse_Attestation_List;

   ---------------------------------------------------------------------------
   --  Parse_Attestations - Parse [attestations] section
   ---------------------------------------------------------------------------

   procedure Parse_Attestations (P : in out Parser_State; M : in out Manifest) is
   begin
      Skip_Newlines (P);

      while P.Current_Tok.Kind = Tok_Identifier loop
         declare
            Key : constant String := To_String (P.Current_Tok.Value);
         begin
            Advance (P);

            if P.Current_Tok.Kind /= Tok_Equals then
               Set_Error (P, "Expected '=' after key");
               return;
            end if;
            Advance (P);

            if Key = "require" then
               M.Attestations.Required := Parse_Attestation_List (P);
            elsif Key = "recommend" then
               M.Attestations.Recommended := Parse_Attestation_List (P);
            else
               --  Skip unknown key
               Skip_Value (P);
            end if;

            Skip_Newlines (P);
         end;
      end loop;
   end Parse_Attestations;

   ---------------------------------------------------------------------------
   --  Skip_Value - Skip a complete value (including arrays and inline tables)
   ---------------------------------------------------------------------------

   procedure Skip_Value (P : in out Parser_State) is
      Depth : Natural := 0;
   begin
      --  Handle array values
      if P.Current_Tok.Kind = Tok_Lbracket then
         Depth := 1;
         Advance (P);
         while Depth > 0 and P.Current_Tok.Kind /= Tok_EOF loop
            if P.Current_Tok.Kind = Tok_Lbracket then
               Depth := Depth + 1;
            elsif P.Current_Tok.Kind = Tok_Rbracket then
               Depth := Depth - 1;
            end if;
            Advance (P);
         end loop;
      --  Handle inline table values
      elsif P.Current_Tok.Kind = Tok_Lbrace then
         Depth := 1;
         Advance (P);
         while Depth > 0 and P.Current_Tok.Kind /= Tok_EOF loop
            if P.Current_Tok.Kind = Tok_Lbrace then
               Depth := Depth + 1;
            elsif P.Current_Tok.Kind = Tok_Rbrace then
               Depth := Depth - 1;
            end if;
            Advance (P);
         end loop;
      --  Simple value (string, integer, boolean, etc.)
      else
         Advance (P);
      end if;
   end Skip_Value;

   ---------------------------------------------------------------------------
   --  Skip_Unknown_Section - Skip key=value pairs until next section
   ---------------------------------------------------------------------------

   procedure Skip_Unknown_Section (P : in out Parser_State) is
   begin
      Skip_Newlines (P);

      --  Parse key=value pairs until we hit a section header or EOF
      while P.Current_Tok.Kind = Tok_Identifier loop
         Advance (P);  --  Skip key

         if P.Current_Tok.Kind = Tok_Equals then
            Advance (P);  --  Skip =
            Skip_Value (P);  --  Skip the entire value (including arrays)
         end if;

         Skip_Newlines (P);
      end loop;
   end Skip_Unknown_Section;

   ---------------------------------------------------------------------------
   --  Parse_Section - Parse a section starting with [name] or [[name]]
   ---------------------------------------------------------------------------

   procedure Parse_Section (P : in out Parser_State; M : in out Manifest) is
      Is_Array_Table : Boolean := False;
   begin
      --  We're at [, skip it
      Advance (P);

      --  Check for [[ (array of tables)
      if P.Current_Tok.Kind = Tok_Lbracket then
         Is_Array_Table := True;
         Advance (P);  --  Skip second [
      end if;

      --  Get section name (may contain dots like "build.environment")
      if P.Current_Tok.Kind /= Tok_Identifier then
         Set_Error (P, "Expected section name after '['");
         return;
      end if;

      declare
         Section_Name : Unbounded_String := P.Current_Tok.Value;
      begin
         Advance (P);  --  Skip first part of section name

         --  Handle dotted section names like [build.environment]
         while P.Current_Tok.Kind = Tok_Dot loop
            Append (Section_Name, ".");
            Advance (P);  --  Skip .
            if P.Current_Tok.Kind = Tok_Identifier then
               Append (Section_Name, P.Current_Tok.Value);
               Advance (P);
            end if;
         end loop;

         --  Expect ] (or ]] for array tables)
         if P.Current_Tok.Kind /= Tok_Rbracket then
            Set_Error (P, "Expected ']' after section name");
            return;
         end if;
         Advance (P);  --  Skip ]

         --  For array tables, expect second ]
         if Is_Array_Table then
            if P.Current_Tok.Kind /= Tok_Rbracket then
               Set_Error (P, "Expected ']]' for array table");
               return;
            end if;
            Advance (P);  --  Skip second ]
         end if;

         Skip_Newlines (P);

         declare
            Name : constant String := To_String (Section_Name);
         begin
            --  Route to appropriate section parser
            if Name = "metadata" then
               Parse_Metadata (P, M);
            elsif Name = "provenance" then
               Parse_Provenance (P, M);
            elsif Name = "dependencies" then
               Parse_Dependencies (P, M);
            elsif Name = "build" then
               Parse_Build (P, M);
            elsif Name = "outputs" then
               Parse_Outputs (P, M);
            elsif Name = "attestations" then
               Parse_Attestations (P, M);
            else
               --  Skip unknown section (including array tables, subsections)
               --  Read key=value pairs until next section
               Skip_Unknown_Section (P);
            end if;
         end;
      end;
   end Parse_Section;

   ---------------------------------------------------------------------------
   --  Parse_String - Main entry point for parsing from string
   ---------------------------------------------------------------------------

   function Parse_String (Content : String) return Parse_Result is
      P : Parser_State;
      M : Manifest;
   begin
      --  Initialize parser
      P.Lexer := Init (Content);
      P.Has_Error := False;
      P.Error_Line := 0;
      P.Error_Msg := Null_Unbounded_String;

      --  Get first token
      Advance (P);
      Skip_Newlines (P);

      --  Parse sections
      while P.Current_Tok.Kind /= Tok_EOF and not P.Has_Error loop
         if P.Current_Tok.Kind = Tok_Lbracket then
            Parse_Section (P, M);
         elsif P.Current_Tok.Kind = Tok_Identifier then
            --  Top-level key=value (like ctp_version = "1.0")
            Advance (P);
            if P.Current_Tok.Kind = Tok_Equals then
               Advance (P);  --  Skip =
               Advance (P);  --  Skip value
            end if;
            Skip_Newlines (P);
         else
            Advance (P);
            Skip_Newlines (P);
         end if;
      end loop;

      --  Return result
      if P.Has_Error then
         return (Success => False,
                 Error => Syntax_Error,
                 Error_Line => P.Error_Line,
                 Error_Msg => P.Error_Msg);
      else
         return (Success => True,
                 Value => M);
      end if;
   end Parse_String;

   ---------------------------------------------------------------------------
   --  Parse_File - Parse manifest from file
   ---------------------------------------------------------------------------

   function Parse_File (Path : String) return Parse_Result is
      use Ada.Text_IO;
      File    : File_Type;
      Content : Unbounded_String := Null_Unbounded_String;
      Line    : String (1 .. 4096);
      Last    : Natural;
   begin
      --  Open file
      begin
         Open (File, In_File, Path);
      exception
         when others =>
            return (Success => False,
                    Error => File_Not_Found,
                    Error_Line => 0,
                    Error_Msg => To_Unbounded_String ("Cannot open file: " & Path));
      end;

      --  Read entire file
      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         Append (Content, Line (1 .. Last));
         Append (Content, ASCII.LF);
      end loop;

      Close (File);

      --  Parse content
      return Parse_String (To_String (Content));
   end Parse_File;

   ---------------------------------------------------------------------------
   --  Is_Complete - Check if manifest has all required fields
   ---------------------------------------------------------------------------

   function Is_Complete (M : Manifest) return Boolean is
   begin
      --  Required fields in metadata
      if Length (M.Metadata.Name) = 0 then
         return False;
      end if;
      if Length (M.Metadata.Summary) = 0 then
         return False;
      end if;
      if Length (M.Metadata.License) = 0 then
         return False;
      end if;
      if Length (M.Metadata.Maintainer) = 0 then
         return False;
      end if;

      --  Required fields in provenance (for simple packages)
      if Length (M.Provenance.Upstream_URL) > 0 then
         if not Is_Valid_Hash (M.Provenance.Upstream_Hash) then
            return False;
         end if;
      end if;

      --  Required fields in build
      --  (Build.System has a default, so no check needed)

      --  Required fields in outputs
      if Length (M.Outputs.Primary) = 0 then
         return False;
      end if;

      return True;
   end Is_Complete;

   ---------------------------------------------------------------------------
   --  Is_Valid_Hash - Validate hash format
   ---------------------------------------------------------------------------

   function Is_Valid_Hash (H : Hash_Value) return Boolean is
      Len : constant Natural := Length (H.Digest);
   begin
      --  Empty digest means hash was never set
      if Len = 0 then
         return False;
      end if;

      case H.Algorithm is
         when SHA256 =>
            return Len = 64;
         when SHA384 =>
            return Len = 96;
         when SHA512 =>
            return Len = 128;
         when Blake3 =>
            return Len = 64;
         when Shake256 =>
            --  SHAKE256 is an XOF with variable output length
            --  Common lengths: 64 (256-bit), 128 (512-bit)
            return Len >= 64 and Len mod 2 = 0;
      end case;
   end Is_Valid_Hash;

   ---------------------------------------------------------------------------
   --  Is_Valid_Version - Validate version structure
   ---------------------------------------------------------------------------

   function Is_Valid_Version (V : Version) return Boolean is
   begin
      --  Upstream version must be non-empty
      if Length (V.Upstream) = 0 then
         return False;
      end if;

      --  Revision must be positive
      if V.Revision < 1 then
         return False;
      end if;

      return True;
   end Is_Valid_Version;

   ---------------------------------------------------------------------------
   --  To_String - Serialize manifest to CTP format
   ---------------------------------------------------------------------------

   function To_String (M : Manifest) return String is
      Result : Unbounded_String := Null_Unbounded_String;

      procedure Append_Line (S : String) is
      begin
         Append (Result, S);
         Append (Result, ASCII.LF);
      end Append_Line;

      procedure Append_KV (K : String; V : Unbounded_String) is
      begin
         if Length (V) > 0 then
            Append_Line (K & " = """ & To_String (V) & """");
         end if;
      end Append_KV;

      procedure Append_KV_Int (K : String; V : Integer) is
      begin
         Append_Line (K & " = " & Integer'Image (V));
      end Append_KV_Int;

      procedure Append_String_List (K : String; L : String_List) is
      begin
         if not String_Vectors.Is_Empty (L) then
            Append (Result, K & " = [");
            for I in L.First_Index .. L.Last_Index loop
               Append (Result, """" & To_String (L (I)) & """");
               if I < L.Last_Index then
                  Append (Result, ", ");
               end if;
            end loop;
            Append_Line ("]");
         end if;
      end Append_String_List;

   begin
      --  [metadata]
      Append_Line ("[metadata]");
      Append_KV ("name", M.Metadata.Name);
      Append_KV ("version", M.Metadata.Version.Upstream);
      Append_KV_Int ("revision", M.Metadata.Version.Revision);
      if M.Metadata.Version.Epoch > 0 then
         Append_KV_Int ("epoch", M.Metadata.Version.Epoch);
      end if;
      Append_KV ("summary", M.Metadata.Summary);
      Append_KV ("description", M.Metadata.Description);
      Append_KV ("license", M.Metadata.License);
      Append_KV ("homepage", M.Metadata.Homepage);
      Append_KV ("maintainer", M.Metadata.Maintainer);
      Append_Line ("");

      --  [provenance]
      Append_Line ("[provenance]");
      Append_KV ("upstream", M.Provenance.Upstream_URL);
      if Length (M.Provenance.Upstream_Hash.Digest) > 0 then
         declare
            Algo : constant String :=
               (case M.Provenance.Upstream_Hash.Algorithm is
                  when SHA256   => "sha256",
                  when SHA384   => "sha384",
                  when SHA512   => "sha512",
                  when Blake3   => "blake3",
                  when Shake256 => "shake256");
         begin
            Append_Line ("upstream_hash = """ & Algo & ":" &
                        To_String (M.Provenance.Upstream_Hash.Digest) & """");
         end;
      end if;
      Append_KV ("upstream_signature", M.Provenance.Upstream_Signature);
      Append_KV ("imported_from", M.Provenance.Imported_From);
      Append_String_List ("patches", M.Provenance.Patches);
      Append_Line ("");

      --  [dependencies]
      if not Dependency_Vectors.Is_Empty (M.Dependencies.Runtime) or
         not Dependency_Vectors.Is_Empty (M.Dependencies.Build)
      then
         Append_Line ("[dependencies]");
         --  Simplified: would need Dependency_List serialization
         Append_Line ("");
      end if;

      --  [build]
      Append_Line ("[build]");
      declare
         Sys : constant String :=
            (case M.Build.System is
               when Autoconf => "autoconf",
               when CMake    => "cmake",
               when Meson    => "meson",
               when Cargo    => "cargo",
               when Alire    => "alire",
               when Dune     => "dune",
               when Mix      => "mix",
               when Make     => "make",
               when Custom   => "custom");
      begin
         Append_Line ("system = """ & Sys & """");
      end;
      Append_String_List ("configure_flags", M.Build.Configure_Flags);
      Append_String_List ("build_flags", M.Build.Build_Flags);
      Append_String_List ("install_flags", M.Build.Install_Flags);
      Append_Line ("");

      --  [outputs]
      Append_Line ("[outputs]");
      Append_KV ("primary", M.Outputs.Primary);
      Append_String_List ("split", M.Outputs.Split);
      Append_Line ("");

      --  [attestations]
      Append_Line ("[attestations]");
      --  Simplified: would need attestation list serialization

      return To_String (Result);
   end To_String;

end Cerro_Manifest;
