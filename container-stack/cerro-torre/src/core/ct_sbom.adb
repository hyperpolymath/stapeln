-------------------------------------------------------------------------------
--  CT_SBOM - Software Bill of Materials Generation (Implementation)
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
-------------------------------------------------------------------------------

--  Implementation uses Ada.Calendar formatting and random number generation
--  which are not compatible with SPARK mode
pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Ada.Calendar.Formatting;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Handling;

package body CT_SBOM is

   use Ada.Strings.Unbounded;

   ---------------------------------------------------------------------------
   --  Internal Constants
   ---------------------------------------------------------------------------

   Newline : constant Character := ASCII.LF;
   Indent_Unit : constant String := "  ";

   ---------------------------------------------------------------------------
   --  Forward Declarations
   ---------------------------------------------------------------------------

   function Escape_JSON_String (S : String) return String;
   function Build_JSON_Object
     (Pairs       : String;
      Pretty      : Boolean;
      Indent_Level : Natural := 0) return String;
   function Build_JSON_Array
     (Items        : String;
      Pretty       : Boolean;
      Indent_Level : Natural := 0) return String;

   ---------------------------------------------------------------------------
   --  UUID Generation (v4 random)
   ---------------------------------------------------------------------------

   function Generate_UUID return String is
      type Hex_Range is range 0 .. 15;
      package Hex_Random is new Ada.Numerics.Discrete_Random (Hex_Range);

      Hex_Chars : constant String := "0123456789abcdef";
      Generator : Hex_Random.Generator;
      Result    : String (1 .. 36);
      Position  : Positive := 1;

      procedure Add_Hex is
         Value : constant Hex_Range := Hex_Random.Random (Generator);
      begin
         Result (Position) := Hex_Chars (Integer (Value) + 1);
         Position := Position + 1;
      end Add_Hex;

      procedure Add_Dash is
      begin
         Result (Position) := '-';
         Position := Position + 1;
      end Add_Dash;

   begin
      Hex_Random.Reset (Generator);

      --  UUID v4 format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
      --  where y is one of 8, 9, a, or b

      --  First 8 hex digits
      for I in 1 .. 8 loop
         Add_Hex;
      end loop;
      Add_Dash;

      --  Next 4 hex digits
      for I in 1 .. 4 loop
         Add_Hex;
      end loop;
      Add_Dash;

      --  Version 4 indicator + 3 hex digits
      Result (Position) := '4';
      Position := Position + 1;
      for I in 1 .. 3 loop
         Add_Hex;
      end loop;
      Add_Dash;

      --  Variant (8, 9, a, or b) + 3 hex digits
      declare
         Variant_Chars : constant String := "89ab";
         Variant_Idx   : constant Hex_Range := Hex_Random.Random (Generator) mod 4;
      begin
         Result (Position) := Variant_Chars (Integer (Variant_Idx) + 1);
         Position := Position + 1;
      end;
      for I in 1 .. 3 loop
         Add_Hex;
      end loop;
      Add_Dash;

      --  Last 12 hex digits
      for I in 1 .. 12 loop
         Add_Hex;
      end loop;

      return Result;
   end Generate_UUID;

   ---------------------------------------------------------------------------
   --  JSON String Escaping
   ---------------------------------------------------------------------------

   function Escape_JSON_String (S : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for C of S loop
         case C is
            when '"' =>
               Append (Result, "\""");
            when '\' =>
               Append (Result, "\\");
            when ASCII.BS =>
               Append (Result, "\b");
            when ASCII.FF =>
               Append (Result, "\f");
            when ASCII.LF =>
               Append (Result, "\n");
            when ASCII.CR =>
               Append (Result, "\r");
            when ASCII.HT =>
               Append (Result, "\t");
            when others =>
               if Character'Pos (C) < 32 then
                  --  Control character - encode as \uXXXX
                  declare
                     Hex : constant String := "0123456789abcdef";
                     Code : constant Natural := Character'Pos (C);
                  begin
                     Append (Result, "\u00");
                     Append (Result, Hex ((Code / 16) + 1));
                     Append (Result, Hex ((Code mod 16) + 1));
                  end;
               else
                  Append (Result, C);
               end if;
         end case;
      end loop;
      return To_String (Result);
   end Escape_JSON_String;

   ---------------------------------------------------------------------------
   --  JSON Building Helpers
   ---------------------------------------------------------------------------

   function Make_Indent (Level : Natural) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in 1 .. Level loop
         Append (Result, Indent_Unit);
      end loop;
      return To_String (Result);
   end Make_Indent;

   function Build_JSON_Object
     (Pairs        : String;
      Pretty       : Boolean;
      Indent_Level : Natural := 0) return String
   is
      Indent : constant String := Make_Indent (Indent_Level);
   begin
      if Pairs'Length = 0 then
         return "{}";
      end if;

      if Pretty then
         return "{" & Newline & Pairs & Newline & Indent & "}";
      else
         return "{" & Pairs & "}";
      end if;
   end Build_JSON_Object;

   function Build_JSON_Array
     (Items        : String;
      Pretty       : Boolean;
      Indent_Level : Natural := 0) return String
   is
      Indent : constant String := Make_Indent (Indent_Level);
   begin
      if Items'Length = 0 then
         return "[]";
      end if;

      if Pretty then
         return "[" & Newline & Items & Newline & Indent & "]";
      else
         return "[" & Items & "]";
      end if;
   end Build_JSON_Array;

   function JSON_String (S : String) return String is
   begin
      return """" & Escape_JSON_String (S) & """";
   end JSON_String;

   function JSON_String (S : Unbounded_String) return String is
   begin
      return JSON_String (To_String (S));
   end JSON_String;

   function JSON_KV
     (Key          : String;
      Value        : String;
      Pretty       : Boolean;
      Indent_Level : Natural := 0;
      Is_Last      : Boolean := False) return String
   is
      Indent : constant String := Make_Indent (Indent_Level);
      Sep    : constant String := (if Is_Last then "" else ",");
      NL     : constant String := (if Pretty then "" & Newline else "");
   begin
      if Pretty then
         return Indent & JSON_String (Key) & ": " & Value & Sep & NL;
      else
         return JSON_String (Key) & ":" & Value & Sep;
      end if;
   end JSON_KV;

   ---------------------------------------------------------------------------
   --  Format Functions
   ---------------------------------------------------------------------------

   function Format_Name (F : SBOM_Format) return String is
   begin
      case F is
         when CycloneDX_1_5 => return "CycloneDX";
         when SPDX_2_3      => return "SPDX";
      end case;
   end Format_Name;

   function Format_Version (F : SBOM_Format) return String is
   begin
      case F is
         when CycloneDX_1_5 => return CycloneDX_Spec_Version;
         when SPDX_2_3      => return SPDX_Spec_Version;
      end case;
   end Format_Version;

   ---------------------------------------------------------------------------
   --  Error Messages
   ---------------------------------------------------------------------------

   function Error_Message (E : Generation_Error) return String is
   begin
      case E is
         when None =>
            return "No error";
         when Invalid_Manifest =>
            return "Manifest is missing required fields";
         when Invalid_License =>
            return "Invalid or unrecognized license expression";
         when Serialization_Error =>
            return "Failed to serialize SBOM to JSON";
         when IO_Error =>
            return "File I/O error occurred";
      end case;
   end Error_Message;

   ---------------------------------------------------------------------------
   --  Hash Algorithm Conversion
   ---------------------------------------------------------------------------

   function Convert_Hash_Algorithm
     (Algo : Cerro_Manifest.Hash_Algorithm) return SBOM_Hash_Algorithm
   is
   begin
      case Algo is
         when Cerro_Manifest.SHA256   => return SHA_256;
         when Cerro_Manifest.SHA384   => return SHA_384;
         when Cerro_Manifest.SHA512   => return SHA_512;
         when Cerro_Manifest.Blake3   => return BLAKE3;
         when Cerro_Manifest.Shake256 => return SHA3_256;  --  Map SHAKE to SHA3
      end case;
   end Convert_Hash_Algorithm;

   function Convert_Hash
     (Hash : Cerro_Manifest.Hash_Value) return SBOM_Hash
   is
   begin
      return (Algorithm => Convert_Hash_Algorithm (Hash.Algorithm),
              Content   => Hash.Digest);
   end Convert_Hash;

   function Hash_Algorithm_Name (Algo : SBOM_Hash_Algorithm) return String is
   begin
      case Algo is
         when SHA_256  => return "SHA-256";
         when SHA_384  => return "SHA-384";
         when SHA_512  => return "SHA-512";
         when BLAKE3   => return "BLAKE3";
         when SHA3_256 => return "SHA3-256";
         when SHA3_512 => return "SHA3-512";
      end case;
   end Hash_Algorithm_Name;

   ---------------------------------------------------------------------------
   --  PURL Generation
   ---------------------------------------------------------------------------

   function Generate_PURL
     (Manifest : Cerro_Manifest.Manifest) return Package_URL
   is
      Result : Package_URL;
   begin
      Result.Pkg_Type := To_Unbounded_String (CT_PURL_Type);
      Result.Name := Manifest.Metadata.Name;

      --  Use version.upstream as the version string
      Result.Version := Manifest.Metadata.Version.Upstream;

      --  Extract namespace from maintainer (organization part)
      declare
         Maintainer : constant String := To_String (Manifest.Metadata.Maintainer);
         At_Pos     : Natural := 0;
      begin
         for I in Maintainer'Range loop
            if Maintainer (I) = '@' then
               At_Pos := I;
               exit;
            end if;
         end loop;

         if At_Pos > 0 then
            --  Extract domain part as namespace
            Result.Namespace := To_Unbounded_String
              (Maintainer (At_Pos + 1 .. Maintainer'Last));
         end if;
      end;

      return Result;
   end Generate_PURL;

   function Generate_PURL
     (Pkg_Type  : String;
      Namespace : String;
      Name      : String;
      Version   : String) return Package_URL
   is
   begin
      return (Pkg_Type   => To_Unbounded_String (Pkg_Type),
              Namespace  => To_Unbounded_String (Namespace),
              Name       => To_Unbounded_String (Name),
              Version    => To_Unbounded_String (Version),
              Qualifiers => Null_Unbounded_String,
              Subpath    => Null_Unbounded_String);
   end Generate_PURL;

   function PURL_To_String (P : Package_URL) return String is
      Result : Unbounded_String := To_Unbounded_String ("pkg:");
   begin
      Append (Result, P.Pkg_Type);
      Append (Result, "/");

      if Length (P.Namespace) > 0 then
         Append (Result, P.Namespace);
         Append (Result, "/");
      end if;

      Append (Result, P.Name);

      if Length (P.Version) > 0 then
         Append (Result, "@");
         Append (Result, P.Version);
      end if;

      if Length (P.Qualifiers) > 0 then
         Append (Result, "?");
         Append (Result, P.Qualifiers);
      end if;

      if Length (P.Subpath) > 0 then
         Append (Result, "#");
         Append (Result, P.Subpath);
      end if;

      return To_String (Result);
   end PURL_To_String;

   ---------------------------------------------------------------------------
   --  CPE Generation
   ---------------------------------------------------------------------------

   function Generate_CPE
     (Manifest : Cerro_Manifest.Manifest) return CPE_Identifier
   is
      Result : CPE_Identifier;
   begin
      Result.Part := To_Unbounded_String ("a");  --  Application
      Result.Product := Manifest.Metadata.Name;
      Result.Version := Manifest.Metadata.Version.Upstream;

      --  Extract vendor from maintainer
      declare
         Maintainer : constant String := To_String (Manifest.Metadata.Maintainer);
         At_Pos     : Natural := 0;
         Space_Pos  : Natural := 0;
      begin
         --  Find @ for email or space for "Name <email>"
         for I in Maintainer'Range loop
            if Maintainer (I) = '@' then
               At_Pos := I;
            elsif Maintainer (I) = ' ' or Maintainer (I) = '<' then
               Space_Pos := I;
               exit;
            end if;
         end loop;

         if Space_Pos > 0 then
            Result.Vendor := To_Unbounded_String
              (Maintainer (Maintainer'First .. Space_Pos - 1));
         elsif At_Pos > 0 then
            Result.Vendor := To_Unbounded_String
              (Maintainer (At_Pos + 1 .. Maintainer'Last));
         else
            Result.Vendor := To_Unbounded_String (Maintainer);
         end if;
      end;

      return Result;
   end Generate_CPE;

   function Generate_CPE
     (Vendor  : String;
      Product : String;
      Version : String) return CPE_Identifier
   is
   begin
      return (Part    => To_Unbounded_String ("a"),
              Vendor  => To_Unbounded_String (Vendor),
              Product => To_Unbounded_String (Product),
              Version => To_Unbounded_String (Version),
              Update  => Null_Unbounded_String,
              Edition => Null_Unbounded_String,
              Language => Null_Unbounded_String);
   end Generate_CPE;

   function CPE_To_String (C : CPE_Identifier) return String is
      function CPE_Escape (S : Unbounded_String) return String is
         Result : Unbounded_String := Null_Unbounded_String;
         Source : constant String := To_String (S);
      begin
         if Length (S) = 0 then
            return "*";
         end if;

         for Ch of Source loop
            --  CPE special characters need escaping
            if Ch in '\' | '*' | '?' | ':' then
               Append (Result, '\');
            end if;
            Append (Result, Ada.Characters.Handling.To_Lower (Ch));
         end loop;
         return To_String (Result);
      end CPE_Escape;

      Part_Str : constant String :=
        (if Length (C.Part) > 0 then To_String (C.Part) else "a");
   begin
      return "cpe:2.3:" & Part_Str & ":" &
             CPE_Escape (C.Vendor) & ":" &
             CPE_Escape (C.Product) & ":" &
             CPE_Escape (C.Version) & ":" &
             CPE_Escape (C.Update) & ":" &
             CPE_Escape (C.Edition) & ":" &
             CPE_Escape (C.Language) & ":*:*:*:*";
   end CPE_To_String;

   ---------------------------------------------------------------------------
   --  License Processing
   ---------------------------------------------------------------------------

   function Is_Valid_SPDX_ID (ID : String) return Boolean is
      --  Basic validation: non-empty, alphanumeric with dashes and dots
   begin
      if ID'Length = 0 then
         return False;
      end if;

      for C of ID loop
         if C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '+' then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_SPDX_ID;

   function Parse_License_Expression
     (Expression : String) return License_List
   is
      Result : License_List := License_Vectors.Empty_Vector;
      Current_Start : Positive := Expression'First;
      Current_End   : Natural := Expression'First - 1;
      In_Parens     : Natural := 0;
   begin
      --  Simple parser: split on " OR " and " AND " (respecting parentheses)
      for I in Expression'Range loop
         declare
            C : constant Character := Expression (I);
         begin
            if C = '(' then
               In_Parens := In_Parens + 1;
            elsif C = ')' then
               if In_Parens > 0 then
                  In_Parens := In_Parens - 1;
               end if;
            end if;

            Current_End := I;

            --  Check for " OR " or " AND " separators at top level
            if In_Parens = 0 and I + 3 <= Expression'Last then
               declare
                  Sep_Check : constant String :=
                    Expression (I .. Natural'Min (I + 4, Expression'Last));
               begin
                  if Sep_Check (I .. I + 3) = " OR " or
                     (Sep_Check'Length >= 5 and then Sep_Check (I .. I + 4) = " AND ")
                  then
                     --  Extract license ID
                     if Current_End > Current_Start then
                        declare
                           ID : constant String :=
                             Expression (Current_Start .. Current_End - 1);
                        begin
                           if Is_Valid_SPDX_ID (ID) then
                              License_Vectors.Append (Result,
                                License_Info'(SPDX_ID    => To_Unbounded_String (ID),
                                              Name       => Null_Unbounded_String,
                                              URL        => Null_Unbounded_String,
                                              Expression => Null_Unbounded_String));
                           end if;
                        end;
                     end if;

                     --  Skip separator
                     Current_Start :=
                       (if Sep_Check (I .. I + 3) = " OR " then I + 4 else I + 5);
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Add final license ID
      if Current_End >= Current_Start then
         declare
            ID : constant String := Expression (Current_Start .. Current_End);
         begin
            if Is_Valid_SPDX_ID (ID) then
               License_Vectors.Append (Result,
                 License_Info'(SPDX_ID    => To_Unbounded_String (ID),
                               Name       => Null_Unbounded_String,
                               URL        => Null_Unbounded_String,
                               Expression => Null_Unbounded_String));
            end if;
         end;
      end if;

      --  Store full expression in first entry if present
      if not License_Vectors.Is_Empty (Result) then
         declare
            First : License_Info := Result.First_Element;
         begin
            First.Expression := To_Unbounded_String (Expression);
            Result.Replace_Element (Result.First_Index, First);
         end;
      end if;

      return Result;
   end Parse_License_Expression;

   ---------------------------------------------------------------------------
   --  Component Conversion
   ---------------------------------------------------------------------------

   function Manifest_To_Component
     (Manifest : Cerro_Manifest.Manifest) return SBOM_Component
   is
      Result : SBOM_Component;
   begin
      --  Identity
      Result.BOM_Ref := To_Unbounded_String
        (To_String (Manifest.Metadata.Name) & "-" &
         To_String (Manifest.Metadata.Version.Upstream));
      Result.Comp_Type := Application;  --  Default; could be Library
      Result.Name := Manifest.Metadata.Name;
      Result.Version := Manifest.Metadata.Version.Upstream;
      Result.Description := Manifest.Metadata.Summary;

      --  Identifiers
      Result.PURL := Generate_PURL (Manifest);
      Result.CPE := Generate_CPE (Manifest);

      --  Provenance
      Result.Publisher := Manifest.Metadata.Maintainer;

      --  Hashes from provenance
      if Length (Manifest.Provenance.Upstream_Hash.Digest) > 0 then
         Hash_Vectors.Append (Result.Hashes,
           SBOM_Hash'(Convert_Hash (Manifest.Provenance.Upstream_Hash)));
      end if;

      --  Licensing
      Result.Licenses := Parse_License_Expression
        (To_String (Manifest.Metadata.License));

      --  Scope
      Result.Scope := Required;

      --  External references
      if Length (Manifest.Metadata.Homepage) > 0 then
         External_Ref_Vectors.Append (Result.External_Refs,
           External_Reference'(Ref_Type => Website,
                               URL      => Manifest.Metadata.Homepage,
                               Comment  => To_Unbounded_String ("Project homepage")));
      end if;

      if Length (Manifest.Provenance.Upstream_URL) > 0 then
         External_Ref_Vectors.Append (Result.External_Refs,
           External_Reference'(Ref_Type => Distribution,
                               URL      => Manifest.Provenance.Upstream_URL,
                               Comment  => To_Unbounded_String ("Upstream source")));
      end if;

      return Result;
   end Manifest_To_Component;

   function Dependency_To_Component
     (Dep : Cerro_Manifest.Dependency_Reference) return SBOM_Component
   is
      Result : SBOM_Component;
      Version_Str : Unbounded_String := Null_Unbounded_String;
   begin
      Result.Name := Dep.Name;
      Result.Comp_Type := Library;  --  Dependencies are typically libraries

      --  Extract version from constraint
      case Dep.Constraint.Kind is
         when Cerro_Manifest.Exact | Cerro_Manifest.Minimum |
              Cerro_Manifest.Maximum =>
            Version_Str := Dep.Constraint.Bound;
         when Cerro_Manifest.Range_Constraint =>
            Version_Str := Dep.Constraint.Lower_Bound;
         when Cerro_Manifest.Any =>
            Version_Str := To_Unbounded_String ("*");
      end case;

      Result.Version := Version_Str;
      Result.BOM_Ref := To_Unbounded_String
        (To_String (Dep.Name) & "-" & To_String (Version_Str));

      --  Generate identifiers
      Result.PURL := Generate_PURL
        (CT_PURL_Type, "", To_String (Dep.Name), To_String (Version_Str));

      return Result;
   end Dependency_To_Component;

   ---------------------------------------------------------------------------
   --  Timestamp Formatting
   ---------------------------------------------------------------------------

   function Format_ISO8601 (T : Ada.Calendar.Time) return String is
   begin
      return Ada.Calendar.Formatting.Image (T, Include_Time_Fraction => False) & "Z";
   end Format_ISO8601;

   ---------------------------------------------------------------------------
   --  CycloneDX JSON Generation
   ---------------------------------------------------------------------------

   function Component_Type_Name (CT : Component_Type) return String is
   begin
      case CT is
         when Application      => return "application";
         when Framework        => return "framework";
         when Library          => return "library";
         when Container        => return "container";
         when Operating_System => return "operating-system";
         when Device           => return "device";
         when Firmware         => return "firmware";
         when File             => return "file";
         when Platform         => return "platform";
         when Machine_Learning => return "machine-learning-model";
         when Data             => return "data";
      end case;
   end Component_Type_Name;

   function External_Ref_Type_Name (RT : External_Reference_Type) return String is
   begin
      case RT is
         when VCS           => return "vcs";
         when Issue_Tracker => return "issue-tracker";
         when Website       => return "website";
         when Advisories    => return "advisories";
         when BOM           => return "bom";
         when Mailing_List  => return "mailing-list";
         when Social        => return "social";
         when Chat          => return "chat";
         when Documentation => return "documentation";
         when Support       => return "support";
         when Distribution  => return "distribution";
         when License       => return "license";
         when Build_Meta    => return "build-meta";
         when Build_System  => return "build-system";
         when Release_Notes => return "release-notes";
         when Other         => return "other";
      end case;
   end External_Ref_Type_Name;

   function Component_To_CycloneDX_JSON
     (Comp         : SBOM_Component;
      Pretty       : Boolean;
      Indent_Level : Natural) return String
   is
      Result : Unbounded_String := Null_Unbounded_String;
      Indent : constant String := Make_Indent (Indent_Level + 1);
      NL     : constant String := (if Pretty then "" & Newline else "");
      First  : Boolean := True;

      procedure Add_Field (Key : String; Value : String) is
      begin
         if Value'Length > 0 then
            if not First then
               Append (Result, "," & NL);
            end if;
            First := False;
            Append (Result, Indent & JSON_String (Key) & ": " & Value);
         end if;
      end Add_Field;

   begin
      --  Required fields
      Add_Field ("type", JSON_String (Component_Type_Name (Comp.Comp_Type)));
      Add_Field ("name", JSON_String (Comp.Name));

      --  Optional fields
      if Length (Comp.BOM_Ref) > 0 then
         Add_Field ("bom-ref", JSON_String (Comp.BOM_Ref));
      end if;

      if Length (Comp.Version) > 0 then
         Add_Field ("version", JSON_String (Comp.Version));
      end if;

      if Length (Comp.Description) > 0 then
         Add_Field ("description", JSON_String (Comp.Description));
      end if;

      if Length (Comp.Publisher) > 0 then
         Add_Field ("publisher", JSON_String (Comp.Publisher));
      end if;

      --  PURL
      if Length (Comp.PURL.Name) > 0 then
         Add_Field ("purl", JSON_String (PURL_To_String (Comp.PURL)));
      end if;

      --  CPE
      if Length (Comp.CPE.Product) > 0 then
         Add_Field ("cpe", JSON_String (CPE_To_String (Comp.CPE)));
      end if;

      --  Hashes
      if not Hash_Vectors.Is_Empty (Comp.Hashes) then
         declare
            Hashes_JSON : Unbounded_String := Null_Unbounded_String;
            H_First     : Boolean := True;
         begin
            for H of Comp.Hashes loop
               if not H_First then
                  Append (Hashes_JSON, "," & NL);
               end if;
               H_First := False;
               Append (Hashes_JSON, Indent & Indent_Unit &
                 "{""alg"": " & JSON_String (Hash_Algorithm_Name (H.Algorithm)) &
                 ", ""content"": " & JSON_String (H.Content) & "}");
            end loop;
            Add_Field ("hashes", "[" & NL & To_String (Hashes_JSON) & NL & Indent & "]");
         end;
      end if;

      --  Licenses
      if not License_Vectors.Is_Empty (Comp.Licenses) then
         declare
            Licenses_JSON : Unbounded_String := Null_Unbounded_String;
            L_First       : Boolean := True;
         begin
            for L of Comp.Licenses loop
               if not L_First then
                  Append (Licenses_JSON, "," & NL);
               end if;
               L_First := False;

               if Length (L.Expression) > 0 then
                  Append (Licenses_JSON, Indent & Indent_Unit &
                    "{""expression"": " & JSON_String (L.Expression) & "}");
               else
                  Append (Licenses_JSON, Indent & Indent_Unit &
                    "{""license"": {""id"": " & JSON_String (L.SPDX_ID) & "}}");
               end if;
            end loop;
            Add_Field ("licenses", "[" & NL & To_String (Licenses_JSON) & NL & Indent & "]");
         end;
      end if;

      --  External references
      if not External_Ref_Vectors.Is_Empty (Comp.External_Refs) then
         declare
            Refs_JSON : Unbounded_String := Null_Unbounded_String;
            R_First   : Boolean := True;
         begin
            for R of Comp.External_Refs loop
               if not R_First then
                  Append (Refs_JSON, "," & NL);
               end if;
               R_First := False;
               Append (Refs_JSON, Indent & Indent_Unit &
                 "{""type"": " & JSON_String (External_Ref_Type_Name (R.Ref_Type)) &
                 ", ""url"": " & JSON_String (R.URL) & "}");
            end loop;
            Add_Field ("externalReferences",
              "[" & NL & To_String (Refs_JSON) & NL & Indent & "]");
         end;
      end if;

      return Build_JSON_Object (To_String (Result), Pretty, Indent_Level);
   end Component_To_CycloneDX_JSON;

   function To_CycloneDX_JSON
     (Doc          : SBOM_Document;
      Pretty_Print : Boolean := True) return String
   is
      Result : Unbounded_String := Null_Unbounded_String;
      NL     : constant String := (if Pretty_Print then "" & Newline else "");
      Indent1 : constant String := Make_Indent (1);
      Indent2 : constant String := Make_Indent (2);
      First  : Boolean := True;

      procedure Add_Field (Key : String; Value : String) is
      begin
         if Value'Length > 0 then
            if not First then
               Append (Result, "," & NL);
            end if;
            First := False;
            Append (Result, Indent1 & JSON_String (Key) & ": " & Value);
         end if;
      end Add_Field;

   begin
      --  BOM format identifier
      Add_Field ("bomFormat", JSON_String ("CycloneDX"));
      Add_Field ("specVersion", JSON_String (CycloneDX_Spec_Version));

      --  Serial number (UUID)
      if Length (Doc.Metadata.Serial_Number) > 0 then
         Add_Field ("serialNumber",
           JSON_String ("urn:uuid:" & To_String (Doc.Metadata.Serial_Number)));
      end if;

      Add_Field ("version", Integer'Image (Doc.Metadata.Version));

      --  Metadata section
      declare
         Meta_JSON : Unbounded_String := Null_Unbounded_String;
         M_First   : Boolean := True;

         procedure Add_Meta (Key : String; Value : String) is
         begin
            if Value'Length > 0 then
               if not M_First then
                  Append (Meta_JSON, "," & NL);
               end if;
               M_First := False;
               Append (Meta_JSON, Indent2 & JSON_String (Key) & ": " & Value);
            end if;
         end Add_Meta;

      begin
         --  Timestamp
         Add_Meta ("timestamp", JSON_String (Format_ISO8601 (Doc.Metadata.Timestamp)));

         --  Tools
         declare
            Tool_JSON : constant String :=
              Indent2 & Indent_Unit & "{" & NL &
              Indent2 & Indent_Unit & Indent_Unit &
                """vendor"": " & JSON_String (Doc.Metadata.Tool.Vendor) & "," & NL &
              Indent2 & Indent_Unit & Indent_Unit &
                """name"": " & JSON_String (Doc.Metadata.Tool.Name) & "," & NL &
              Indent2 & Indent_Unit & Indent_Unit &
                """version"": " & JSON_String (Doc.Metadata.Tool.Version) & NL &
              Indent2 & Indent_Unit & "}";
         begin
            Add_Meta ("tools", "[" & NL & Tool_JSON & NL & Indent2 & "]");
         end;

         --  Component (main package)
         Add_Meta ("component",
           Component_To_CycloneDX_JSON (Doc.Metadata.Component, Pretty_Print, 2));

         Add_Field ("metadata", "{" & NL & To_String (Meta_JSON) & NL & Indent1 & "}");
      end;

      --  Components array
      if not Component_Vectors.Is_Empty (Doc.Components) then
         declare
            Comps_JSON : Unbounded_String := Null_Unbounded_String;
            C_First    : Boolean := True;
         begin
            for C of Doc.Components loop
               if not C_First then
                  Append (Comps_JSON, "," & NL);
               end if;
               C_First := False;
               Append (Comps_JSON, Indent2 &
                 Component_To_CycloneDX_JSON (C, Pretty_Print, 2));
            end loop;
            Add_Field ("components", "[" & NL & To_String (Comps_JSON) & NL & Indent1 & "]");
         end;
      end if;

      --  Dependencies
      if not Dependency_Vectors.Is_Empty (Doc.Dependencies) then
         declare
            Deps_JSON : Unbounded_String := Null_Unbounded_String;
            D_First   : Boolean := True;
            --  Group by source ref
            Current_Ref : Unbounded_String := Null_Unbounded_String;
            Dep_List    : Unbounded_String := Null_Unbounded_String;
         begin
            for D of Doc.Dependencies loop
               if D.Ref /= Current_Ref then
                  --  Output previous group if any
                  if Length (Current_Ref) > 0 then
                     if not D_First then
                        Append (Deps_JSON, "," & NL);
                     end if;
                     D_First := False;
                     Append (Deps_JSON, Indent2 &
                       "{""ref"": " & JSON_String (Current_Ref) &
                       ", ""dependsOn"": [" & To_String (Dep_List) & "]}");
                  end if;
                  Current_Ref := D.Ref;
                  Dep_List := To_Unbounded_String (JSON_String (D.Depends_On));
               else
                  Append (Dep_List, ", " & JSON_String (D.Depends_On));
               end if;
            end loop;

            --  Output last group
            if Length (Current_Ref) > 0 then
               if not D_First then
                  Append (Deps_JSON, "," & NL);
               end if;
               Append (Deps_JSON, Indent2 &
                 "{""ref"": " & JSON_String (Current_Ref) &
                 ", ""dependsOn"": [" & To_String (Dep_List) & "]}");
            end if;

            Add_Field ("dependencies", "[" & NL & To_String (Deps_JSON) & NL & Indent1 & "]");
         end;
      end if;

      return "{" & NL & To_String (Result) & NL & "}";
   end To_CycloneDX_JSON;

   ---------------------------------------------------------------------------
   --  SPDX JSON Generation
   ---------------------------------------------------------------------------

   function To_SPDX_JSON
     (Doc          : SBOM_Document;
      Pretty_Print : Boolean := True) return String
   is
      Result : Unbounded_String := Null_Unbounded_String;
      NL     : constant String := (if Pretty_Print then "" & Newline else "");
      Indent1 : constant String := Make_Indent (1);
      Indent2 : constant String := Make_Indent (2);
      First  : Boolean := True;

      procedure Add_Field (Key : String; Value : String) is
      begin
         if Value'Length > 0 then
            if not First then
               Append (Result, "," & NL);
            end if;
            First := False;
            Append (Result, Indent1 & JSON_String (Key) & ": " & Value);
         end if;
      end Add_Field;

      --  SPDX document namespace
      SPDX_Namespace : constant String :=
        "https://spdx.org/spdxdocs/" & To_String (Doc.Metadata.Component.Name) &
        "-" & To_String (Doc.Metadata.Serial_Number);

   begin
      --  Document header
      Add_Field ("spdxVersion", JSON_String (SPDX_Spec_Version));
      Add_Field ("dataLicense", JSON_String ("CC0-1.0"));
      Add_Field ("SPDXID", JSON_String ("SPDXRef-DOCUMENT"));
      Add_Field ("name", JSON_String (Doc.Metadata.Component.Name));
      Add_Field ("documentNamespace", JSON_String (SPDX_Namespace));

      --  Creation info
      declare
         Creator_Tool : constant String :=
           "Tool: " & To_String (Doc.Metadata.Tool.Name) & "-" &
           To_String (Doc.Metadata.Tool.Version);
         Creation_JSON : constant String :=
           "{" & NL &
           Indent2 & """created"": " & JSON_String (Format_ISO8601 (Doc.Metadata.Timestamp)) & "," & NL &
           Indent2 & """creators"": [" & JSON_String (Creator_Tool) & "]" & NL &
           Indent1 & "}";
      begin
         Add_Field ("creationInfo", Creation_JSON);
      end;

      --  Packages array
      declare
         Packages_JSON : Unbounded_String := Null_Unbounded_String;
         P_First       : Boolean := True;

         procedure Add_Package (Comp : SBOM_Component; SPDX_ID : String) is
            Pkg_JSON : Unbounded_String := Null_Unbounded_String;
            PF_First : Boolean := True;

            procedure Add_Pkg_Field (Key : String; Value : String) is
            begin
               if Value'Length > 0 then
                  if not PF_First then
                     Append (Pkg_JSON, "," & NL);
                  end if;
                  PF_First := False;
                  Append (Pkg_JSON, Indent2 & Indent_Unit & JSON_String (Key) & ": " & Value);
               end if;
            end Add_Pkg_Field;

         begin
            if not P_First then
               Append (Packages_JSON, "," & NL);
            end if;
            P_First := False;

            Add_Pkg_Field ("SPDXID", JSON_String (SPDX_ID));
            Add_Pkg_Field ("name", JSON_String (Comp.Name));
            Add_Pkg_Field ("versionInfo", JSON_String (Comp.Version));

            if Length (Comp.Description) > 0 then
               Add_Pkg_Field ("description", JSON_String (Comp.Description));
            end if;

            Add_Pkg_Field ("downloadLocation", JSON_String ("NOASSERTION"));
            Add_Pkg_Field ("filesAnalyzed", "false");

            --  License
            if not License_Vectors.Is_Empty (Comp.Licenses) then
               declare
                  First_Lic : constant License_Info := Comp.Licenses.First_Element;
               begin
                  if Length (First_Lic.Expression) > 0 then
                     Add_Pkg_Field ("licenseDeclared", JSON_String (First_Lic.Expression));
                  elsif Length (First_Lic.SPDX_ID) > 0 then
                     Add_Pkg_Field ("licenseDeclared", JSON_String (First_Lic.SPDX_ID));
                  end if;
               end;
            end if;

            Add_Pkg_Field ("licenseConcluded", JSON_String ("NOASSERTION"));
            Add_Pkg_Field ("copyrightText", JSON_String ("NOASSERTION"));

            --  External refs (PURL as package reference)
            if Length (Comp.PURL.Name) > 0 then
               declare
                  Ext_Ref : constant String :=
                    "[{" & NL &
                    Indent2 & Indent_Unit & Indent_Unit &
                      """referenceCategory"": ""PACKAGE-MANAGER""," & NL &
                    Indent2 & Indent_Unit & Indent_Unit &
                      """referenceType"": ""purl""," & NL &
                    Indent2 & Indent_Unit & Indent_Unit &
                      """referenceLocator"": " & JSON_String (PURL_To_String (Comp.PURL)) & NL &
                    Indent2 & Indent_Unit & "}]";
               begin
                  Add_Pkg_Field ("externalRefs", Ext_Ref);
               end;
            end if;

            --  Checksums
            if not Hash_Vectors.Is_Empty (Comp.Hashes) then
               declare
                  Checksums_JSON : Unbounded_String := Null_Unbounded_String;
                  CS_First       : Boolean := True;
               begin
                  for H of Comp.Hashes loop
                     if not CS_First then
                        Append (Checksums_JSON, "," & NL);
                     end if;
                     CS_First := False;
                     Append (Checksums_JSON, Indent2 & Indent_Unit & Indent_Unit &
                       "{""algorithm"": " & JSON_String (Hash_Algorithm_Name (H.Algorithm)) &
                       ", ""checksumValue"": " & JSON_String (H.Content) & "}");
                  end loop;
                  Add_Pkg_Field ("checksums",
                    "[" & NL & To_String (Checksums_JSON) & NL & Indent2 & Indent_Unit & "]");
               end;
            end if;

            Append (Packages_JSON, Indent2 & "{" & NL & To_String (Pkg_JSON) & NL & Indent2 & "}");
         end Add_Package;

      begin
         --  Main component
         Add_Package (Doc.Metadata.Component, "SPDXRef-Package");

         --  Dependency components
         for I in Doc.Components.First_Index .. Doc.Components.Last_Index loop
            Add_Package (Doc.Components (I),
              "SPDXRef-Package-" & Integer'Image (I));
         end loop;

         Add_Field ("packages", "[" & NL & To_String (Packages_JSON) & NL & Indent1 & "]");
      end;

      --  Relationships
      declare
         Rels_JSON : Unbounded_String := Null_Unbounded_String;
         R_First   : Boolean := True;

         procedure Add_Rel (A_ID : String; Rel_Type : String; B_ID : String) is
         begin
            if not R_First then
               Append (Rels_JSON, "," & NL);
            end if;
            R_First := False;
            Append (Rels_JSON, Indent2 &
              "{""spdxElementId"": " & JSON_String (A_ID) &
              ", ""relationshipType"": " & JSON_String (Rel_Type) &
              ", ""relatedSpdxElement"": " & JSON_String (B_ID) & "}");
         end Add_Rel;

      begin
         --  Document describes main package
         Add_Rel ("SPDXRef-DOCUMENT", "DESCRIBES", "SPDXRef-Package");

         --  Dependencies as DEPENDS_ON relationships
         for D of Doc.Dependencies loop
            Add_Rel
              ("SPDXRef-" & To_String (D.Ref),
               "DEPENDS_ON",
               "SPDXRef-" & To_String (D.Depends_On));
         end loop;

         Add_Field ("relationships", "[" & NL & To_String (Rels_JSON) & NL & Indent1 & "]");
      end;

      return "{" & NL & To_String (Result) & NL & "}";
   end To_SPDX_JSON;

   ---------------------------------------------------------------------------
   --  Generic To_JSON
   ---------------------------------------------------------------------------

   function To_JSON
     (Doc          : SBOM_Document;
      Pretty_Print : Boolean := True) return String
   is
   begin
      case Doc.Format is
         when CycloneDX_1_5 =>
            return To_CycloneDX_JSON (Doc, Pretty_Print);
         when SPDX_2_3 =>
            return To_SPDX_JSON (Doc, Pretty_Print);
      end case;
   end To_JSON;

   ---------------------------------------------------------------------------
   --  SBOM Document Validation
   ---------------------------------------------------------------------------

   function Is_Valid (Doc : SBOM_Document) return Boolean is
   begin
      --  Must have metadata with component
      if Length (Doc.Metadata.Component.Name) = 0 then
         return False;
      end if;

      --  Must have serial number
      if Length (Doc.Metadata.Serial_Number) = 0 then
         return False;
      end if;

      --  Must have tool information
      if Length (Doc.Metadata.Tool.Name) = 0 then
         return False;
      end if;

      return True;
   end Is_Valid;

   ---------------------------------------------------------------------------
   --  Main Generation Function
   ---------------------------------------------------------------------------

   function Generate
     (Manifest : Cerro_Manifest.Manifest;
      Options  : SBOM_Options := Default_Options) return Generation_Result
   is
      Doc : SBOM_Document;
   begin
      --  Validate manifest
      if not Cerro_Manifest.Is_Complete (Manifest) then
         return (Success   => False,
                 Error     => Invalid_Manifest,
                 Error_Msg => To_Unbounded_String ("Manifest is incomplete"));
      end if;

      --  Set format
      Doc.Format := Options.Format;

      --  Generate serial number
      if Length (Options.Serial_Number) > 0 then
         Doc.Metadata.Serial_Number := Options.Serial_Number;
      else
         Doc.Metadata.Serial_Number := To_Unbounded_String (Generate_UUID);
      end if;

      --  Set metadata
      Doc.Metadata.Version := 1;
      Doc.Metadata.Timestamp := Ada.Calendar.Clock;

      --  Set tool info
      Doc.Metadata.Tool := (Vendor  => To_Unbounded_String (CT_Vendor),
                            Name    => To_Unbounded_String (CT_Name),
                            Version => To_Unbounded_String (CT_Version));

      --  Convert main component
      Doc.Metadata.Component := Manifest_To_Component (Manifest);

      --  Add runtime dependencies as components
      for Dep of Manifest.Dependencies.Runtime loop
         Component_Vectors.Append (Doc.Components, Dependency_To_Component (Dep));

         --  Add dependency edge
         Dependency_Vectors.Append (Doc.Dependencies,
           Dependency_Edge'(Ref        => Doc.Metadata.Component.BOM_Ref,
                            Depends_On => Dependency_To_Component (Dep).BOM_Ref,
                            Relation   => Depends_On));
      end loop;

      --  Add build dependencies if requested
      if Options.Include_Build_Deps then
         for Dep of Manifest.Dependencies.Build loop
            declare
               Comp : SBOM_Component := Dependency_To_Component (Dep);
            begin
               Comp.Scope := Required;  --  Build scope
               Component_Vectors.Append (Doc.Components, Comp);

               Dependency_Vectors.Append (Doc.Dependencies,
                 Dependency_Edge'(Ref        => Doc.Metadata.Component.BOM_Ref,
                                  Depends_On => Comp.BOM_Ref,
                                  Relation   => Build_Dependency));
            end;
         end loop;
      end if;

      --  Add test dependencies if requested
      if Options.Include_Test_Deps then
         for Dep of Manifest.Dependencies.Check loop
            declare
               Comp : SBOM_Component := Dependency_To_Component (Dep);
            begin
               Comp.Scope := Optional;  --  Test scope
               Component_Vectors.Append (Doc.Components, Comp);

               Dependency_Vectors.Append (Doc.Dependencies,
                 Dependency_Edge'(Ref        => Doc.Metadata.Component.BOM_Ref,
                                  Depends_On => Comp.BOM_Ref,
                                  Relation   => Test_Dependency));
            end;
         end loop;
      end if;

      --  Add optional dependencies if requested
      if Options.Include_Optional then
         for Dep of Manifest.Dependencies.Optional loop
            declare
               Comp : SBOM_Component := Dependency_To_Component (Dep);
            begin
               Comp.Scope := Optional;
               Component_Vectors.Append (Doc.Components, Comp);

               Dependency_Vectors.Append (Doc.Dependencies,
                 Dependency_Edge'(Ref        => Doc.Metadata.Component.BOM_Ref,
                                  Depends_On => Comp.BOM_Ref,
                                  Relation   => Optional_Dependency));
            end;
         end loop;
      end if;

      --  Generate JSON
      declare
         JSON_Output : constant String := To_JSON (Doc, Options.Pretty_Print);
      begin
         return (Success  => True,
                 Document => Doc,
                 JSON     => To_Unbounded_String (JSON_Output));
      end;

   exception
      when others =>
         return (Success   => False,
                 Error     => Serialization_Error,
                 Error_Msg => To_Unbounded_String ("Failed to generate SBOM"));
   end Generate;

   ---------------------------------------------------------------------------
   --  Generate from File
   ---------------------------------------------------------------------------

   function Generate_From_File
     (Manifest_Path : String;
      Options       : SBOM_Options := Default_Options) return Generation_Result
   is
      Parse_Result : constant Cerro_Manifest.Parse_Result :=
        Cerro_Manifest.Parse_File (Manifest_Path);
   begin
      if not Parse_Result.Success then
         return (Success   => False,
                 Error     => Invalid_Manifest,
                 Error_Msg => To_Unbounded_String
                   ("Failed to parse manifest: " &
                    To_String (Parse_Result.Error_Msg)));
      end if;

      return Generate (Parse_Result.Value, Options);
   end Generate_From_File;

   ---------------------------------------------------------------------------
   --  Generate to File
   ---------------------------------------------------------------------------

   function Generate_To_File
     (Manifest    : Cerro_Manifest.Manifest;
      Output_Path : String;
      Options     : SBOM_Options := Default_Options) return Generation_Error
   is
      use Ada.Text_IO;
      Result : constant Generation_Result := Generate (Manifest, Options);
      File   : File_Type;
   begin
      if not Result.Success then
         return Result.Error;
      end if;

      begin
         Create (File, Out_File, Output_Path);
         Put (File, To_String (Result.JSON));
         Close (File);
         return None;
      exception
         when others =>
            if Is_Open (File) then
               Close (File);
            end if;
            return IO_Error;
      end;
   end Generate_To_File;

end CT_SBOM;
