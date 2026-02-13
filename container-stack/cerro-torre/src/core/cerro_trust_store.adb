--  Cerro_Trust_Store - Key and trust management implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0

with Ada.Text_IO;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Interfaces;
with Cerro_Crypto;

package body Cerro_Trust_Store is

   use Ada.Text_IO;
   use Ada.Directories;
   use Ada.Strings.Fixed;
   use Ada.Characters.Handling;

   Store_Initialized : Boolean := False;
   Store_Path_Cache  : String (1 .. 256);
   Store_Path_Len    : Natural := 0;

   ---------------------------------------------------------------------------
   --  Internal helpers
   ---------------------------------------------------------------------------

   function Trim (S : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
   end Trim;

   function Get_Home_Dir return String is
   begin
      if Ada.Environment_Variables.Exists ("HOME") then
         return Ada.Environment_Variables.Value ("HOME");
      elsif Ada.Environment_Variables.Exists ("USERPROFILE") then
         return Ada.Environment_Variables.Value ("USERPROFILE");
      else
         return "/tmp";
      end if;
   end Get_Home_Dir;

   function Build_Store_Path return String is
      Home : constant String := Get_Home_Dir;
   begin
      --  Use XDG_CONFIG_HOME if set, otherwise ~/.config
      if Ada.Environment_Variables.Exists ("XDG_CONFIG_HOME") then
         return Ada.Environment_Variables.Value ("XDG_CONFIG_HOME") &
                "/cerro-torre/trust";
      else
         return Home & "/.config/cerro-torre/trust";
      end if;
   end Build_Store_Path;

   function Key_File_Path (Key_Id : String) return String is
   begin
      return Get_Store_Path & "/" & Key_Id & ".pub";
   end Key_File_Path;

   function Meta_File_Path (Key_Id : String) return String is
   begin
      return Get_Store_Path & "/" & Key_Id & ".meta";
   end Meta_File_Path;

   function Default_Key_Path return String is
   begin
      return Get_Store_Path & "/default";
   end Default_Key_Path;

   function Is_Valid_Hex (S : String) return Boolean is
   begin
      if S'Length = 0 or S'Length mod 2 /= 0 then
         return False;
      end if;
      for C of S loop
         if not (C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F') then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid_Hex;

   function Is_Valid_Key_Id (S : String) return Boolean is
   begin
      if S'Length = 0 or S'Length > 64 then
         return False;
      end if;
      for C of S loop
         if not (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.') then
            return False;
         end if;
      end loop;
      return True;
   end Is_Valid_Key_Id;

   procedure Write_Text_File (Path : String; Content : String) is
      F : File_Type;
   begin
      Create (F, Out_File, Path);
      Put (F, Content);
      Close (F);
   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
   end Write_Text_File;

   function Read_Text_File (Path : String) return String is
      F      : File_Type;
      Buffer : String (1 .. 4096);
      Last   : Natural;
   begin
      if not Exists (Path) then
         return "";
      end if;
      Open (F, In_File, Path);
      Get_Line (F, Buffer, Last);
      Close (F);
      return Buffer (1 .. Last);
   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         return "";
   end Read_Text_File;

   function Trust_To_String (T : Trust_Level) return String is
   begin
      case T is
         when Untrusted => return "untrusted";
         when Marginal  => return "marginal";
         when Full      => return "full";
         when Ultimate  => return "ultimate";
      end case;
   end Trust_To_String;

   function String_To_Trust (S : String) return Trust_Level is
      Lower : constant String := To_Lower (S);
   begin
      if Lower = "untrusted" then
         return Untrusted;
      elsif Lower = "marginal" then
         return Marginal;
      elsif Lower = "full" then
         return Full;
      elsif Lower = "ultimate" then
         return Ultimate;
      else
         return Untrusted;
      end if;
   end String_To_Trust;

   procedure Write_Meta_File (Key_Id : String; Info : Key_Info) is
      Path    : constant String := Meta_File_Path (Key_Id);
      Content : constant String :=
         "key_id = " & Info.Key_Id (1 .. Info.Key_Id_Len) & ASCII.LF &
         "fingerprint = " & Info.Fingerprint (1 .. Info.Finger_Len) & ASCII.LF &
         "trust = " & Trust_To_String (Info.Trust) & ASCII.LF &
         "created = " & Info.Created (1 .. Info.Created_Len) & ASCII.LF;
   begin
      Write_Text_File (Path, Content);
   end Write_Meta_File;

   function Read_Meta_File (Key_Id : String; Info : out Key_Info) return Boolean is
      Path : constant String := Meta_File_Path (Key_Id);
      F    : File_Type;
      Line : String (1 .. 256);
      Last : Natural;
   begin
      if not Exists (Path) then
         return False;
      end if;

      Info := (others => <>);

      Open (F, In_File, Path);
      while not End_Of_File (F) loop
         Get_Line (F, Line, Last);
         declare
            L     : constant String := Trim (Line (1 .. Last));
            Eq    : constant Natural := Index (L, "=");
            Key   : String (1 .. 64);
            Value : String (1 .. 128);
         begin
            if Eq > 0 then
               declare
                  K : constant String := Trim (L (L'First .. Eq - 1));
                  V : constant String := Trim (L (Eq + 1 .. L'Last));
               begin
                  if K = "key_id" then
                     Info.Key_Id_Len := Natural'Min (V'Length, Info.Key_Id'Length);
                     Info.Key_Id (1 .. Info.Key_Id_Len) := V (V'First .. V'First + Info.Key_Id_Len - 1);
                  elsif K = "fingerprint" then
                     Info.Finger_Len := Natural'Min (V'Length, Info.Fingerprint'Length);
                     Info.Fingerprint (1 .. Info.Finger_Len) := V (V'First .. V'First + Info.Finger_Len - 1);
                  elsif K = "trust" then
                     Info.Trust := String_To_Trust (V);
                  elsif K = "created" then
                     Info.Created_Len := Natural'Min (V'Length, Info.Created'Length);
                     Info.Created (1 .. Info.Created_Len) := V (V'First .. V'First + Info.Created_Len - 1);
                  end if;
               end;
            end if;
         end;
      end loop;
      Close (F);
      return True;
   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         return False;
   end Read_Meta_File;

   ---------------------------------------------------------------------------
   --  Public interface
   ---------------------------------------------------------------------------

   procedure Initialize is
      Path : constant String := Build_Store_Path;
   begin
      --  Cache the path
      Store_Path_Len := Natural'Min (Path'Length, Store_Path_Cache'Length);
      Store_Path_Cache (1 .. Store_Path_Len) := Path (Path'First .. Path'First + Store_Path_Len - 1);

      --  Create directory structure
      if not Exists (Path) then
         Create_Path (Path);
      end if;

      Store_Initialized := True;
   exception
      when others =>
         Store_Initialized := False;
   end Initialize;

   function Is_Initialized return Boolean is
   begin
      return Store_Initialized;
   end Is_Initialized;

   function Get_Store_Path return String is
   begin
      if not Store_Initialized then
         Initialize;
      end if;
      return Store_Path_Cache (1 .. Store_Path_Len);
   end Get_Store_Path;

   function Compute_Fingerprint (Public_Key_Hex : String) return String is
      use Interfaces;
      --  SHA-256 of the public key hex string (simple approach)
      Hash : Cerro_Crypto.SHA256_Digest;
   begin
      if Public_Key_Hex'Length /= 64 then
         return (1 .. 64 => '0');
      end if;

      --  Hash the hex string directly (fingerprint of key representation)
      Hash := Cerro_Crypto.Compute_SHA256 (Public_Key_Hex);

      --  Convert hash to hex string
      declare
         Result : String (1 .. 64);
         Hex    : constant String := "0123456789abcdef";
         Val    : Unsigned_8;
      begin
         for I in Hash'Range loop
            Val := Hash (I);
            Result ((I - Hash'First) * 2 + 1) := Hex (Natural (Val / 16) + 1);
            Result ((I - Hash'First) * 2 + 2) := Hex (Natural (Val mod 16) + 1);
         end loop;
         return Result;
      end;
   end Compute_Fingerprint;

   function Import_Key_Hex (Hex_Key : String; Key_Id : String) return Store_Result is
      Clean_Hex : constant String := To_Lower (Trim (Hex_Key));
      Info      : Key_Info;
      Pub_Path  : constant String := Key_File_Path (Key_Id);
   begin
      if not Is_Initialized then
         Initialize;
      end if;

      --  Validate key ID
      if not Is_Valid_Key_Id (Key_Id) then
         return Invalid_Format;
      end if;

      --  Validate hex key (Ed25519 public key = 32 bytes = 64 hex chars)
      if Clean_Hex'Length /= 64 or not Is_Valid_Hex (Clean_Hex) then
         return Invalid_Key;
      end if;

      --  Check if already exists
      if Exists (Pub_Path) then
         return Already_Exists;
      end if;

      --  Build key info
      Info.Key_Id_Len := Key_Id'Length;
      Info.Key_Id (1 .. Info.Key_Id_Len) := Key_Id;
      Info.Pubkey_Len := 64;
      Info.Public_Key (1 .. 64) := Clean_Hex;
      Info.Finger_Len := 64;
      Info.Fingerprint := Compute_Fingerprint (Clean_Hex);
      Info.Trust := Untrusted;
      Info.Created_Len := 10;
      Info.Created (1 .. 10) := "2026-01-18";  --  TODO: get actual date

      --  Write files
      Write_Text_File (Pub_Path, Clean_Hex);
      Write_Meta_File (Key_Id, Info);

      return OK;
   exception
      when others =>
         return IO_Error;
   end Import_Key_Hex;

   function Import_Key (Path : String; Key_Id : String := "") return Store_Result is
      Content  : constant String := Read_Text_File (Path);
      Clean    : constant String := Trim (Content);
      Actual_Id : String (1 .. 64);
      Id_Len    : Natural;
   begin
      if Clean'Length = 0 then
         return Not_Found;
      end if;

      --  Determine key ID
      if Key_Id'Length > 0 then
         Id_Len := Key_Id'Length;
         Actual_Id (1 .. Id_Len) := Key_Id;
      else
         --  Extract from filename (strip .pub extension)
         declare
            Base : constant String := Simple_Name (Path);
            Dot  : constant Natural := Index (Base, ".", Ada.Strings.Backward);
         begin
            if Dot > 0 then
               Id_Len := Dot - 1;
               Actual_Id (1 .. Id_Len) := Base (Base'First .. Dot - 1);
            else
               Id_Len := Base'Length;
               Actual_Id (1 .. Id_Len) := Base;
            end if;
         end;
      end if;

      return Import_Key_Hex (Clean, Actual_Id (1 .. Id_Len));
   end Import_Key;

   function Export_Key (Key_Id : String; Output_Path : String) return Store_Result is
      Pub_Path : constant String := Key_File_Path (Key_Id);
      Content  : constant String := Read_Text_File (Pub_Path);
   begin
      if Content'Length = 0 then
         return Not_Found;
      end if;

      Write_Text_File (Output_Path, Content & ASCII.LF);
      return OK;
   exception
      when others =>
         return IO_Error;
   end Export_Key;

   function Delete_Key (Key_Id : String) return Store_Result is
      Pub_Path  : constant String := Key_File_Path (Key_Id);
      Meta_Path : constant String := Meta_File_Path (Key_Id);
   begin
      if not Exists (Pub_Path) then
         return Not_Found;
      end if;

      Delete_File (Pub_Path);
      if Exists (Meta_Path) then
         Delete_File (Meta_Path);
      end if;

      --  Clear default if this was the default key
      declare
         Default : constant String := Get_Default_Key;
      begin
         if Default = Key_Id then
            Delete_File (Default_Key_Path);
         end if;
      end;

      return OK;
   exception
      when others =>
         return IO_Error;
   end Delete_Key;

   function Set_Trust (Key_Id : String; Level : Trust_Level) return Store_Result is
      Info : Key_Info;
   begin
      if not Read_Meta_File (Key_Id, Info) then
         return Not_Found;
      end if;

      Info.Trust := Level;
      Write_Meta_File (Key_Id, Info);
      return OK;
   exception
      when others =>
         return IO_Error;
   end Set_Trust;

   function Key_Count return Natural is
      Count  : Natural := 0;
      Search : Search_Type;
      Entry_Info : Directory_Entry_Type;
   begin
      if not Is_Initialized then
         Initialize;
      end if;

      Start_Search (Search, Get_Store_Path, "*.pub", (others => True));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Entry_Info);
         Count := Count + 1;
      end loop;
      End_Search (Search);
      return Count;
   exception
      when others =>
         return 0;
   end Key_Count;

   function Get_Key (Key_Id : String; Info : out Key_Info) return Store_Result is
      Pub_Path : constant String := Key_File_Path (Key_Id);
      Content  : constant String := Read_Text_File (Pub_Path);
   begin
      if Content'Length = 0 then
         return Not_Found;
      end if;

      if not Read_Meta_File (Key_Id, Info) then
         --  Create minimal info from pub file
         Info := (others => <>);
         Info.Key_Id_Len := Key_Id'Length;
         Info.Key_Id (1 .. Info.Key_Id_Len) := Key_Id;
         Info.Pubkey_Len := Natural'Min (Content'Length, 64);
         Info.Public_Key (1 .. Info.Pubkey_Len) := Content (Content'First .. Content'First + Info.Pubkey_Len - 1);
         Info.Fingerprint := Compute_Fingerprint (Info.Public_Key (1 .. Info.Pubkey_Len));
         Info.Finger_Len := 64;
         Info.Trust := Untrusted;
      else
         --  Read public key
         Info.Pubkey_Len := Natural'Min (Content'Length, 64);
         Info.Public_Key (1 .. Info.Pubkey_Len) := Content (Content'First .. Content'First + Info.Pubkey_Len - 1);
      end if;

      return OK;
   end Get_Key;

   function Get_Key_By_Fingerprint (Fingerprint : String; Info : out Key_Info) return Store_Result is
      Search     : Search_Type;
      Entry_Info : Directory_Entry_Type;
      Target_FP  : constant String := To_Lower (Fingerprint);
   begin
      if not Is_Initialized then
         Initialize;
      end if;

      Start_Search (Search, Get_Store_Path, "*.pub", (others => True));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Entry_Info);
         declare
            Name    : constant String := Simple_Name (Entry_Info);
            Key_Id  : constant String := Name (Name'First .. Name'Last - 4);  -- strip .pub
            Temp    : Key_Info;
            Result  : Store_Result;
         begin
            Result := Get_Key (Key_Id, Temp);
            if Result = OK then
               if To_Lower (Temp.Fingerprint (1 .. Temp.Finger_Len)) = Target_FP then
                  Info := Temp;
                  End_Search (Search);
                  return OK;
               end if;
            end if;
         end;
      end loop;
      End_Search (Search);
      return Not_Found;
   exception
      when others =>
         return IO_Error;
   end Get_Key_By_Fingerprint;

   function Is_Trusted (Fingerprint : String) return Boolean is
      Info : Key_Info;
   begin
      if Get_Key_By_Fingerprint (Fingerprint, Info) = OK then
         return Info.Trust in Marginal | Full | Ultimate;
      end if;
      return False;
   end Is_Trusted;

   function Get_Trust_Level (Fingerprint : String) return Trust_Level is
      Info : Key_Info;
   begin
      if Get_Key_By_Fingerprint (Fingerprint, Info) = OK then
         return Info.Trust;
      end if;
      return Untrusted;
   end Get_Trust_Level;

   procedure For_Each_Key (Callback : Key_Callback) is
      Search     : Search_Type;
      Entry_Info : Directory_Entry_Type;
   begin
      if not Is_Initialized then
         Initialize;
      end if;

      Start_Search (Search, Get_Store_Path, "*.pub", (others => True));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Entry_Info);
         declare
            Name   : constant String := Simple_Name (Entry_Info);
            Key_Id : constant String := Name (Name'First .. Name'Last - 4);
            Info   : Key_Info;
            Result : Store_Result;
         begin
            Result := Get_Key (Key_Id, Info);
            if Result = OK then
               Callback (Info);
            end if;
         end;
      end loop;
      End_Search (Search);
   exception
      when others =>
         null;
   end For_Each_Key;

   function Get_Default_Key return String is
      Content : constant String := Read_Text_File (Default_Key_Path);
   begin
      return Trim (Content);
   end Get_Default_Key;

   function Set_Default_Key (Key_Id : String) return Store_Result is
      Pub_Path : constant String := Key_File_Path (Key_Id);
   begin
      if not Exists (Pub_Path) then
         return Not_Found;
      end if;

      Write_Text_File (Default_Key_Path, Key_Id);
      return OK;
   exception
      when others =>
         return IO_Error;
   end Set_Default_Key;

end Cerro_Trust_Store;
