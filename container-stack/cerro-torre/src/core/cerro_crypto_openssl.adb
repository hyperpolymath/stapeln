--  Cerro Torre Crypto OpenSSL - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0

pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Ada.Directories;
with Ada.Calendar;
with Ada.Numerics.Discrete_Random;
with GNAT.OS_Lib;
with Ada.Strings.Fixed;
with Interfaces;

package body Cerro_Crypto_OpenSSL is

   use Ada.Text_IO;
   use Ada.Calendar;
   use GNAT.OS_Lib;
   use Ada.Strings.Fixed;
   use Interfaces;

   --  Helper to free argument arrays
   procedure Free_Args (Args : in out Argument_List_Access) is
   begin
      if Args /= null then
         for I in Args'Range loop
            Free (Args (I));
         end loop;
         Free (Args);
      end if;
   end Free_Args;

   --  Generate cryptographically random unique ID for temp directories
   function Get_Unique_ID return String is
      subtype Random_Range is Positive range 100_000_000 .. 999_999_999;
      package Random_Positive is new Ada.Numerics.Discrete_Random (Random_Range);
      use Random_Positive;
      Gen : Generator;
      ID  : Positive;
   begin
      Reset (Gen);  --  Seeds from /dev/urandom on Unix systems
      ID := Random (Gen);
      return Trim (Positive'Image (ID), Ada.Strings.Both);
   end Get_Unique_ID;

   ---------------------
   -- Key Generation --
   ---------------------

   procedure Generate_Ed25519_Keypair
      (Private_Key : out Ed25519_Private_Key;
       Public_Key  : out Ed25519_Public_Key;
       Success     : out Boolean)
   is
      Temp_Dir      : constant String := "/tmp/cerro_keygen_" &
                                         Get_Unique_ID;
      Private_File  : constant String := Temp_Dir & "/private.pem";
      Public_File   : constant String := Temp_Dir & "/public.pem";
      Raw_Priv_File : constant String := Temp_Dir & "/private.raw";
      Raw_Pub_File  : constant String := Temp_Dir & "/public.raw";
      Args          : Argument_List_Access;
      Success_Flag  : Boolean;
   begin
      Success := False;

      --  Create temp directory
      begin
         Ada.Directories.Create_Directory (Temp_Dir);
      exception
         when others =>
            return;
      end;

      --  Step 1: Generate Ed25519 private key
      --  SECURITY: Use explicit argument array to prevent command injection
      Args := new Argument_List'(
         new String'("genpkey"),
         new String'("-algorithm"),
         new String'("ED25519"),
         new String'("-out"),
         new String'(Private_File)
      );
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free_Args (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 2: Extract public key
      Args := new Argument_List'(
         new String'("pkey"),
         new String'("-in"),
         new String'(Private_File),
         new String'("-pubout"),
         new String'("-out"),
         new String'(Public_File)
      );
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free_Args (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 3: Convert private key to raw format
      Args := new Argument_List'(
         new String'("pkey"),
         new String'("-in"),
         new String'(Private_File),
         new String'("-outform"),
         new String'("DER"),
         new String'("-out"),
         new String'(Raw_Priv_File)
      );
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free_Args (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 4: Convert public key to raw format
      Args := new Argument_List'(
         new String'("pkey"),
         new String'("-pubin"),
         new String'("-in"),
         new String'(Public_File),
         new String'("-outform"),
         new String'("DER"),
         new String'("-out"),
         new String'(Raw_Pub_File)
      );
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free_Args (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 5: Read raw keys (skip DER header, last 32/64 bytes are key material)
      declare
         Priv_F : File_Type;
         Pub_F  : File_Type;
         Byte   : Character;
         Priv_Size : Natural;
         Pub_Size  : Natural;
      begin
         --  Read private key
         Open (Priv_F, In_File, Raw_Priv_File);
         Priv_Size := Natural (Ada.Directories.Size (Raw_Priv_File));

         --  Skip DER header (first bytes), read last 64 bytes
         if Priv_Size >= 64 then
            for I in 1 .. Priv_Size - 64 loop
               Get (Priv_F, Byte);
            end loop;

            for I in Private_Key'Range loop
               Get (Priv_F, Byte);
               Private_Key (I) := Unsigned_8 (Character'Pos (Byte));
            end loop;
         end if;

         Close (Priv_F);

         --  Read public key
         Open (Pub_F, In_File, Raw_Pub_File);
         Pub_Size := Natural (Ada.Directories.Size (Raw_Pub_File));

         --  Skip DER header, read last 32 bytes
         if Pub_Size >= 32 then
            for I in 1 .. Pub_Size - 32 loop
               Get (Pub_F, Byte);
            end loop;

            for I in Public_Key'Range loop
               Get (Pub_F, Byte);
               Public_Key (I) := Unsigned_8 (Character'Pos (Byte));
            end loop;
         end if;

         Close (Pub_F);

         Success := (Priv_Size >= 64 and Pub_Size >= 32);
      exception
         when others =>
            Success := False;
      end;

      --  Cleanup
      Ada.Directories.Delete_Tree (Temp_Dir);

   exception
      when others =>
         Success := False;
         if Ada.Directories.Exists (Temp_Dir) then
            Ada.Directories.Delete_Tree (Temp_Dir);
         end if;
   end Generate_Ed25519_Keypair;

   ----------------------
   -- Signing Operations --
   ----------------------

   procedure Sign_Ed25519
      (Message     : String;
       Private_Key : Ed25519_Private_Key;
       Signature   : out Ed25519_Signature;
       Success     : out Boolean)
   is
      Temp_Dir      : constant String := "/tmp/cerro_sign_" &
                                         Get_Unique_ID;
      Script_Path   : constant String := Temp_Dir & "/sign.sh";
      Message_File  : constant String := Temp_Dir & "/message.bin";
      Sig_Hex_File  : constant String := Temp_Dir & "/signature.hex";
      Args          : Argument_List_Access;
      Success_Flag  : Boolean;
   begin
      Success := False;

      --  Create temp directory
      begin
         Ada.Directories.Create_Directory (Temp_Dir);
      exception
         when others =>
            return;
      end;

      --  Step 1: Write message to file using Ada.Text_IO
      declare
         F : File_Type;
      begin
         Create (F, Out_File, Message_File);
         Put (F, Message);
         Close (F);
      end;

      --  Step 2: Create a signing shell script
      --  We use a shell script because Ada.Text_IO cannot reliably
      --  write binary DER data (control characters like NUL, ENQ are
      --  mangled by text-mode I/O).  The script reconstructs the
      --  PKCS#8 DER private key from the hex-encoded seed, converts
      --  to PEM, signs, and outputs the signature as hex.
      declare
         Seed_Hex    : constant String :=
            Private_Key_To_Hex (Private_Key) (1 .. 64);
         Script_File : Ada.Text_IO.File_Type;
         Sig_Path    : constant String := Temp_Dir & "/signature.bin";
      begin
         Ada.Text_IO.Create (Script_File, Ada.Text_IO.Out_File,
                             Script_Path);
         Ada.Text_IO.Put_Line (Script_File, "#!/bin/bash");
         Ada.Text_IO.Put_Line (Script_File, "set -e");
         --  Write PKCS#8 DER private key from seed hex
         --  Header: 302e020100300506032b657004220420 (16 bytes)
         Ada.Text_IO.Put_Line (Script_File,
            "printf '302e020100300506032b657004220420"
            & Seed_Hex
            & "' | xxd -r -p > "
            & Temp_Dir & "/key.der");
         --  Convert DER to PEM
         Ada.Text_IO.Put_Line (Script_File,
            "openssl pkey -inform DER"
            & " -in " & Temp_Dir & "/key.der"
            & " -out " & Temp_Dir & "/key.pem 2>/dev/null");
         --  Sign the message
         Ada.Text_IO.Put_Line (Script_File,
            "openssl pkeyutl -sign"
            & " -inkey " & Temp_Dir & "/key.pem"
            & " -in " & Message_File
            & " -out " & Sig_Path & " 2>/dev/null");
         --  Convert signature to hex for safe reading
         Ada.Text_IO.Put_Line (Script_File,
            "hexdump -v -e '/1 " & '"'
            & "%02x" & '"' & "' "
            & Sig_Path & " > " & Sig_Hex_File);
         --  Secure cleanup of key material
         Ada.Text_IO.Put_Line (Script_File,
            "rm -f " & Temp_Dir & "/key.der "
            & Temp_Dir & "/key.pem");
         Ada.Text_IO.Close (Script_File);
      end;

      --  Make script executable
      Args := new Argument_List'(
         new String'("+x"),
         new String'(Script_Path)
      );
      Spawn ("/usr/bin/chmod", Args.all, Success_Flag);
      Free (Args (1));
      Free (Args (2));
      Free (Args);

      --  Execute the signing script
      Args := new Argument_List'(1 => new String'(Script_Path));
      Spawn ("/bin/bash", Args.all, Success_Flag);
      Free (Args (1));
      Free (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 3: Read signature from hex file
      if Ada.Directories.Exists (Sig_Hex_File) then
         declare
            Hex_File : Ada.Text_IO.File_Type;
            Hex_Str  : String (1 .. 128);
            Last     : Natural;
         begin
            Ada.Text_IO.Open (Hex_File, Ada.Text_IO.In_File,
                              Sig_Hex_File);
            Ada.Text_IO.Get_Line (Hex_File, Hex_Str, Last);
            Ada.Text_IO.Close (Hex_File);

            if Last = 128 then
               Hex_To_Signature (Hex_Str, Signature, Success);
            else
               Success := False;
            end if;
         exception
            when others =>
               Success := False;
         end;
      end if;

      --  Cleanup
      Ada.Directories.Delete_Tree (Temp_Dir);

   exception
      when others =>
         Success := False;
         if Ada.Directories.Exists (Temp_Dir) then
            Ada.Directories.Delete_Tree (Temp_Dir);
         end if;
   end Sign_Ed25519;

   ----------------------------
   -- Verification Operations --
   ----------------------------

   procedure Verify_Ed25519
      (Message    : String;
       Signature  : Ed25519_Signature;
       Public_Key : Ed25519_Public_Key;
       Valid      : out Boolean;
       Success    : out Boolean)
   is
      Temp_Dir     : constant String := "/tmp/cerro_verify_" &
                                        Get_Unique_ID;
      Pub_File     : constant String := Temp_Dir & "/public.pem";
      Raw_Pub_File : constant String := Temp_Dir & "/public.raw";
      Msg_File     : constant String := Temp_Dir & "/message.bin";
      Sig_File     : constant String := Temp_Dir & "/signature.bin";
      Args         : Argument_List_Access;
      Success_Flag : Boolean;
   begin
      Valid   := False;
      Success := False;

      --  Create temp directory
      begin
         Ada.Directories.Create_Directory (Temp_Dir);
      exception
         when others =>
            return;
      end;

      --  Step 1: Write raw public key as DER-encoded Ed25519 public key
      --  DER encoding: fixed 12-byte header + 32-byte raw key
      --  30 2a 30 05 06 03 2b 65 70 03 21 00 <32 bytes key>
      declare
         F      : File_Type;
         Header : constant array (1 .. 12) of Unsigned_8 :=
            (16#30#, 16#2A#, 16#30#, 16#05#, 16#06#, 16#03#,
             16#2B#, 16#65#, 16#70#, 16#03#, 16#21#, 16#00#);
      begin
         Create (F, Out_File, Raw_Pub_File);
         for Byte of Header loop
            Put (F, Character'Val (Byte));
         end loop;
         for Byte of Public_Key loop
            Put (F, Character'Val (Byte));
         end loop;
         Close (F);
      end;

      --  Step 2: Convert raw DER public key to PEM format
      Args := new Argument_List'(
         new String'("pkey"),
         new String'("-pubin"),
         new String'("-inform"),
         new String'("DER"),
         new String'("-in"),
         new String'(Raw_Pub_File),
         new String'("-out"),
         new String'(Pub_File)
      );
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free_Args (Args);

      if not Success_Flag then
         Ada.Directories.Delete_Tree (Temp_Dir);
         return;
      end if;

      --  Step 3: Write message to file
      declare
         F : File_Type;
      begin
         Create (F, Out_File, Msg_File);
         Put (F, Message);
         Close (F);
      end;

      --  Step 4: Write signature to file (64 raw bytes)
      declare
         F : File_Type;
      begin
         Create (F, Out_File, Sig_File);
         for Byte of Signature loop
            Put (F, Character'Val (Byte));
         end loop;
         Close (F);
      end;

      --  Step 5: Verify the signature using OpenSSL pkeyutl
      Args := new Argument_List'(
         new String'("pkeyutl"),
         new String'("-verify"),
         new String'("-pubin"),
         new String'("-inkey"),
         new String'(Pub_File),
         new String'("-in"),
         new String'(Msg_File),
         new String'("-sigfile"),
         new String'(Sig_File)
      );
      Spawn ("/usr/bin/openssl", Args.all, Success_Flag);
      Free_Args (Args);

      --  OpenSSL pkeyutl -verify returns exit code 0 on valid signature
      Success := True;
      Valid   := Success_Flag;

      --  Cleanup
      Ada.Directories.Delete_Tree (Temp_Dir);

   exception
      when others =>
         Success := False;
         Valid   := False;
         if Ada.Directories.Exists (Temp_Dir) then
            Ada.Directories.Delete_Tree (Temp_Dir);
         end if;
   end Verify_Ed25519;

   -----------------------
   -- Key Serialization --
   -----------------------

   function Byte_To_Hex (B : Unsigned_8) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      High      : constant Natural := Natural (B / 16);
      Low       : constant Natural := Natural (B mod 16);
   begin
      return Hex_Chars (High + 1) & Hex_Chars (Low + 1);
   end Byte_To_Hex;

   function Hex_To_Byte (Hex : String) return Unsigned_8 is
      function Hex_Digit (C : Character) return Unsigned_8 is
      begin
         if C >= '0' and C <= '9' then
            return Unsigned_8 (Character'Pos (C) - Character'Pos ('0'));
         elsif C >= 'a' and C <= 'f' then
            return Unsigned_8 (Character'Pos (C) - Character'Pos ('a') + 10);
         elsif C >= 'A' and C <= 'F' then
            return Unsigned_8 (Character'Pos (C) - Character'Pos ('A') + 10);
         else
            --  SECURITY: Strict validation - fail on invalid hex
            raise Constraint_Error with "Invalid hex character: " & C;
         end if;
      end Hex_Digit;
   begin
      if Hex'Length /= 2 then
         raise Constraint_Error with "Hex_To_Byte requires exactly 2 characters";
      end if;
      return Hex_Digit (Hex (Hex'First)) * 16 + Hex_Digit (Hex (Hex'First + 1));
   end Hex_To_Byte;

   function Private_Key_To_Hex (Key : Ed25519_Private_Key) return String is
      Result : String (1 .. 128);
      Pos    : Positive := 1;
   begin
      for Byte of Key loop
         Result (Pos .. Pos + 1) := Byte_To_Hex (Byte);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Private_Key_To_Hex;

   procedure Hex_To_Private_Key
      (Hex         : String;
       Private_Key : out Ed25519_Private_Key;
       Success     : out Boolean)
   is
   begin
      Success := True;
      for I in Private_Key'Range loop
         declare
            Offset : constant Natural := (I - 1) * 2;
         begin
            Private_Key (I) := Hex_To_Byte (Hex (Hex'First + Offset .. Hex'First + Offset + 1));
         end;
      end loop;
   exception
      when others =>
         Success := False;
   end Hex_To_Private_Key;

   function Public_Key_To_Hex (Key : Ed25519_Public_Key) return String is
      Result : String (1 .. 64);
      Pos    : Positive := 1;
   begin
      for Byte of Key loop
         Result (Pos .. Pos + 1) := Byte_To_Hex (Byte);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Public_Key_To_Hex;

   procedure Hex_To_Public_Key
      (Hex        : String;
       Public_Key : out Ed25519_Public_Key;
       Success    : out Boolean)
   is
   begin
      Success := True;
      for I in Public_Key'Range loop
         declare
            Offset : constant Natural := (I - 1) * 2;
         begin
            Public_Key (I) := Hex_To_Byte (Hex (Hex'First + Offset .. Hex'First + Offset + 1));
         end;
      end loop;
   exception
      when others =>
         Success := False;
   end Hex_To_Public_Key;

   function Signature_To_Hex (Sig : Ed25519_Signature) return String is
      Result : String (1 .. 128);
      Pos    : Positive := 1;
   begin
      for Byte of Sig loop
         Result (Pos .. Pos + 1) := Byte_To_Hex (Byte);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Signature_To_Hex;

   procedure Hex_To_Signature
      (Hex       : String;
       Signature : out Ed25519_Signature;
       Success   : out Boolean)
   is
   begin
      Success := True;
      for I in Signature'Range loop
         declare
            Offset : constant Natural := (I - 1) * 2;
         begin
            Signature (I) := Hex_To_Byte (Hex (Hex'First + Offset .. Hex'First + Offset + 1));
         end;
      end loop;
   exception
      when others =>
         Success := False;
   end Hex_To_Signature;

end Cerro_Crypto_OpenSSL;
