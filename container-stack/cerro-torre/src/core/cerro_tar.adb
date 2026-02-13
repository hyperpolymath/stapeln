--  Cerro_Tar - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later

with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Calendar;
with Ada.Calendar.Conversions;
with Interfaces.C;

package body Cerro_Tar is

   package SIO renames Ada.Streams.Stream_IO;

   --  Convert integer to octal string with specified width
   function To_Octal (Value : Natural; Width : Positive) return String is
      Result : String (1 .. Width) := (others => '0');
      V      : Natural := Value;
      I      : Natural := Width;
   begin
      --  Leave last character as NUL or space
      I := Width - 1;
      while V > 0 and I >= 1 loop
         Result (I) := Character'Val (Character'Pos ('0') + (V mod 8));
         V := V / 8;
         I := I - 1;
      end loop;
      --  Pad with leading zeros
      for J in 1 .. I loop
         Result (J) := '0';
      end loop;
      --  Terminate with NUL
      Result (Width) := ASCII.NUL;
      return Result;
   end To_Octal;

   --  Pad string to fixed length with NULs
   function Pad_String (S : String; Len : Positive) return String is
      Result : String (1 .. Len) := (others => ASCII.NUL);
   begin
      if S'Length >= Len then
         Result := S (S'First .. S'First + Len - 1);
      else
         Result (1 .. S'Length) := S;
      end if;
      return Result;
   end Pad_String;

   --  Calculate header checksum
   function Compute_Checksum (Header : Tar_Header) return Natural is
      Sum : Natural := 0;

      procedure Add_Field (S : String) is
      begin
         for C of S loop
            Sum := Sum + Character'Pos (C);
         end loop;
      end Add_Field;
   begin
      Add_Field (Header.Name);
      Add_Field (Header.Mode);
      Add_Field (Header.UID);
      Add_Field (Header.GID);
      Add_Field (Header.Size);
      Add_Field (Header.Mtime);
      --  Checksum field treated as spaces during calculation
      Sum := Sum + 8 * Character'Pos (' ');
      Sum := Sum + Character'Pos (Header.Typeflag);
      Add_Field (Header.Linkname);
      Add_Field (Header.Magic);
      Add_Field (Header.Version);
      Add_Field (Header.Uname);
      Add_Field (Header.Gname);
      Add_Field (Header.Devmajor);
      Add_Field (Header.Devminor);
      Add_Field (Header.Prefix);
      Add_Field (Header.Padding);
      return Sum;
   end Compute_Checksum;

   --  Create initial header with defaults
   function Make_Header
      (Name     : String;
       Size     : Natural;
       Typeflag : Character) return Tar_Header
   is
      Header : Tar_Header;
      Now    : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Unix_Time : Interfaces.C.long;
   begin
      --  Get Unix timestamp
      Unix_Time := Ada.Calendar.Conversions.To_Unix_Time (Now);

      Header.Name     := Pad_String (Name, 100);
      Header.Mode     := To_Octal (8#644#, 8);  --  rw-r--r--
      Header.UID      := To_Octal (0, 8);
      Header.GID      := To_Octal (0, 8);
      Header.Size     := To_Octal (Size, 12);
      Header.Mtime    := To_Octal (Natural (Unix_Time), 12);
      Header.Checksum := "        ";  --  8 spaces initially
      Header.Typeflag := Typeflag;
      Header.Linkname := (others => ASCII.NUL);
      Header.Magic    := "ustar" & ASCII.NUL;
      Header.Version  := "00";
      Header.Uname    := Pad_String ("root", 32);
      Header.Gname    := Pad_String ("root", 32);
      Header.Devmajor := To_Octal (0, 8);
      Header.Devminor := To_Octal (0, 8);
      Header.Prefix   := (others => ASCII.NUL);
      Header.Padding  := (others => ASCII.NUL);

      --  Compute and set checksum
      declare
         Checksum : constant Natural := Compute_Checksum (Header);
         Chk_Str  : constant String := To_Octal (Checksum, 7);
      begin
         Header.Checksum := Chk_Str & " ";
      end;

      return Header;
   end Make_Header;

   --  Write header as bytes
   procedure Write_Header (File : SIO.File_Type; Header : Tar_Header) is
      use Ada.Streams;

      Buffer : Stream_Element_Array (1 .. Block_Size);
      Idx    : Stream_Element_Offset := 1;

      procedure Write_Field (S : String) is
      begin
         for C of S loop
            Buffer (Idx) := Stream_Element (Character'Pos (C));
            Idx := Idx + 1;
         end loop;
      end Write_Field;
   begin
      Write_Field (Header.Name);
      Write_Field (Header.Mode);
      Write_Field (Header.UID);
      Write_Field (Header.GID);
      Write_Field (Header.Size);
      Write_Field (Header.Mtime);
      Write_Field (Header.Checksum);
      Buffer (Idx) := Stream_Element (Character'Pos (Header.Typeflag));
      Idx := Idx + 1;
      Write_Field (Header.Linkname);
      Write_Field (Header.Magic);
      Write_Field (Header.Version);
      Write_Field (Header.Uname);
      Write_Field (Header.Gname);
      Write_Field (Header.Devmajor);
      Write_Field (Header.Devminor);
      Write_Field (Header.Prefix);
      Write_Field (Header.Padding);

      SIO.Write (File, Buffer);
   end Write_Header;

   --  Write content with padding to block boundary
   procedure Write_Content (File : SIO.File_Type; Content : String) is
      use Ada.Streams;

      --  Calculate padding needed
      Remainder : constant Natural := Content'Length mod Block_Size;
      Pad_Bytes : constant Natural :=
         (if Remainder = 0 then 0 else Block_Size - Remainder);

      Buffer  : Stream_Element_Array (1 .. Stream_Element_Offset (Content'Length));
      Padding : Stream_Element_Array (1 .. Stream_Element_Offset (Pad_Bytes)) :=
         (others => 0);
   begin
      --  Convert content to stream elements
      for I in Content'Range loop
         Buffer (Stream_Element_Offset (I - Content'First + 1)) :=
            Stream_Element (Character'Pos (Content (I)));
      end loop;

      --  Write content
      if Buffer'Length > 0 then
         SIO.Write (File, Buffer);
      end if;

      --  Write padding
      if Padding'Length > 0 then
         SIO.Write (File, Padding);
      end if;
   end Write_Content;

   ---------------------------------------------------------------------------
   --  Public Interface
   ---------------------------------------------------------------------------

   procedure Create (Writer : out Tar_Writer; Path : String) is
   begin
      Writer.Path := To_Unbounded_String (Path);
      Writer.Is_Open := True;

      --  Create empty file (will append entries)
      declare
         File : SIO.File_Type;
      begin
         SIO.Create (File, SIO.Out_File, Path);
         SIO.Close (File);
      end;
   end Create;

   procedure Add_File
      (Writer  : in out Tar_Writer;
       Name    : String;
       Content : String)
   is
      File   : SIO.File_Type;
      Header : Tar_Header;
   begin
      if not Writer.Is_Open then
         return;
      end if;

      --  Open file for appending
      SIO.Open (File, SIO.Append_File, To_String (Writer.Path));

      --  Create and write header
      Header := Make_Header (Name, Content'Length, Type_Regular);
      Write_Header (File, Header);

      --  Write content with padding
      Write_Content (File, Content);

      SIO.Close (File);
   end Add_File;

   procedure Add_File_From_Disk
      (Writer       : in out Tar_Writer;
       Archive_Name : String;
       Disk_Path    : String)
   is
      use Ada.Directories;

      Content : Unbounded_String := Null_Unbounded_String;
   begin
      if not Writer.Is_Open or not Exists (Disk_Path) then
         return;
      end if;

      --  Read file content
      declare
         File   : SIO.File_Type;
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
         Last   : Ada.Streams.Stream_Element_Offset;
      begin
         SIO.Open (File, SIO.In_File, Disk_Path);
         while not SIO.End_Of_File (File) loop
            SIO.Read (File, Buffer, Last);
            for I in 1 .. Last loop
               Append (Content, Character'Val (Natural (Buffer (I))));
            end loop;
         end loop;
         SIO.Close (File);
      end;

      --  Add to archive
      Add_File (Writer, Archive_Name, To_String (Content));
   end Add_File_From_Disk;

   procedure Close (Writer : in out Tar_Writer) is
      use Ada.Streams;

      File       : SIO.File_Type;
      End_Blocks : constant Stream_Element_Array (1 .. Block_Size * 2) :=
         (others => 0);
   begin
      if not Writer.Is_Open then
         return;
      end if;

      --  Append two zero blocks to mark end of archive
      SIO.Open (File, SIO.Append_File, To_String (Writer.Path));
      SIO.Write (File, End_Blocks);
      SIO.Close (File);

      Writer.Is_Open := False;
   end Close;

   function Is_Open (Writer : Tar_Writer) return Boolean is
   begin
      return Writer.Is_Open;
   end Is_Open;

end Cerro_Tar;
