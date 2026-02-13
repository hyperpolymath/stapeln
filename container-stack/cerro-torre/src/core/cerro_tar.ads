--  Cerro_Tar - TAR archive creation
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Creates POSIX ustar format tar archives.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;

package Cerro_Tar is

   --  TAR block size (always 512 bytes)
   Block_Size : constant := 512;

   --  File type flags
   Type_Regular   : constant Character := '0';
   Type_Directory : constant Character := '5';

   --  TAR header (512 bytes)
   type Tar_Header is record
      Name     : String (1 .. 100);   --  File name
      Mode     : String (1 .. 8);     --  File mode (octal)
      UID      : String (1 .. 8);     --  Owner UID (octal)
      GID      : String (1 .. 8);     --  Owner GID (octal)
      Size     : String (1 .. 12);    --  File size (octal)
      Mtime    : String (1 .. 12);    --  Modification time (octal)
      Checksum : String (1 .. 8);     --  Header checksum
      Typeflag : Character;           --  File type
      Linkname : String (1 .. 100);   --  Link target
      Magic    : String (1 .. 6);     --  "ustar" + NUL
      Version  : String (1 .. 2);     --  "00"
      Uname    : String (1 .. 32);    --  Owner name
      Gname    : String (1 .. 32);    --  Group name
      Devmajor : String (1 .. 8);     --  Device major
      Devminor : String (1 .. 8);     --  Device minor
      Prefix   : String (1 .. 155);   --  Filename prefix
      Padding  : String (1 .. 12);    --  Padding to 512 bytes
   end record;

   for Tar_Header'Size use Block_Size * 8;

   --  Archive writer state
   type Tar_Writer is limited private;

   --  Create a new tar archive at the given path
   procedure Create (Writer : out Tar_Writer; Path : String);

   --  Add a file to the archive
   procedure Add_File
      (Writer   : in out Tar_Writer;
       Name     : String;           --  Name in archive
       Content  : String);          --  File content

   --  Add a file from disk to the archive
   procedure Add_File_From_Disk
      (Writer      : in out Tar_Writer;
       Archive_Name: String;        --  Name in archive
       Disk_Path   : String);       --  Path on disk

   --  Finalize and close the archive
   procedure Close (Writer : in out Tar_Writer);

   --  Check if writer is open
   function Is_Open (Writer : Tar_Writer) return Boolean;

private

   type Tar_Writer is limited record
      Path    : Unbounded_String;
      Is_Open : Boolean := False;
   end record;

end Cerro_Tar;
