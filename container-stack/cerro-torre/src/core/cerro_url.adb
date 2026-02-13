-------------------------------------------------------------------------------
--  Cerro_URL - URL Encoding Operations Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Cerro_URL
   with SPARK_Mode => Off  --  String building requires unbounded strings
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Hex_Digits : constant String := "0123456789ABCDEF";

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Is_Unreserved (C : Character) return Boolean is
   begin
      return Is_Alphanumeric (C) or else
             C = '-' or else C = '_' or else C = '.' or else C = '~';
   end Is_Unreserved;

   function Hex_To_Char (Hi, Lo : Character) return Character is
      Hi_Val : Natural;
      Lo_Val : Natural;
   begin
      --  Convert hex digits to values
      if Hi in '0' .. '9' then
         Hi_Val := Character'Pos (Hi) - Character'Pos ('0');
      elsif Hi in 'A' .. 'F' then
         Hi_Val := Character'Pos (Hi) - Character'Pos ('A') + 10;
      elsif Hi in 'a' .. 'f' then
         Hi_Val := Character'Pos (Hi) - Character'Pos ('a') + 10;
      else
         return ' ';  --  Invalid hex
      end if;

      if Lo in '0' .. '9' then
         Lo_Val := Character'Pos (Lo) - Character'Pos ('0');
      elsif Lo in 'A' .. 'F' then
         Lo_Val := Character'Pos (Lo) - Character'Pos ('A') + 10;
      elsif Lo in 'a' .. 'f' then
         Lo_Val := Character'Pos (Lo) - Character'Pos ('a') + 10;
      else
         return ' ';  --  Invalid hex
      end if;

      return Character'Val (Hi_Val * 16 + Lo_Val);
   end Hex_To_Char;

   ---------------------------------------------------------------------------
   --  Public API
   ---------------------------------------------------------------------------

   function URL_Encode (Input : String) return String is
      Result : Unbounded_String;
   begin
      for C of Input loop
         if Is_Unreserved (C) then
            Append (Result, C);
         else
            --  Percent-encode as %XX
            declare
               Byte : constant Natural := Character'Pos (C);
               Hi   : constant Natural := Byte / 16;
               Lo   : constant Natural := Byte mod 16;
            begin
               Append (Result, '%');
               Append (Result, Hex_Digits (Hex_Digits'First + Hi));
               Append (Result, Hex_Digits (Hex_Digits'First + Lo));
            end;
         end if;
      end loop;

      return To_String (Result);
   end URL_Encode;

   function URL_Decode (Input : String) return String is
      Result : Unbounded_String;
      I      : Positive := Input'First;
   begin
      while I <= Input'Last loop
         if Input (I) = '%' then
            --  Decode %XX sequence
            if I + 2 > Input'Last then
               return "";  --  Invalid encoding
            end if;

            declare
               Decoded : constant Character :=
                 Hex_To_Char (Input (I + 1), Input (I + 2));
            begin
               if Decoded = ' ' and then
                  (Input (I + 1) /= '2' or Input (I + 2) /= '0')
               then
                  return "";  --  Invalid hex (unless it's actually %20 for space)
               end if;
               Append (Result, Decoded);
            end;
            I := I + 3;

         elsif Input (I) = '+' then
            --  Plus sign decodes to space (application/x-www-form-urlencoded)
            Append (Result, ' ');
            I := I + 1;

         else
            Append (Result, Input (I));
            I := I + 1;
         end if;
      end loop;

      return To_String (Result);
   end URL_Decode;

   function Is_Valid_Encoded (Input : String) return Boolean is
      I : Positive := Input'First;
   begin
      while I <= Input'Last loop
         if Input (I) = '%' then
            --  Check that %XX is valid
            if I + 2 > Input'Last then
               return False;  --  Truncated
            end if;

            if not Is_Hexadecimal_Digit (Input (I + 1)) or else
               not Is_Hexadecimal_Digit (Input (I + 2))
            then
               return False;  --  Invalid hex
            end if;

            I := I + 3;
         else
            I := I + 1;
         end if;
      end loop;

      return True;
   end Is_Valid_Encoded;

end Cerro_URL;
