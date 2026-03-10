-------------------------------------------------------------------------------
--  CT_JSON - Minimal JSON parser implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;

package body CT_JSON is

   use Ada.Strings.Fixed;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Find the position of a field in JSON
   function Find_Field
     (JSON  : String;
      Field : String) return Natural
   is
      Search : constant String := """" & Field & """:";
   begin
      return Index (JSON, Search);
   end Find_Field;

   --  Skip whitespace from position
   function Skip_Whitespace
     (JSON : String;
      Pos  : Positive) return Positive
   is
      I : Positive := Pos;
   begin
      while I <= JSON'Last and then
            (JSON (I) = ' ' or JSON (I) = ASCII.HT or
             JSON (I) = ASCII.LF or JSON (I) = ASCII.CR)
      loop
         I := I + 1;
      end loop;
      return I;
   end Skip_Whitespace;

   --  Extract string value starting at position (after opening quote)
   function Extract_String_Value
     (JSON : String;
      Pos  : Positive) return String
   is
      Start  : constant Positive := Pos;
      I      : Positive := Start;
      Escape : Boolean := False;
   begin
      --  Find closing quote (handle escaped quotes)
      while I <= JSON'Last loop
         if Escape then
            Escape := False;
         elsif JSON (I) = '\' then
            Escape := True;
         elsif JSON (I) = '"' then
            return Unescape_JSON_String (JSON (Start .. I - 1));
         end if;
         I := I + 1;
      end loop;

      return "";  --  No closing quote found
   end Extract_String_Value;

   ---------------------------------------------------------------------------
   --  Public API - Value Extraction
   ---------------------------------------------------------------------------

   function Get_String_Field
     (JSON  : String;
      Field : String) return String
   is
      Field_Pos : constant Natural := Find_Field (JSON, Field);
   begin
      if Field_Pos = 0 then
         return "";  --  Field not found
      end if;

      --  Skip past field name and colon to value
      declare
         Value_Start : Positive := Field_Pos + Field'Length + 3;  --  "field":
      begin
         Value_Start := Skip_Whitespace (JSON, Value_Start);

         if Value_Start > JSON'Last or else JSON (Value_Start) /= '"' then
            return "";  --  Not a string value
         end if;

         return Extract_String_Value (JSON, Value_Start + 1);
      end;
   end Get_String_Field;

   function Get_Integer_Field
     (JSON  : String;
      Field : String) return Integer
   is
      Field_Pos : constant Natural := Find_Field (JSON, Field);
   begin
      if Field_Pos = 0 then
         return 0;  --  Field not found
      end if;

      declare
         Value_Start : Positive := Field_Pos + Field'Length + 3;
         Value_End   : Natural := Value_Start;
      begin
         Value_Start := Skip_Whitespace (JSON, Value_Start);

         --  Find end of number (comma, brace, or whitespace)
         while Value_End <= JSON'Last and then
               JSON (Value_End) /= ',' and then
               JSON (Value_End) /= '}' and then
               JSON (Value_End) /= ' '
         loop
            Value_End := Value_End + 1;
         end loop;

         return Integer'Value (JSON (Value_Start .. Value_End - 1));
      exception
         when others =>
            return 0;  --  Invalid integer
      end;
   end Get_Integer_Field;

   function Get_Boolean_Field
     (JSON  : String;
      Field : String) return Boolean
   is
      Field_Pos : constant Natural := Find_Field (JSON, Field);
   begin
      if Field_Pos = 0 then
         return False;  --  Field not found
      end if;

      declare
         Value_Start : Positive := Field_Pos + Field'Length + 3;
      begin
         Value_Start := Skip_Whitespace (JSON, Value_Start);

         if Value_Start + 3 <= JSON'Last and then
            JSON (Value_Start .. Value_Start + 3) = "true"
         then
            return True;
         else
            return False;
         end if;
      end;
   end Get_Boolean_Field;

   --  Find the end of a nested JSON object or array starting at Pos.
   --  Pos must point to the opening '{' or '['.
   --  Returns the position of the matching closing '}' or ']'.
   function Find_Matching_Close
     (JSON : String;
      Pos  : Positive) return Natural
   is
      Depth   : Natural := 0;
      I       : Positive := Pos;
      Opener  : constant Character := JSON (Pos);
      Closer  : constant Character := (if Opener = '{' then '}' else ']');
      In_Str  : Boolean := False;
      Escaped : Boolean := False;
   begin
      while I <= JSON'Last loop
         if Escaped then
            Escaped := False;
         elsif In_Str then
            if JSON (I) = '\' then
               Escaped := True;
            elsif JSON (I) = '"' then
               In_Str := False;
            end if;
         else
            if JSON (I) = '"' then
               In_Str := True;
            elsif JSON (I) = Opener then
               Depth := Depth + 1;
            elsif JSON (I) = Closer then
               Depth := Depth - 1;
               if Depth = 0 then
                  return I;
               end if;
            end if;
         end if;
         I := I + 1;
      end loop;
      return 0;  --  No matching close found
   end Find_Matching_Close;

   --  Find the end of a number value starting at Pos.
   --  Returns the position one past the last digit/sign/dot/e character.
   function Find_Number_End
     (JSON : String;
      Pos  : Positive) return Positive
   is
      I : Positive := Pos;
   begin
      while I <= JSON'Last and then
            (JSON (I) in '0' .. '9' or else JSON (I) = '-' or else
             JSON (I) = '+' or else JSON (I) = '.' or else
             JSON (I) = 'e' or else JSON (I) = 'E')
      loop
         I := I + 1;
      end loop;
      return I;
   end Find_Number_End;

   function Get_Array_Field
     (JSON  : String;
      Field : String) return String_Array
   is
      Result    : String_Array;
      Field_Pos : constant Natural := Find_Field (JSON, Field);
   begin
      if Field_Pos = 0 then
         return Result;  --  Field not found, return empty array
      end if;

      declare
         Array_Start : Positive := Field_Pos + Field'Length + 3;
         I           : Positive;
      begin
         Array_Start := Skip_Whitespace (JSON, Array_Start);

         if Array_Start > JSON'Last or else JSON (Array_Start) /= '[' then
            return Result;  --  Not an array
         end if;

         I := Array_Start + 1;

         --  Parse array elements (strings, numbers, booleans, objects, arrays)
         loop
            I := Skip_Whitespace (JSON, I);

            exit when I > JSON'Last or else JSON (I) = ']';

            if JSON (I) = '"' then
               --  String element
               declare
                  Str : constant String := Extract_String_Value (JSON, I + 1);
               begin
                  Result.Append (To_Unbounded_String (Str));
                  --  Advance past opening quote, content, and closing quote.
                  --  The raw length in JSON may differ from Str'Length when
                  --  escape sequences are present (e.g. \" becomes ").  Walk
                  --  forward from the opening quote to find the actual closing
                  --  quote in the source text.
                  declare
                     J      : Positive := I + 1;  --  past opening "
                     Esc    : Boolean := False;
                  begin
                     while J <= JSON'Last loop
                        if Esc then
                           Esc := False;
                        elsif JSON (J) = '\' then
                           Esc := True;
                        elsif JSON (J) = '"' then
                           I := J + 1;  --  past closing "
                           exit;
                        end if;
                        J := J + 1;
                     end loop;
                  end;
               end;

            elsif JSON (I) = '{' then
               --  Nested object element — capture raw JSON text
               declare
                  Close_Pos : constant Natural := Find_Matching_Close (JSON, I);
               begin
                  if Close_Pos = 0 then
                     exit;  --  Malformed JSON, stop parsing
                  end if;
                  Result.Append (To_Unbounded_String (
                     JSON (I .. Close_Pos)));
                  I := Close_Pos + 1;
               end;

            elsif JSON (I) = '[' then
               --  Nested array element — capture raw JSON text
               declare
                  Close_Pos : constant Natural := Find_Matching_Close (JSON, I);
               begin
                  if Close_Pos = 0 then
                     exit;  --  Malformed JSON, stop parsing
                  end if;
                  Result.Append (To_Unbounded_String (
                     JSON (I .. Close_Pos)));
                  I := Close_Pos + 1;
               end;

            elsif JSON (I) in '0' .. '9' or else JSON (I) = '-' then
               --  Number element
               declare
                  Num_End : constant Positive := Find_Number_End (JSON, I);
               begin
                  Result.Append (To_Unbounded_String (
                     JSON (I .. Num_End - 1)));
                  I := Num_End;
               end;

            elsif I + 3 <= JSON'Last and then
                  JSON (I .. I + 3) = "true"
            then
               --  Boolean true
               Result.Append (To_Unbounded_String ("true"));
               I := I + 4;

            elsif I + 4 <= JSON'Last and then
                  JSON (I .. I + 4) = "false"
            then
               --  Boolean false
               Result.Append (To_Unbounded_String ("false"));
               I := I + 5;

            elsif I + 3 <= JSON'Last and then
                  JSON (I .. I + 3) = "null"
            then
               --  Null value
               Result.Append (To_Unbounded_String ("null"));
               I := I + 4;

            else
               --  Unexpected character, skip it to avoid infinite loop
               I := I + 1;
            end if;

            --  Skip comma between elements
            I := Skip_Whitespace (JSON, I);
            if I <= JSON'Last and then JSON (I) = ',' then
               I := I + 1;
            end if;
         end loop;

         return Result;
      end;
   end Get_Array_Field;

   function Has_Field
     (JSON  : String;
      Field : String) return Boolean
   is
   begin
      return Find_Field (JSON, Field) /= 0;
   end Has_Field;

   ---------------------------------------------------------------------------
   --  Public API - JSON Construction
   ---------------------------------------------------------------------------

   function Create return JSON_Builder is
      Builder : JSON_Builder;
   begin
      Builder.Content := To_Unbounded_String ("{");
      Builder.Empty := True;
      return Builder;
   end Create;

   procedure Add_Separator (Builder : in out JSON_Builder) is
   begin
      if not Builder.Empty then
         Append (Builder.Content, ",");
      end if;
      Builder.Empty := False;
   end Add_Separator;

   procedure Add_String
     (Builder : in out JSON_Builder;
      Key     : String;
      Value   : String)
   is
   begin
      Add_Separator (Builder);
      Append (Builder.Content, """" & Key & """:""" &
              Escape_JSON_String (Value) & """");
   end Add_String;

   procedure Add_Integer
     (Builder : in out JSON_Builder;
      Key     : String;
      Value   : Integer)
   is
   begin
      Add_Separator (Builder);
      Append (Builder.Content, """" & Key & """:" & Integer'Image (Value));
   end Add_Integer;

   procedure Add_Boolean
     (Builder : in out JSON_Builder;
      Key     : String;
      Value   : Boolean)
   is
   begin
      Add_Separator (Builder);
      Append (Builder.Content, """" & Key & """:" &
              (if Value then "true" else "false"));
   end Add_Boolean;

   procedure Add_Null
     (Builder : in out JSON_Builder;
      Key     : String)
   is
   begin
      Add_Separator (Builder);
      Append (Builder.Content, """" & Key & """:null");
   end Add_Null;

   procedure Add_Array
     (Builder : in out JSON_Builder;
      Key     : String;
      Values  : String_Array)
   is
   begin
      Add_Separator (Builder);
      Append (Builder.Content, """" & Key & """:[");

      for I in Values.First_Index .. Values.Last_Index loop
         if I > Values.First_Index then
            Append (Builder.Content, ",");
         end if;
         Append (Builder.Content, """" &
                 Escape_JSON_String (To_String (Values (I))) & """");
      end loop;

      Append (Builder.Content, "]");
   end Add_Array;

   procedure Add_Object
     (Builder : in out JSON_Builder;
      Key     : String;
      Value   : String)
   is
   begin
      Add_Separator (Builder);
      Append (Builder.Content, """" & Key & """:" & Value);
   end Add_Object;

   function To_JSON (Builder : JSON_Builder) return String is
   begin
      return To_String (Builder.Content) & "}";
   end To_JSON;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Escape_JSON_String (S : String) return String is
      Result : Unbounded_String;
   begin
      for C of S loop
         case C is
            when '"'     => Append (Result, "\""");
            when '\'     => Append (Result, "\\");
            when ASCII.BS => Append (Result, "\b");
            when ASCII.FF => Append (Result, "\f");
            when ASCII.LF => Append (Result, "\n");
            when ASCII.CR => Append (Result, "\r");
            when ASCII.HT => Append (Result, "\t");
            when others  => Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Escape_JSON_String;

   function Unescape_JSON_String (S : String) return String is
      Result : Unbounded_String;
      I      : Positive := S'First;
   begin
      while I <= S'Last loop
         if S (I) = '\' and then I < S'Last then
            case S (I + 1) is
               when '"'  => Append (Result, '"');
               when '\'  => Append (Result, '\');
               when 'b'  => Append (Result, ASCII.BS);
               when 'f'  => Append (Result, ASCII.FF);
               when 'n'  => Append (Result, ASCII.LF);
               when 'r'  => Append (Result, ASCII.CR);
               when 't'  => Append (Result, ASCII.HT);
               when '/'  => Append (Result, '/');
               when others => Append (Result, S (I + 1));
            end case;
            I := I + 2;
         else
            Append (Result, S (I));
            I := I + 1;
         end if;
      end loop;
      return To_String (Result);
   end Unescape_JSON_String;

end CT_JSON;
