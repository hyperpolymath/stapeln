-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Vörðr Gatekeeper
-- OCI configuration JSON parser implementation

pragma SPARK_Mode (On);

with Container_Policy; use Container_Policy;

package body OCI_Parser is

   ----------------------
   -- Skip_Whitespace --
   ----------------------

   procedure Skip_Whitespace (
      Json  : String;
      State : in out Parser_State
   )
   is
   begin
      while State.Position <= State.Length
        and then State.Position <= Json'Last
        and then Is_Whitespace (Json (State.Position))
      loop
         State.Position := State.Position + 1;
      end loop;
   end Skip_Whitespace;

   -------------------
   -- Match_String --
   -------------------

   function Match_String (
      Json    : String;
      State   : in Out Parser_State;
      Pattern : String
   ) return Boolean
   is
      Start_Pos : constant Natural := State.Position;
   begin
      if State.Position + Pattern'Length - 1 > State.Length then
         return False;
      end if;

      if State.Position + Pattern'Length - 1 > Json'Last then
         return False;
      end if;

      for I in Pattern'Range loop
         if Json (State.Position + I - Pattern'First) /= Pattern (I) then
            State.Position := Start_Pos;
            return False;
         end if;
      end loop;

      State.Position := State.Position + Pattern'Length;
      return True;
   end Match_String;

   -------------------
   -- Parse_Boolean --
   -------------------

   function Parse_Boolean (
      Json  : String;
      State : in Out Parser_State
   ) return Boolean
   is
   begin
      Skip_Whitespace (Json, State);

      if Match_String (Json, State, "true") then
         return True;
      elsif Match_String (Json, State, "false") then
         return False;
      else
         --  Default to false on parse error
         return False;
      end if;
   end Parse_Boolean;

   -------------------
   -- Parse_Natural --
   -------------------

   function Parse_Natural (
      Json  : String;
      State : in Out Parser_State
   ) return Natural
   is
      Result : Natural := 0;
   begin
      Skip_Whitespace (Json, State);

      while State.Position <= State.Length
        and then State.Position <= Json'Last
        and then Is_Digit (Json (State.Position))
      loop
         --  Prevent overflow
         if Result > Natural'Last / 10 then
            return Natural'Last;
         end if;

         Result := Result * 10 +
           (Character'Pos (Json (State.Position)) - Character'Pos ('0'));
         State.Position := State.Position + 1;
      end loop;

      return Result;
   end Parse_Natural;

   ------------------------
   -- Parse_String_Value --
   ------------------------

   function Parse_String_Value (
      Json   : String;
      State  : in Out Parser_State;
      Result : out String;
      Length : out Natural
   ) return Boolean
   is
      Start_Pos : Natural;
   begin
      Length := 0;
      Result := (others => ' ');

      Skip_Whitespace (Json, State);

      --  Expect opening quote
      if State.Position > State.Length
        or else State.Position > Json'Last
        or else Json (State.Position) /= '"'
      then
         return False;
      end if;

      State.Position := State.Position + 1;
      Start_Pos := State.Position;

      --  Find closing quote
      while State.Position <= State.Length
        and then State.Position <= Json'Last
        and then Json (State.Position) /= '"'
      loop
         --  Handle escape sequences (simplified)
         if Json (State.Position) = '\' then
            State.Position := State.Position + 2;
         else
            if Length < Result'Length then
               Length := Length + 1;
               Result (Result'First + Length - 1) := Json (State.Position);
            end if;
            State.Position := State.Position + 1;
         end if;
      end loop;

      --  Consume closing quote
      if State.Position <= State.Length
        and then State.Position <= Json'Last
        and then Json (State.Position) = '"'
      then
         State.Position := State.Position + 1;
         return True;
      end if;

      return False;
   end Parse_String_Value;

   -----------------
   -- Find_Field --
   -----------------

   function Find_Field (
      Json  : String;
      State : in Out Parser_State;
      Name  : String
   ) return Boolean
   is
      Field_Name   : String (1 .. 256);
      Field_Length : Natural;
      Depth        : Natural := 0;
   begin
      --  Search through the JSON for the field name
      while State.Position <= State.Length
        and then State.Position <= Json'Last
      loop
         Skip_Whitespace (Json, State);

         if State.Position > State.Length or State.Position > Json'Last then
            return False;
         end if;

         case Json (State.Position) is
            when '{' | '[' =>
               Depth := Depth + 1;
               State.Position := State.Position + 1;

            when '}' | ']' =>
               if Depth > 0 then
                  Depth := Depth - 1;
               end if;
               State.Position := State.Position + 1;

            when '"' =>
               --  Parse string - could be field name or value
               if Parse_String_Value (Json, State, Field_Name, Field_Length) then
                  Skip_Whitespace (Json, State);

                  --  Check if this is a field (followed by :)
                  if State.Position <= State.Length
                    and then State.Position <= Json'Last
                    and then Json (State.Position) = ':'
                  then
                     State.Position := State.Position + 1;

                     --  Check if this is the field we're looking for
                     if Field_Length = Name'Length
                       and then Field_Name (1 .. Field_Length) = Name
                       and then Depth = 1
                     then
                        return True;
                     end if;
                  end if;
               end if;

            when ',' | ':' =>
               State.Position := State.Position + 1;

            when others =>
               --  Skip other characters (numbers, literals, etc.)
               State.Position := State.Position + 1;
         end case;
      end loop;

      return False;
   end Find_Field;

   ----------------------
   -- Parse_Oci_Config --
   ----------------------

   function Parse_Oci_Config (
      Json   : String;
      Length : Natural
   ) return Parse_Result
   is
      Result : Parse_Result;
      State  : Parser_State;
      Dummy_String : String (1 .. 256);
      Dummy_Length : Natural;
   begin
      --  Initialize with defaults
      Result.Status := Parse_OK;
      Result.Config := Container_Policy.Default_Config;

      --  Check length
      if Length > Max_Json_Length then
         Result.Status := Parse_Too_Long;
         return Result;
      end if;

      if Length = 0 then
         Result.Status := Parse_Invalid_Json;
         return Result;
      end if;

      State.Position := Json'First;
      State.Length := Length;

      --  Look for "process" object to find user info
      State.Position := Json'First;
      if Find_Field (Json, State, "process") then
         --  Look for "user" within process
         if Find_Field (Json, State, "user") then
            --  Look for "uid"
            if Find_Field (Json, State, "uid") then
               Result.Config.User_ID := Parse_Natural (Json, State);
            end if;
         end if;
      end if;

      --  Look for "linux" object to find namespace info
      State.Position := Json'First;
      if Find_Field (Json, State, "linux") then
         --  Look for "namespaces"
         if Find_Field (Json, State, "namespaces") then
            --  Check for "user" namespace type
            State.Position := Json'First;
            --  Simplified: if we find "user" in namespaces, enable it
            if Find_Field (Json, State, "type") then
               if Parse_String_Value (Json, State, Dummy_String, Dummy_Length) then
                  if Dummy_Length = 4
                    and then Dummy_String (1 .. 4) = "user"
                  then
                     Result.Config.User_Namespace := True;
                  end if;
               end if;
            end if;
         end if;
      end if;

      --  Look for "root" object
      State.Position := Json'First;
      if Find_Field (Json, State, "root") then
         if Find_Field (Json, State, "readonly") then
            Result.Config.Root_Read_Only := Parse_Boolean (Json, State);
         end if;
      end if;

      --  Apply security defaults to ensure valid configuration
      Container_Policy.Apply_Security_Defaults (Result.Config);

      return Result;
   end Parse_Oci_Config;

end OCI_Parser;
