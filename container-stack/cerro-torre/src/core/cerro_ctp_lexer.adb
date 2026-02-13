--  Cerro Torre CTP Lexer - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later

pragma SPARK_Mode (On);

package body Cerro_CTP_Lexer is

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Is_Ident_Start (C : Character) return Boolean is
   begin
      return C in 'a' .. 'z' | 'A' .. 'Z' | '_';
   end Is_Ident_Start;

   function Is_Ident_Char (C : Character) return Boolean is
   begin
      return C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-';
   end Is_Ident_Char;

   function Is_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '9';
   end Is_Digit;

   function Is_Hex_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';
   end Is_Hex_Digit;

   function Is_Whitespace (C : Character) return Boolean is
   begin
      return C = ' ' or C = ASCII.HT;
   end Is_Whitespace;

   ---------------------------------------------------------------------------
   --  Init
   ---------------------------------------------------------------------------

   function Init (Content : String) return Lexer_State is
   begin
      return (Input    => To_Unbounded_String (Content),
              Position => 1,
              Line     => 1,
              Column   => 1,
              Length   => Content'Length);
   end Init;

   ---------------------------------------------------------------------------
   --  Peek_Char
   ---------------------------------------------------------------------------

   function Peek_Char (State : Lexer_State) return Character is
   begin
      if State.Position > State.Length then
         return ASCII.NUL;
      end if;
      return Element (State.Input, State.Position);
   end Peek_Char;

   ---------------------------------------------------------------------------
   --  At_End
   ---------------------------------------------------------------------------

   function At_End (State : Lexer_State) return Boolean is
   begin
      return State.Position > State.Length;
   end At_End;

   ---------------------------------------------------------------------------
   --  Advance - Internal helper to move forward one character
   ---------------------------------------------------------------------------

   procedure Advance (State : in out Lexer_State) is
      C : Character;
   begin
      if State.Position <= State.Length then
         C := Element (State.Input, State.Position);
         State.Position := State.Position + 1;
         if C = ASCII.LF then
            State.Line := State.Line + 1;
            State.Column := 1;
         else
            State.Column := State.Column + 1;
         end if;
      end if;
   end Advance;

   ---------------------------------------------------------------------------
   --  Peek_Ahead - Look at character at offset from current position
   ---------------------------------------------------------------------------

   function Peek_Ahead (State : Lexer_State; Offset : Natural) return Character is
      Pos : constant Natural := State.Position + Offset;
   begin
      if Pos > State.Length then
         return ASCII.NUL;
      end if;
      return Element (State.Input, Pos);
   end Peek_Ahead;

   ---------------------------------------------------------------------------
   --  Skip_Whitespace - Skip spaces and tabs (not newlines)
   ---------------------------------------------------------------------------

   procedure Skip_Whitespace (State : in out Lexer_State) is
   begin
      while State.Position <= State.Length loop
         declare
            C : constant Character := Element (State.Input, State.Position);
         begin
            exit when not Is_Whitespace (C);
            Advance (State);
         end;
      end loop;
   end Skip_Whitespace;

   ---------------------------------------------------------------------------
   --  Read_String - Read a quoted string
   ---------------------------------------------------------------------------

   procedure Read_String
      (State : in out Lexer_State;
       Tok   : out Token)
   is
      Start_Line   : constant Positive := State.Line;
      Start_Column : constant Positive := State.Column;
      Result       : Unbounded_String := Null_Unbounded_String;
      Is_Multiline : Boolean := False;
   begin
      Tok.Kind := Tok_String;
      Tok.Line := Start_Line;
      Tok.Column := Start_Column;

      --  Skip opening quote
      Advance (State);

      --  Check for triple-quote multiline string
      if Peek_Char (State) = '"' and Peek_Ahead (State, 1) = '"' then
         Is_Multiline := True;
         Advance (State);  --  Skip second quote
         Advance (State);  --  Skip third quote
         --  Skip optional newline after opening """
         if Peek_Char (State) = ASCII.LF then
            Advance (State);
         end if;
      end if;

      --  Read string content
      while State.Position <= State.Length loop
         declare
            C : constant Character := Peek_Char (State);
         begin
            if Is_Multiline then
               --  Check for closing """
               if C = '"' and Peek_Ahead (State, 1) = '"'
                  and Peek_Ahead (State, 2) = '"'
               then
                  Advance (State);  --  Skip first quote
                  Advance (State);  --  Skip second quote
                  Advance (State);  --  Skip third quote
                  exit;
               end if;
               Append (Result, C);
               Advance (State);
            else
               --  Single-line string
               if C = '"' then
                  Advance (State);  --  Skip closing quote
                  exit;
               elsif C = ASCII.LF then
                  --  Unterminated string
                  Tok.Kind := Tok_Error;
                  Tok.Value := To_Unbounded_String ("Unterminated string");
                  return;
               elsif C = '\' then
                  --  Escape sequence
                  Advance (State);
                  declare
                     Escaped : constant Character := Peek_Char (State);
                  begin
                     case Escaped is
                        when 'n'  => Append (Result, ASCII.LF);
                        when 't'  => Append (Result, ASCII.HT);
                        when '"'  => Append (Result, '"');
                        when '\' => Append (Result, '\');
                        when others =>
                           --  Unknown escape, keep literal
                           Append (Result, '\');
                           Append (Result, Escaped);
                     end case;
                     Advance (State);
                  end;
               else
                  Append (Result, C);
                  Advance (State);
               end if;
            end if;
         end;
      end loop;

      Tok.Value := Result;
   end Read_String;

   ---------------------------------------------------------------------------
   --  Read_Number_Or_Date - Read integer, date, or datetime
   ---------------------------------------------------------------------------

   procedure Read_Number_Or_Date
      (State : in Out Lexer_State;
       Tok   : out Token)
   is
      Start_Line   : constant Positive := State.Line;
      Start_Column : constant Positive := State.Column;
      Result       : Unbounded_String := Null_Unbounded_String;
      Has_Dash     : Boolean := False;
      Has_T        : Boolean := False;
      Is_Negative  : Boolean := False;
   begin
      Tok.Line := Start_Line;
      Tok.Column := Start_Column;

      --  Check for negative sign
      if Peek_Char (State) = '-' then
         Is_Negative := True;
         Append (Result, '-');
         Advance (State);
      end if;

      --  Read digits and potential date/time characters
      while State.Position <= State.Length loop
         declare
            C : constant Character := Peek_Char (State);
         begin
            if Is_Digit (C) then
               Append (Result, C);
               Advance (State);
            elsif C = '-' and not Is_Negative then
               Has_Dash := True;
               Append (Result, C);
               Advance (State);
            elsif C = 'T' then
               Has_T := True;
               Append (Result, C);
               Advance (State);
            elsif C = ':' and Has_T then
               Append (Result, C);
               Advance (State);
            elsif C = 'Z' and Has_T then
               Append (Result, C);
               Advance (State);
               exit;  --  Z marks end of datetime
            else
               exit;
            end if;
         end;
      end loop;

      Tok.Value := Result;

      --  Determine token type
      if Has_T then
         Tok.Kind := Tok_DateTime;
      elsif Has_Dash and Length (Result) = 10 then
         --  YYYY-MM-DD format
         Tok.Kind := Tok_Date;
      else
         Tok.Kind := Tok_Integer;
      end if;
   end Read_Number_Or_Date;

   ---------------------------------------------------------------------------
   --  Read_Identifier_Or_Keyword - Read identifier or boolean keyword
   ---------------------------------------------------------------------------

   procedure Read_Identifier_Or_Keyword
      (State : in out Lexer_State;
       Tok   : out Token)
   is
      Start_Line   : constant Positive := State.Line;
      Start_Column : constant Positive := State.Column;
      Result       : Unbounded_String := Null_Unbounded_String;
   begin
      Tok.Line := Start_Line;
      Tok.Column := Start_Column;

      --  Read identifier characters
      while State.Position <= State.Length loop
         declare
            C : constant Character := Peek_Char (State);
         begin
            exit when not Is_Ident_Char (C) and C /= '.';
            Append (Result, C);
            Advance (State);
         end;
      end loop;

      Tok.Value := Result;

      --  Check for boolean keywords
      declare
         S : constant String := To_String (Result);
      begin
         if S = "true" or S = "false" then
            Tok.Kind := Tok_Boolean;
         else
            Tok.Kind := Tok_Identifier;
         end if;
      end;
   end Read_Identifier_Or_Keyword;

   ---------------------------------------------------------------------------
   --  Read_Comment - Read comment to end of line
   ---------------------------------------------------------------------------

   procedure Read_Comment
      (State : in Out Lexer_State;
       Tok   : out Token)
   is
      Start_Line   : constant Positive := State.Line;
      Start_Column : constant Positive := State.Column;
      Result       : Unbounded_String := Null_Unbounded_String;
   begin
      Tok.Kind := Tok_Comment;
      Tok.Line := Start_Line;
      Tok.Column := Start_Column;

      --  Skip the # character
      Advance (State);

      --  Read to end of line
      while State.Position <= State.Length loop
         declare
            C : constant Character := Peek_Char (State);
         begin
            exit when C = ASCII.LF;
            Append (Result, C);
            Advance (State);
         end;
      end loop;

      Tok.Value := Result;
   end Read_Comment;

   ---------------------------------------------------------------------------
   --  Next_Token
   ---------------------------------------------------------------------------

   procedure Next_Token
      (State : in Out Lexer_State;
       Tok   : out Token)
   is
   begin
      --  Skip whitespace (not newlines)
      Skip_Whitespace (State);

      --  Initialize token with current position
      Tok.Line := State.Line;
      Tok.Column := State.Column;
      Tok.Value := Null_Unbounded_String;

      --  Check for end of input
      if At_End (State) then
         Tok.Kind := Tok_EOF;
         return;
      end if;

      declare
         C : constant Character := Peek_Char (State);
      begin
         case C is
            when ASCII.LF =>
               Tok.Kind := Tok_Newline;
               Advance (State);

            when '=' =>
               Tok.Kind := Tok_Equals;
               Advance (State);

            when ',' =>
               Tok.Kind := Tok_Comma;
               Advance (State);

            when '.' =>
               Tok.Kind := Tok_Dot;
               Advance (State);

            when ':' =>
               Tok.Kind := Tok_Colon;
               Advance (State);

            when '[' =>
               Tok.Kind := Tok_Lbracket;
               Advance (State);

            when ']' =>
               Tok.Kind := Tok_Rbracket;
               Advance (State);

            when '{' =>
               Tok.Kind := Tok_Lbrace;
               Advance (State);

            when '}' =>
               Tok.Kind := Tok_Rbrace;
               Advance (State);

            when '"' =>
               Read_String (State, Tok);

            when '#' =>
               Read_Comment (State, Tok);

            when '-' | '0' .. '9' =>
               Read_Number_Or_Date (State, Tok);

            when 'a' .. 'z' | 'A' .. 'Z' | '_' =>
               Read_Identifier_Or_Keyword (State, Tok);

            when others =>
               Tok.Kind := Tok_Error;
               Tok.Value := To_Unbounded_String ("Unexpected character: " & C);
               Advance (State);
         end case;
      end;
   end Next_Token;

end Cerro_CTP_Lexer;
