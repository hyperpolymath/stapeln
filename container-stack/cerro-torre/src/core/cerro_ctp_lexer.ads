--  Cerro Torre CTP Lexer - Tokenizer for CTP manifest format
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  This package provides tokenization for CTP (Cerro Torre Package) manifests.
--  The lexer is proven to terminate on all inputs.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Cerro_CTP_Lexer
   with SPARK_Mode => On
is
   --  Maximum sizes for safety/termination proofs
   Max_Input_Length    : constant := 1_000_000;  --  1MB max manifest
   Max_String_Length   : constant := 100_000;    --  100KB max string value
   Max_Identifier_Len  : constant := 256;        --  Max identifier/key length

   ---------------------
   -- Token Types
   ---------------------

   type Token_Kind is
     (Tok_EOF,           --  End of input
      Tok_Newline,       --  Line break
      Tok_Equals,        --  =
      Tok_Comma,         --  ,
      Tok_Dot,           --  .
      Tok_Colon,         --  : (in hashes)
      Tok_Lbracket,      --  [
      Tok_Rbracket,      --  ]
      Tok_Lbrace,        --  {
      Tok_Rbrace,        --  }
      Tok_String,        --  "..." or """..."""
      Tok_Integer,       --  123 or -456
      Tok_Boolean,       --  true or false
      Tok_Date,          --  2024-12-07
      Tok_DateTime,      --  2024-12-07T10:30:00Z
      Tok_Identifier,    --  key names, section names
      Tok_Comment,       --  # ...
      Tok_Error);        --  Lexer error

   type Token is record
      Kind   : Token_Kind;
      Value  : Unbounded_String;  --  Actual token text
      Line   : Positive;
      Column : Positive;
   end record;

   ---------------------
   -- Lexer State
   ---------------------

   type Lexer_State is record
      Input    : Unbounded_String;
      Position : Natural;           --  Current position in input
      Line     : Positive;          --  Current line number
      Column   : Positive;          --  Current column number
      Length   : Natural;           --  Cached input length
   end record;

   ---------------------
   -- Lexer Functions
   ---------------------

   --  Initialize lexer with input string
   function Init (Content : String) return Lexer_State
      with Pre => Content'Length <= Max_Input_Length;

   --  Get the next token
   procedure Next_Token
      (State : in out Lexer_State;
       Tok   : out Token)
      with Global => null;

   --  Peek at current character without consuming
   function Peek_Char (State : Lexer_State) return Character
      with Global => null;

   --  Check if at end of input
   function At_End (State : Lexer_State) return Boolean
      with Global => null,
           Post => At_End'Result = (State.Position > State.Length);

   ---------------------
   -- Helper Functions
   ---------------------

   --  Check if character is valid identifier start (a-z, A-Z, _)
   function Is_Ident_Start (C : Character) return Boolean
      with Global => null;

   --  Check if character is valid identifier continuation
   function Is_Ident_Char (C : Character) return Boolean
      with Global => null;

   --  Check if character is digit
   function Is_Digit (C : Character) return Boolean
      with Global => null;

   --  Check if character is hex digit
   function Is_Hex_Digit (C : Character) return Boolean
      with Global => null;

   --  Check if character is whitespace (space or tab, not newline)
   function Is_Whitespace (C : Character) return Boolean
      with Global => null;

end Cerro_CTP_Lexer;
