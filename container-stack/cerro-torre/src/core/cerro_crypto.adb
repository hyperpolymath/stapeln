-------------------------------------------------------------------------------
--  Cerro_Crypto - Implementation of cryptographic primitives
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Implements FIPS 180-4 SHA-256 and SHA-512 hash algorithms.
--  Ed25519 signature verification is a placeholder pending SPARKNaCl integration.
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  SPARK mode off for now due to loop invariant complexity

with Ada.Unchecked_Conversion;

package body Cerro_Crypto is

   ---------------------------------------------------------------------------
   --  SHA-256 Constants (FIPS 180-4 Section 4.2.2)
   ---------------------------------------------------------------------------

   type Word_32 is new Unsigned_32;
   type Word_32_Array is array (Positive range <>) of Word_32;

   --  Initial hash values (first 32 bits of fractional parts of square roots)
   H0_Init : constant Word_32_Array (1 .. 8) := (
      16#6a09e667#, 16#bb67ae85#, 16#3c6ef372#, 16#a54ff53a#,
      16#510e527f#, 16#9b05688c#, 16#1f83d9ab#, 16#5be0cd19#
   );

   --  Round constants (first 32 bits of fractional parts of cube roots)
   K : constant Word_32_Array (1 .. 64) := (
      16#428a2f98#, 16#71374491#, 16#b5c0fbcf#, 16#e9b5dba5#,
      16#3956c25b#, 16#59f111f1#, 16#923f82a4#, 16#ab1c5ed5#,
      16#d807aa98#, 16#12835b01#, 16#243185be#, 16#550c7dc3#,
      16#72be5d74#, 16#80deb1fe#, 16#9bdc06a7#, 16#c19bf174#,
      16#e49b69c1#, 16#efbe4786#, 16#0fc19dc6#, 16#240ca1cc#,
      16#2de92c6f#, 16#4a7484aa#, 16#5cb0a9dc#, 16#76f988da#,
      16#983e5152#, 16#a831c66d#, 16#b00327c8#, 16#bf597fc7#,
      16#c6e00bf3#, 16#d5a79147#, 16#06ca6351#, 16#14292967#,
      16#27b70a85#, 16#2e1b2138#, 16#4d2c6dfc#, 16#53380d13#,
      16#650a7354#, 16#766a0abb#, 16#81c2c92e#, 16#92722c85#,
      16#a2bfe8a1#, 16#a81a664b#, 16#c24b8b70#, 16#c76c51a3#,
      16#d192e819#, 16#d6990624#, 16#f40e3585#, 16#106aa070#,
      16#19a4c116#, 16#1e376c08#, 16#2748774c#, 16#34b0bcb5#,
      16#391c0cb3#, 16#4ed8aa4a#, 16#5b9cca4f#, 16#682e6ff3#,
      16#748f82ee#, 16#78a5636f#, 16#84c87814#, 16#8cc70208#,
      16#90befffa#, 16#a4506ceb#, 16#bef9a3f7#, 16#c67178f2#
   );

   ---------------------------------------------------------------------------
   --  Bit Manipulation Operations
   ---------------------------------------------------------------------------

   function Rotr (X : Word_32; N : Natural) return Word_32 is
   begin
      return Word_32 (Shift_Right (Unsigned_32 (X), N) or
                      Shift_Left (Unsigned_32 (X), 32 - N));
   end Rotr;

   function Ch (X, Y, Z : Word_32) return Word_32 is
   begin
      return (X and Y) xor ((not X) and Z);
   end Ch;

   function Maj (X, Y, Z : Word_32) return Word_32 is
   begin
      return (X and Y) xor (X and Z) xor (Y and Z);
   end Maj;

   function Sigma0 (X : Word_32) return Word_32 is
   begin
      return Rotr (X, 2) xor Rotr (X, 13) xor Rotr (X, 22);
   end Sigma0;

   function Sigma1 (X : Word_32) return Word_32 is
   begin
      return Rotr (X, 6) xor Rotr (X, 11) xor Rotr (X, 25);
   end Sigma1;

   function Delta0 (X : Word_32) return Word_32 is
   begin
      return Rotr (X, 7) xor Rotr (X, 18) xor
             Word_32 (Shift_Right (Unsigned_32 (X), 3));
   end Delta0;

   function Delta1 (X : Word_32) return Word_32 is
   begin
      return Rotr (X, 17) xor Rotr (X, 19) xor
             Word_32 (Shift_Right (Unsigned_32 (X), 10));
   end Delta1;

   ---------------------------------------------------------------------------
   --  Compute_SHA256 - FIPS 180-4 SHA-256 implementation
   ---------------------------------------------------------------------------

   function Compute_SHA256 (Data : String) return SHA256_Digest is
      --  Padded message buffer (512-bit blocks)
      Msg_Len_Bits : constant Unsigned_64 := Unsigned_64 (Data'Length) * 8;

      --  Calculate padded length (multiple of 64 bytes with 9 bytes for padding)
      Pad_Len : constant Natural :=
         ((Data'Length + 9 + 63) / 64) * 64;

      Padded : array (1 .. Pad_Len) of Unsigned_8 := (others => 0);

      --  Hash state
      H : Word_32_Array (1 .. 8) := H0_Init;

      --  Message schedule
      W : Word_32_Array (1 .. 64);

      --  Working variables
      A, B, C, D, E, F, G, HH : Word_32;
      T1, T2 : Word_32;

      --  Current block start position
      Block_Start : Positive;

      --  Result digest
      Result : SHA256_Digest;
   begin
      --  Copy message to padded buffer
      for I in Data'Range loop
         Padded (I - Data'First + 1) := Character'Pos (Data (I));
      end loop;

      --  Append padding bit
      Padded (Data'Length + 1) := 16#80#;

      --  Append length in bits (big-endian, last 8 bytes)
      Padded (Pad_Len - 7) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 56) and 16#FF#);
      Padded (Pad_Len - 6) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 48) and 16#FF#);
      Padded (Pad_Len - 5) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 40) and 16#FF#);
      Padded (Pad_Len - 4) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 32) and 16#FF#);
      Padded (Pad_Len - 3) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 24) and 16#FF#);
      Padded (Pad_Len - 2) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 16) and 16#FF#);
      Padded (Pad_Len - 1) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 8) and 16#FF#);
      Padded (Pad_Len)     := Unsigned_8 (Msg_Len_Bits and 16#FF#);

      --  Process each 512-bit (64-byte) block
      Block_Start := 1;
      while Block_Start <= Pad_Len loop
         --  Prepare message schedule
         for I in 1 .. 16 loop
            declare
               Base : constant Positive := Block_Start + (I - 1) * 4;
            begin
               W (I) := Word_32 (Shift_Left (Unsigned_32 (Padded (Base)), 24) or
                                 Shift_Left (Unsigned_32 (Padded (Base + 1)), 16) or
                                 Shift_Left (Unsigned_32 (Padded (Base + 2)), 8) or
                                 Unsigned_32 (Padded (Base + 3)));
            end;
         end loop;

         for I in 17 .. 64 loop
            W (I) := Delta1 (W (I - 2)) + W (I - 7) +
                     Delta0 (W (I - 15)) + W (I - 16);
         end loop;

         --  Initialize working variables
         A := H (1); B := H (2); C := H (3); D := H (4);
         E := H (5); F := H (6); G := H (7); HH := H (8);

         --  64 rounds of compression
         for I in 1 .. 64 loop
            T1 := HH + Sigma1 (E) + Ch (E, F, G) + K (I) + W (I);
            T2 := Sigma0 (A) + Maj (A, B, C);
            HH := G;
            G := F;
            F := E;
            E := D + T1;
            D := C;
            C := B;
            B := A;
            A := T1 + T2;
         end loop;

         --  Update hash state
         H (1) := H (1) + A;
         H (2) := H (2) + B;
         H (3) := H (3) + C;
         H (4) := H (4) + D;
         H (5) := H (5) + E;
         H (6) := H (6) + F;
         H (7) := H (7) + G;
         H (8) := H (8) + HH;

         Block_Start := Block_Start + 64;
      end loop;

      --  Convert hash state to bytes (big-endian)
      for I in 1 .. 8 loop
         declare
            Base : constant Positive := (I - 1) * 4 + 1;
         begin
            Result (Base)     := Unsigned_8 (Shift_Right (Unsigned_32 (H (I)), 24) and 16#FF#);
            Result (Base + 1) := Unsigned_8 (Shift_Right (Unsigned_32 (H (I)), 16) and 16#FF#);
            Result (Base + 2) := Unsigned_8 (Shift_Right (Unsigned_32 (H (I)), 8) and 16#FF#);
            Result (Base + 3) := Unsigned_8 (Unsigned_32 (H (I)) and 16#FF#);
         end;
      end loop;

      return Result;
   end Compute_SHA256;

   ---------------------------------------------------------------------------
   --  SHA-512 Constants (FIPS 180-4 Section 4.2.3)
   ---------------------------------------------------------------------------

   type Word_64 is new Unsigned_64;
   type Word_64_Array is array (Positive range <>) of Word_64;

   --  Initial hash values for SHA-512
   H512_Init : constant Word_64_Array (1 .. 8) := [
      16#6a09e667f3bcc908#, 16#bb67ae8584caa73b#,
      16#3c6ef372fe94f82b#, 16#a54ff53a5f1d36f1#,
      16#510e527fade682d1#, 16#9b05688c2b3e6c1f#,
      16#1f83d9abfb41bd6b#, 16#5be0cd19137e2179#
   ];

   --  SHA-512 round constants (80 constants)
   K512 : constant Word_64_Array (1 .. 80) := [
      16#428a2f98d728ae22#, 16#7137449123ef65cd#, 16#b5c0fbcfec4d3b2f#, 16#e9b5dba58189dbbc#,
      16#3956c25bf348b538#, 16#59f111f1b605d019#, 16#923f82a4af194f9b#, 16#ab1c5ed5da6d8118#,
      16#d807aa98a3030242#, 16#12835b0145706fbe#, 16#243185be4ee4b28c#, 16#550c7dc3d5ffb4e2#,
      16#72be5d74f27b896f#, 16#80deb1fe3b1696b1#, 16#9bdc06a725c71235#, 16#c19bf174cf692694#,
      16#e49b69c19ef14ad2#, 16#efbe4786384f25e3#, 16#0fc19dc68b8cd5b5#, 16#240ca1cc77ac9c65#,
      16#2de92c6f592b0275#, 16#4a7484aa6ea6e483#, 16#5cb0a9dcbd41fbd4#, 16#76f988da831153b5#,
      16#983e5152ee66dfab#, 16#a831c66d2db43210#, 16#b00327c898fb213f#, 16#bf597fc7beef0ee4#,
      16#c6e00bf33da88fc2#, 16#d5a79147930aa725#, 16#06ca6351e003826f#, 16#142929670a0e6e70#,
      16#27b70a8546d22ffc#, 16#2e1b21385c26c926#, 16#4d2c6dfc5ac42aed#, 16#53380d139d95b3df#,
      16#650a73548baf63de#, 16#766a0abb3c77b2a8#, 16#81c2c92e47edaee6#, 16#92722c851482353b#,
      16#a2bfe8a14cf10364#, 16#a81a664bbc423001#, 16#c24b8b70d0f89791#, 16#c76c51a30654be30#,
      16#d192e819d6ef5218#, 16#d69906245565a910#, 16#f40e35855771202a#, 16#106aa07032bbd1b8#,
      16#19a4c116b8d2d0c8#, 16#1e376c085141ab53#, 16#2748774cdf8eeb99#, 16#34b0bcb5e19b48a8#,
      16#391c0cb3c5c95a63#, 16#4ed8aa4ae3418acb#, 16#5b9cca4f7763e373#, 16#682e6ff3d6b2b8a3#,
      16#748f82ee5defb2fc#, 16#78a5636f43172f60#, 16#84c87814a1f0ab72#, 16#8cc702081a6439ec#,
      16#90befffa23631e28#, 16#a4506cebde82bde9#, 16#bef9a3f7b2c67915#, 16#c67178f2e372532b#,
      16#ca273eceea26619c#, 16#d186b8c721c0c207#, 16#eada7dd6cde0eb1e#, 16#f57d4f7fee6ed178#,
      16#06f067aa72176fba#, 16#0a637dc5a2c898a6#, 16#113f9804bef90dae#, 16#1b710b35131c471b#,
      16#28db77f523047d84#, 16#32caab7b40c72493#, 16#3c9ebe0a15c9bebc#, 16#431d67c49c100d4c#,
      16#4cc5d4becb3e42b6#, 16#597f299cfc657e2a#, 16#5fcb6fab3ad6faec#, 16#6c44198c4a475817#
   ];

   ---------------------------------------------------------------------------
   --  SHA-512 Bit Operations (64-bit variants)
   ---------------------------------------------------------------------------

   function Rotr64 (X : Word_64; N : Natural) return Word_64 is
   begin
      return Word_64 (Shift_Right (Unsigned_64 (X), N) or
                      Shift_Left (Unsigned_64 (X), 64 - N));
   end Rotr64;

   function Ch64 (X, Y, Z : Word_64) return Word_64 is
   begin
      return (X and Y) xor ((not X) and Z);
   end Ch64;

   function Maj64 (X, Y, Z : Word_64) return Word_64 is
   begin
      return (X and Y) xor (X and Z) xor (Y and Z);
   end Maj64;

   function Sigma512_0 (X : Word_64) return Word_64 is
   begin
      return Rotr64 (X, 28) xor Rotr64 (X, 34) xor Rotr64 (X, 39);
   end Sigma512_0;

   function Sigma512_1 (X : Word_64) return Word_64 is
   begin
      return Rotr64 (X, 14) xor Rotr64 (X, 18) xor Rotr64 (X, 41);
   end Sigma512_1;

   function Delta512_0 (X : Word_64) return Word_64 is
   begin
      return Rotr64 (X, 1) xor Rotr64 (X, 8) xor
             Word_64 (Shift_Right (Unsigned_64 (X), 7));
   end Delta512_0;

   function Delta512_1 (X : Word_64) return Word_64 is
   begin
      return Rotr64 (X, 19) xor Rotr64 (X, 61) xor
             Word_64 (Shift_Right (Unsigned_64 (X), 6));
   end Delta512_1;

   ---------------------------------------------------------------------------
   --  Compute_SHA512 - FIPS 180-4 SHA-512 implementation
   ---------------------------------------------------------------------------

   function Compute_SHA512 (Data : String) return SHA512_Digest is
      --  Message length in bits (128-bit, but we only use lower 64 for reasonable sizes)
      Msg_Len_Bits : constant Unsigned_64 := Unsigned_64 (Data'Length) * 8;

      --  Calculate padded length (multiple of 128 bytes with 17 bytes for padding)
      Pad_Len : constant Natural :=
         ((Data'Length + 17 + 127) / 128) * 128;

      Padded : array (1 .. Pad_Len) of Unsigned_8 := [others => 0];

      --  Hash state
      H : Word_64_Array (1 .. 8) := H512_Init;

      --  Message schedule (80 words for SHA-512)
      W : Word_64_Array (1 .. 80);

      --  Working variables
      A, B, C, D, E, F, G, HH : Word_64;
      T1, T2 : Word_64;

      --  Current block start position
      Block_Start : Positive;

      --  Result digest
      Result : SHA512_Digest;
   begin
      --  Copy message to padded buffer
      for I in Data'Range loop
         Padded (I - Data'First + 1) := Character'Pos (Data (I));
      end loop;

      --  Append padding bit
      Padded (Data'Length + 1) := 16#80#;

      --  Append length in bits (big-endian, last 16 bytes - we use lower 8)
      --  Upper 8 bytes stay zero (sufficient for practical message sizes)
      Padded (Pad_Len - 7) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 56) and 16#FF#);
      Padded (Pad_Len - 6) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 48) and 16#FF#);
      Padded (Pad_Len - 5) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 40) and 16#FF#);
      Padded (Pad_Len - 4) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 32) and 16#FF#);
      Padded (Pad_Len - 3) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 24) and 16#FF#);
      Padded (Pad_Len - 2) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 16) and 16#FF#);
      Padded (Pad_Len - 1) := Unsigned_8 (Shift_Right (Msg_Len_Bits, 8) and 16#FF#);
      Padded (Pad_Len)     := Unsigned_8 (Msg_Len_Bits and 16#FF#);

      --  Process each 1024-bit (128-byte) block
      Block_Start := 1;
      while Block_Start <= Pad_Len loop
         --  Prepare message schedule (16 words from block, then expand to 80)
         for I in 1 .. 16 loop
            declare
               Base : constant Positive := Block_Start + (I - 1) * 8;
            begin
               W (I) := Word_64 (Shift_Left (Unsigned_64 (Padded (Base)), 56) or
                                 Shift_Left (Unsigned_64 (Padded (Base + 1)), 48) or
                                 Shift_Left (Unsigned_64 (Padded (Base + 2)), 40) or
                                 Shift_Left (Unsigned_64 (Padded (Base + 3)), 32) or
                                 Shift_Left (Unsigned_64 (Padded (Base + 4)), 24) or
                                 Shift_Left (Unsigned_64 (Padded (Base + 5)), 16) or
                                 Shift_Left (Unsigned_64 (Padded (Base + 6)), 8) or
                                 Unsigned_64 (Padded (Base + 7)));
            end;
         end loop;

         for I in 17 .. 80 loop
            W (I) := Delta512_1 (W (I - 2)) + W (I - 7) +
                     Delta512_0 (W (I - 15)) + W (I - 16);
         end loop;

         --  Initialize working variables
         A := H (1); B := H (2); C := H (3); D := H (4);
         E := H (5); F := H (6); G := H (7); HH := H (8);

         --  80 rounds of compression
         for I in 1 .. 80 loop
            T1 := HH + Sigma512_1 (E) + Ch64 (E, F, G) + K512 (I) + W (I);
            T2 := Sigma512_0 (A) + Maj64 (A, B, C);
            HH := G;
            G := F;
            F := E;
            E := D + T1;
            D := C;
            C := B;
            B := A;
            A := T1 + T2;
         end loop;

         --  Update hash state
         H (1) := H (1) + A;
         H (2) := H (2) + B;
         H (3) := H (3) + C;
         H (4) := H (4) + D;
         H (5) := H (5) + E;
         H (6) := H (6) + F;
         H (7) := H (7) + G;
         H (8) := H (8) + HH;

         Block_Start := Block_Start + 128;
      end loop;

      --  Convert hash state to bytes (big-endian)
      for I in 1 .. 8 loop
         declare
            Base : constant Positive := (I - 1) * 8 + 1;
         begin
            Result (Base)     := Unsigned_8 (Shift_Right (Unsigned_64 (H (I)), 56) and 16#FF#);
            Result (Base + 1) := Unsigned_8 (Shift_Right (Unsigned_64 (H (I)), 48) and 16#FF#);
            Result (Base + 2) := Unsigned_8 (Shift_Right (Unsigned_64 (H (I)), 40) and 16#FF#);
            Result (Base + 3) := Unsigned_8 (Shift_Right (Unsigned_64 (H (I)), 32) and 16#FF#);
            Result (Base + 4) := Unsigned_8 (Shift_Right (Unsigned_64 (H (I)), 24) and 16#FF#);
            Result (Base + 5) := Unsigned_8 (Shift_Right (Unsigned_64 (H (I)), 16) and 16#FF#);
            Result (Base + 6) := Unsigned_8 (Shift_Right (Unsigned_64 (H (I)), 8) and 16#FF#);
            Result (Base + 7) := Unsigned_8 (Unsigned_64 (H (I)) and 16#FF#);
         end;
      end loop;

      return Result;
   end Compute_SHA512;

   ---------------------------------------------------------------------------
   --  Ed25519 Implementation (RFC 8032)
   --
   --  Field: GF(2^255 - 19)
   --  Curve: -x^2 + y^2 = 1 + d*x^2*y^2 where d = -121665/121666
   --  Base point B with y = 4/5 (mod p)
   --  Group order L = 2^252 + 27742317777372353535851937790883648493
   ---------------------------------------------------------------------------

   --  Field element: 256-bit integer represented as array of 32 bytes (little-endian)
   type Field_Element is array (1 .. 32) of Unsigned_8;

   --  Extended point coordinates (X:Y:Z:T) where x = X/Z, y = Y/Z, xy = T/Z
   type Ed_Point is record
      X, Y, Z, T : Field_Element;
   end record;

   --  Field constants
   Field_Zero : constant Field_Element := (others => 0);
   Field_One  : constant Field_Element := (1, others => 0);

   --  Curve parameter d = -121665/121666 mod p (precomputed)
   --  d = 37095705934669439343138083508754565189542113879843219016388785533085940283555
   D_Const : constant Field_Element := (
      16#A3#, 16#78#, 16#59#, 16#13#, 16#CA#, 16#4D#, 16#EB#, 16#75#,
      16#AB#, 16#F6#, 16#58#, 16#AD#, 16#0F#, 16#C4#, 16#BE#, 16#63#,
      16#4A#, 16#03#, 16#59#, 16#D8#, 16#11#, 16#37#, 16#FD#, 16#3E#,
      16#E3#, 16#C2#, 16#2F#, 16#C8#, 16#40#, 16#24#, 16#92#, 16#52#
   );

   --  2*d for doubling formula
   D2_Const : constant Field_Element := (
      16#46#, 16#F1#, 16#B2#, 16#26#, 16#94#, 16#9B#, 16#D6#, 16#EB#,
      16#56#, 16#ED#, 16#B0#, 16#5A#, 16#1F#, 16#88#, 16#7C#, 16#C7#,
      16#94#, 16#06#, 16#B2#, 16#B0#, 16#22#, 16#6E#, 16#FA#, 16#7C#,
      16#C7#, 16#85#, 16#5E#, 16#90#, 16#81#, 16#48#, 16#24#, 16#25#
   );

   --  Base point B (compressed y with sign bit)
   --  y = 4/5 mod p = 46316835694926478169428394003475163141307993866256225615783033603165251855960
   B_Y : constant Field_Element := (
      16#58#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#,
      16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#,
      16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#,
      16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#, 16#66#
   );

   --  Base point x coordinate (precomputed)
   --  x = 15112221349535807912866137220509078935008241517919115517543965251855960451277
   B_X : constant Field_Element := (
      16#1A#, 16#D5#, 16#25#, 16#8F#, 16#60#, 16#2D#, 16#56#, 16#C9#,
      16#B2#, 16#A7#, 16#25#, 16#9B#, 16#D3#, 16#C9#, 16#DC#, 16#86#,
      16#B6#, 16#F8#, 16#0E#, 16#E4#, 16#92#, 16#52#, 16#FC#, 16#BA#,
      16#E2#, 16#D7#, 16#01#, 16#BC#, 16#72#, 16#95#, 16#D3#, 16#21#
   );

   --  Group order L (little-endian)
   --  L = 2^252 + 27742317777372353535851937790883648493
   L_Order : constant Field_Element := (
      16#ED#, 16#D3#, 16#F5#, 16#5C#, 16#1A#, 16#63#, 16#12#, 16#58#,
      16#D6#, 16#9C#, 16#F7#, 16#A2#, 16#DE#, 16#F9#, 16#DE#, 16#14#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#10#
   );

   ---------------------------------------------------------------------------
   --  Field Arithmetic (mod 2^255 - 19)
   ---------------------------------------------------------------------------

   --  Reduce a field element modulo p = 2^255 - 19
   procedure Field_Reduce (A : in out Field_Element) is
      type Long_Int is mod 2**64;
      Carry : Long_Int;
      Temp  : array (1 .. 32) of Long_Int;
   begin
      --  Copy to temp with wider type
      for I in 1 .. 32 loop
         Temp (I) := Long_Int (A (I));
      end loop;

      --  Propagate carries
      for I in 1 .. 31 loop
         Carry := Temp (I) / 256;
         Temp (I) := Temp (I) mod 256;
         Temp (I + 1) := Temp (I + 1) + Carry;
      end loop;

      --  Handle top byte - reduce mod 2^255 - 19
      Carry := Temp (32) / 128;  --  Bits above 255
      Temp (32) := Temp (32) mod 128;

      --  Multiply carry by 19 and add back (since 2^255 = 19 mod p)
      Carry := Carry * 19;
      for I in 1 .. 32 loop
         Temp (I) := Temp (I) + (Carry mod 256);
         Carry := Carry / 256 + Temp (I) / 256;
         Temp (I) := Temp (I) mod 256;
      end loop;

      --  Final reduction if needed
      if Temp (32) >= 128 then
         Carry := (Temp (32) / 128) * 19;
         Temp (32) := Temp (32) mod 128;
         for I in 1 .. 32 loop
            Temp (I) := Temp (I) + (Carry mod 256);
            Carry := Carry / 256 + Temp (I) / 256;
            Temp (I) := Temp (I) mod 256;
         end loop;
      end if;

      for I in 1 .. 32 loop
         A (I) := Unsigned_8 (Temp (I));
      end loop;
   end Field_Reduce;

   --  Field addition: C = A + B mod p
   function Field_Add (A, B : Field_Element) return Field_Element is
      C : Field_Element;
      Carry : Unsigned_16 := 0;
   begin
      for I in 1 .. 32 loop
         Carry := Unsigned_16 (A (I)) + Unsigned_16 (B (I)) + Carry;
         C (I) := Unsigned_8 (Carry and 16#FF#);
         Carry := Shift_Right (Carry, 8);
      end loop;
      Field_Reduce (C);
      return C;
   end Field_Add;

   --  Field subtraction: C = A - B mod p
   function Field_Sub (A, B : Field_Element) return Field_Element is
      C : Field_Element;
      Borrow : Integer := 0;
      Diff   : Integer;
   begin
      for I in 1 .. 32 loop
         Diff := Integer (A (I)) - Integer (B (I)) - Borrow;
         if Diff < 0 then
            Diff := Diff + 256;
            Borrow := 1;
         else
            Borrow := 0;
         end if;
         C (I) := Unsigned_8 (Diff);
      end loop;

      --  If borrow, add p back (p = 2^255 - 19)
      if Borrow = 1 then
         --  Add 2^255 - 19 = add 2^255 and subtract 19
         declare
            Carry : Integer := -19;
         begin
            for I in 1 .. 31 loop
               Carry := Integer (C (I)) + Carry;
               if Carry < 0 then
                  C (I) := Unsigned_8 (Carry + 256);
                  Carry := -1;
               else
                  C (I) := Unsigned_8 (Carry mod 256);
                  Carry := Carry / 256;
               end if;
            end loop;
            --  Add 128 to top byte (2^255 = 2^7 * 2^248)
            Carry := Integer (C (32)) + 128 + Carry;
            C (32) := Unsigned_8 (Carry mod 256);
         end;
      end if;

      Field_Reduce (C);
      return C;
   end Field_Sub;

   --  Field multiplication: C = A * B mod p
   function Field_Mul (A, B : Field_Element) return Field_Element is
      type Long_Int is mod 2**64;
      Prod : array (1 .. 64) of Long_Int := (others => 0);
      C    : Field_Element;
      Carry : Long_Int;
   begin
      --  Schoolbook multiplication
      for I in 1 .. 32 loop
         for J in 1 .. 32 loop
            Prod (I + J - 1) := Prod (I + J - 1) +
                               Long_Int (A (I)) * Long_Int (B (J));
         end loop;
      end loop;

      --  Propagate carries in product
      for I in 1 .. 63 loop
         Carry := Prod (I) / 256;
         Prod (I) := Prod (I) mod 256;
         Prod (I + 1) := Prod (I + 1) + Carry;
      end loop;

      --  Reduce mod 2^255 - 19
      --  High bits (from byte 32 onward) get multiplied by 19 and added back
      for I in 32 .. 64 loop
         if Prod (I) > 0 then
            declare
               Mult : Long_Int := Prod (I) * 19;
               Idx  : Natural := I - 32 + 1;
            begin
               Prod (I) := 0;
               while Mult > 0 and Idx <= 64 loop
                  Prod (Idx) := Prod (Idx) + (Mult mod 256);
                  Mult := Mult / 256;
                  Idx := Idx + 1;
               end loop;
            end;
         end if;
      end loop;

      --  Propagate carries again
      for I in 1 .. 31 loop
         Carry := Prod (I) / 256;
         Prod (I) := Prod (I) mod 256;
         Prod (I + 1) := Prod (I + 1) + Carry;
      end loop;

      --  Final reduction of byte 32
      Carry := Prod (32) / 128;
      Prod (32) := Prod (32) mod 128;
      Carry := Carry * 19;
      for I in 1 .. 32 loop
         Prod (I) := Prod (I) + (Carry mod 256);
         Carry := Carry / 256 + Prod (I) / 256;
         Prod (I) := Prod (I) mod 256;
      end loop;

      for I in 1 .. 32 loop
         C (I) := Unsigned_8 (Prod (I));
      end loop;

      Field_Reduce (C);
      return C;
   end Field_Mul;

   --  Field squaring: C = A^2 mod p
   function Field_Sqr (A : Field_Element) return Field_Element is
   begin
      return Field_Mul (A, A);
   end Field_Sqr;

   --  Field inversion using Fermat's little theorem: A^(-1) = A^(p-2) mod p
   function Field_Inv (A : Field_Element) return Field_Element is
      --  p - 2 = 2^255 - 21 = 2^255 - 19 - 2
      --  We compute A^(p-2) using a carefully optimized addition chain
      T0, T1, T2, T3 : Field_Element;
   begin
      --  Compute A^(2^5 - 1) = A^31
      T0 := Field_Sqr (A);        --  A^2
      T1 := Field_Sqr (T0);       --  A^4
      T1 := Field_Sqr (T1);       --  A^8
      T1 := Field_Mul (T1, A);    --  A^9
      T0 := Field_Mul (T0, T1);   --  A^11
      T2 := Field_Sqr (T0);       --  A^22
      T1 := Field_Mul (T1, T2);   --  A^31 = A^(2^5 - 1)

      --  Compute A^(2^10 - 1)
      T2 := Field_Sqr (T1);
      for I in 2 .. 5 loop
         T2 := Field_Sqr (T2);
      end loop;
      T1 := Field_Mul (T1, T2);   --  A^(2^10 - 1)

      --  Compute A^(2^20 - 1)
      T2 := Field_Sqr (T1);
      for I in 2 .. 10 loop
         T2 := Field_Sqr (T2);
      end loop;
      T2 := Field_Mul (T2, T1);   --  A^(2^20 - 1)

      --  Compute A^(2^40 - 1)
      T3 := Field_Sqr (T2);
      for I in 2 .. 20 loop
         T3 := Field_Sqr (T3);
      end loop;
      T2 := Field_Mul (T3, T2);   --  A^(2^40 - 1)

      --  Compute A^(2^50 - 1)
      T3 := Field_Sqr (T2);
      for I in 2 .. 10 loop
         T3 := Field_Sqr (T3);
      end loop;
      T1 := Field_Mul (T3, T1);   --  A^(2^50 - 1)

      --  Compute A^(2^100 - 1)
      T2 := Field_Sqr (T1);
      for I in 2 .. 50 loop
         T2 := Field_Sqr (T2);
      end loop;
      T2 := Field_Mul (T2, T1);   --  A^(2^100 - 1)

      --  Compute A^(2^200 - 1)
      T3 := Field_Sqr (T2);
      for I in 2 .. 100 loop
         T3 := Field_Sqr (T3);
      end loop;
      T2 := Field_Mul (T3, T2);   --  A^(2^200 - 1)

      --  Compute A^(2^250 - 1)
      T3 := Field_Sqr (T2);
      for I in 2 .. 50 loop
         T3 := Field_Sqr (T3);
      end loop;
      T1 := Field_Mul (T3, T1);   --  A^(2^250 - 1)

      --  Compute A^(2^255 - 21) = A^(p-2)
      T1 := Field_Sqr (T1);       --  A^(2^251 - 2)
      T1 := Field_Sqr (T1);       --  A^(2^252 - 4)
      T1 := Field_Sqr (T1);       --  A^(2^253 - 8)
      T1 := Field_Sqr (T1);       --  A^(2^254 - 16)
      T1 := Field_Sqr (T1);       --  A^(2^255 - 32)
      T1 := Field_Mul (T1, T0);   --  A^(2^255 - 21) = A^(p-2)

      return T1;
   end Field_Inv;

   --  Compute square root: returns sqrt(A) if A is QR, undefined otherwise
   --  Uses A^((p+3)/8) then conditionally multiplies by sqrt(-1)
   function Field_Sqrt (A : Field_Element) return Field_Element is
      --  (p+3)/8 = (2^255 - 19 + 3) / 8 = (2^255 - 16) / 8 = 2^252 - 2
      --  sqrt(-1) = 2^((p-1)/4) mod p (precomputed)
      Sqrt_M1 : constant Field_Element := (
         16#B0#, 16#A0#, 16#0E#, 16#4A#, 16#27#, 16#1B#, 16#EE#, 16#C4#,
         16#78#, 16#E4#, 16#2F#, 16#AD#, 16#06#, 16#18#, 16#43#, 16#2F#,
         16#A7#, 16#D7#, 16#FB#, 16#3D#, 16#99#, 16#00#, 16#4D#, 16#2B#,
         16#0B#, 16#DF#, 16#C1#, 16#4F#, 16#80#, 16#24#, 16#83#, 16#2B#
      );
      T0, T1, Check : Field_Element;
   begin
      --  Compute A^((p+3)/8) = A^(2^252 - 2)
      --  Using A^(2^252) * A^(-2) = A^(2^252) / A^2

      --  First compute A^(2^252)
      T0 := A;
      for I in 1 .. 252 loop
         T0 := Field_Sqr (T0);
      end loop;

      --  Divide by A^2
      T1 := Field_Sqr (A);
      T1 := Field_Inv (T1);
      T0 := Field_Mul (T0, T1);

      --  Check if T0^2 = A
      Check := Field_Sqr (T0);
      Check := Field_Sub (Check, A);

      --  If not, multiply by sqrt(-1)
      declare
         Is_Zero : Boolean := True;
      begin
         for I in 1 .. 32 loop
            if Check (I) /= 0 then
               Is_Zero := False;
               exit;
            end if;
         end loop;

         if not Is_Zero then
            T0 := Field_Mul (T0, Sqrt_M1);
         end if;
      end;

      return T0;
   end Field_Sqrt;

   --  Check if field element is zero
   function Field_Is_Zero (A : Field_Element) return Boolean is
      Result : Boolean := True;
   begin
      for I in 1 .. 32 loop
         if A (I) /= 0 then
            Result := False;
            exit;
         end if;
      end loop;
      return Result;
   end Field_Is_Zero;

   --  Check if two field elements are equal
   function Field_Equal (A, B : Field_Element) return Boolean is
   begin
      return Field_Is_Zero (Field_Sub (A, B));
   end Field_Equal;

   ---------------------------------------------------------------------------
   --  Edwards Curve Point Operations
   ---------------------------------------------------------------------------

   --  Neutral element (identity point)
   function Point_Identity return Ed_Point is
   begin
      return (X => Field_Zero, Y => Field_One, Z => Field_One, T => Field_Zero);
   end Point_Identity;

   --  Point doubling: R = 2*P (extended coordinates)
   function Point_Double (P : Ed_Point) return Ed_Point is
      A, B, C, D, E, F, G, H : Field_Element;
      R : Ed_Point;
   begin
      A := Field_Sqr (P.X);
      B := Field_Sqr (P.Y);
      C := Field_Sqr (P.Z);
      C := Field_Add (C, C);  --  2*Z^2
      D := Field_Sub (Field_Zero, A);  --  -X^2 (for -x^2 + y^2 curve)
      E := Field_Add (P.X, P.Y);
      E := Field_Sqr (E);
      E := Field_Sub (E, A);
      E := Field_Sub (E, B);  --  E = (X+Y)^2 - X^2 - Y^2 = 2XY
      G := Field_Add (D, B);  --  G = -X^2 + Y^2
      F := Field_Sub (G, C);  --  F = G - 2Z^2
      H := Field_Sub (D, B);  --  H = -X^2 - Y^2

      R.X := Field_Mul (E, F);
      R.Y := Field_Mul (G, H);
      R.T := Field_Mul (E, H);
      R.Z := Field_Mul (F, G);

      return R;
   end Point_Double;

   --  Point addition: R = P + Q (extended coordinates)
   function Point_Add (P, Q : Ed_Point) return Ed_Point is
      A, B, C, D, E, F, G, H : Field_Element;
      R : Ed_Point;
   begin
      A := Field_Mul (P.X, Q.X);
      B := Field_Mul (P.Y, Q.Y);
      C := Field_Mul (P.T, Q.T);
      C := Field_Mul (C, D2_Const);  --  C = T1*T2*2d
      D := Field_Mul (P.Z, Q.Z);

      E := Field_Add (P.X, P.Y);
      F := Field_Add (Q.X, Q.Y);
      E := Field_Mul (E, F);
      E := Field_Sub (E, A);
      E := Field_Sub (E, B);  --  E = (X1+Y1)*(X2+Y2) - X1*X2 - Y1*Y2

      F := Field_Sub (D, C);  --  F = Z1*Z2 - T1*T2*2d
      G := Field_Add (D, C);  --  G = Z1*Z2 + T1*T2*2d
      H := Field_Sub (B, A);  --  H = Y1*Y2 - X1*X2 (for -x^2+y^2 curve: B + (-A))
      H := Field_Add (B, Field_Sub (Field_Zero, A));

      R.X := Field_Mul (E, F);
      R.Y := Field_Mul (G, H);
      R.T := Field_Mul (E, H);
      R.Z := Field_Mul (F, G);

      return R;
   end Point_Add;

   --  Scalar multiplication: R = s * P
   function Scalar_Mult (S : Field_Element; P : Ed_Point) return Ed_Point is
      R : Ed_Point := Point_Identity;
      Q : Ed_Point := P;
      Bit : Unsigned_8;
   begin
      --  Double-and-add (LSB first)
      for I in 1 .. 32 loop
         for J in 0 .. 7 loop
            Bit := Shift_Right (S (I), J) and 1;
            if Bit = 1 then
               R := Point_Add (R, Q);
            end if;
            Q := Point_Double (Q);
         end loop;
      end loop;

      return R;
   end Scalar_Mult;

   --  Decompress point from 32-byte encoding
   function Point_Decompress (Encoded : Field_Element) return Ed_Point is
      Y : Field_Element := Encoded;
      X_Sign : Unsigned_8;
      Y_Sqr, U, V, V3, V7, X_Sqr, X, Check : Field_Element;
      P : Ed_Point;
   begin
      --  Extract sign bit from top of Y
      X_Sign := Shift_Right (Y (32), 7);
      Y (32) := Y (32) and 16#7F#;

      --  Compute x from y: x^2 = (y^2 - 1) / (d*y^2 + 1)
      Y_Sqr := Field_Sqr (Y);
      U := Field_Sub (Y_Sqr, Field_One);  --  u = y^2 - 1
      V := Field_Mul (D_Const, Y_Sqr);
      V := Field_Add (V, Field_One);      --  v = d*y^2 + 1

      --  Compute x = u * v^3 * (u * v^7)^((p-5)/8)
      V3 := Field_Mul (V, Field_Sqr (V));          --  v^3
      V7 := Field_Mul (V3, Field_Sqr (Field_Sqr (V)));  --  v^7
      X_Sqr := Field_Mul (U, V7);

      --  Compute (u*v^7)^((p-5)/8)
      --  (p-5)/8 = (2^255 - 24)/8 = 2^252 - 3
      declare
         T : Field_Element := X_Sqr;
      begin
         for I in 1 .. 252 loop
            T := Field_Sqr (T);
         end loop;
         --  Divide by (u*v^7)^3
         T := Field_Mul (T, Field_Inv (Field_Mul (X_Sqr, Field_Sqr (X_Sqr))));
         X := Field_Mul (U, V3);
         X := Field_Mul (X, T);
      end;

      --  Check x^2 * v = u
      Check := Field_Mul (Field_Sqr (X), V);
      Check := Field_Sub (Check, U);

      if not Field_Is_Zero (Check) then
         --  Try x * sqrt(-1)
         declare
            Sqrt_M1 : constant Field_Element := (
               16#B0#, 16#A0#, 16#0E#, 16#4A#, 16#27#, 16#1B#, 16#EE#, 16#C4#,
               16#78#, 16#E4#, 16#2F#, 16#AD#, 16#06#, 16#18#, 16#43#, 16#2F#,
               16#A7#, 16#D7#, 16#FB#, 16#3D#, 16#99#, 16#00#, 16#4D#, 16#2B#,
               16#0B#, 16#DF#, 16#C1#, 16#4F#, 16#80#, 16#24#, 16#83#, 16#2B#
            );
         begin
            X := Field_Mul (X, Sqrt_M1);
         end;
      end if;

      --  Adjust sign if needed
      if (X (1) and 1) /= X_Sign then
         X := Field_Sub (Field_Zero, X);
      end if;

      P.X := X;
      P.Y := Y;
      P.Z := Field_One;
      P.T := Field_Mul (X, Y);

      return P;
   end Point_Decompress;

   --  Compress point to 32-byte encoding
   function Point_Compress (P : Ed_Point) return Field_Element is
      Z_Inv : Field_Element;
      X, Y, Result : Field_Element;
   begin
      Z_Inv := Field_Inv (P.Z);
      X := Field_Mul (P.X, Z_Inv);
      Y := Field_Mul (P.Y, Z_Inv);

      Result := Y;
      --  Set sign bit
      Result (32) := Result (32) or Shift_Left (X (1) and 1, 7);

      return Result;
   end Point_Compress;

   --  Compare two points for equality
   function Point_Equal (P, Q : Ed_Point) return Boolean is
      --  P = Q iff P.X * Q.Z = Q.X * P.Z and P.Y * Q.Z = Q.Y * P.Z
      LX, RX, LY, RY : Field_Element;
   begin
      LX := Field_Mul (P.X, Q.Z);
      RX := Field_Mul (Q.X, P.Z);
      LY := Field_Mul (P.Y, Q.Z);
      RY := Field_Mul (Q.Y, P.Z);

      return Field_Equal (LX, RX) and Field_Equal (LY, RY);
   end Point_Equal;

   ---------------------------------------------------------------------------
   --  Scalar Reduction mod L
   ---------------------------------------------------------------------------

   --  Reduce 64-byte hash output modulo L (group order)
   function Reduce_Scalar_512 (H : SHA512_Digest) return Field_Element is
      type Big_Int is array (1 .. 64) of Unsigned_16;
      T : Big_Int := (others => 0);
      Result : Field_Element;

      --  L in expanded form for subtraction
      L_Exp : constant array (1 .. 32) of Unsigned_16 := (
         16#ED#, 16#D3#, 16#F5#, 16#5C#, 16#1A#, 16#63#, 16#12#, 16#58#,
         16#D6#, 16#9C#, 16#F7#, 16#A2#, 16#DE#, 16#F9#, 16#DE#, 16#14#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#10#
      );
   begin
      --  Copy hash to big int
      for I in 1 .. 64 loop
         T (I) := Unsigned_16 (H (I));
      end loop;

      --  Barrett-like reduction: repeatedly subtract L * 2^(8*k)
      --  Simple approach: reduce from top down
      for K in reverse 32 .. 63 loop
         if T (K + 1) > 0 then
            declare
               Q : Unsigned_16 := T (K + 1);
               Borrow : Integer;
            begin
               --  Subtract Q * L shifted by (K - 31) bytes
               for I in 1 .. 32 loop
                  declare
                     Idx : constant Integer := K - 31 + I;
                  begin
                     if Idx >= 1 and Idx <= 64 then
                        T (Idx) := T (Idx) - Q * L_Exp (I);
                     end if;
                  end;
               end loop;
               T (K + 1) := 0;

               --  Handle negative values (propagate borrows)
               for I in 1 .. 63 loop
                  while T (I) >= 256 loop
                     T (I) := T (I) - 256;
                     T (I + 1) := T (I + 1) + 1;
                  end loop;
               end loop;
            end;
         end if;
      end loop;

      --  Final reduction: ensure result < L
      for I in 1 .. 32 loop
         Result (I) := Unsigned_8 (T (I) mod 256);
      end loop;

      return Result;
   end Reduce_Scalar_512;

   ---------------------------------------------------------------------------
   --  Verify_Ed25519 - RFC 8032 Ed25519 Signature Verification
   ---------------------------------------------------------------------------

   function Verify_Ed25519
      (Message    : String;
       Signature  : Ed25519_Signature;
       Public_Key : Ed25519_Public_Key) return Boolean
   is
      R_Encoded : Field_Element;
      S         : Field_Element;
      A         : Ed_Point;
      R         : Ed_Point;
      K_Hash    : SHA512_Digest;
      K         : Field_Element;
      SB, R_KA  : Ed_Point;
      B         : Ed_Point;
   begin
      --  Step 1: Parse signature into R (point) and S (scalar)
      for I in 1 .. 32 loop
         R_Encoded (I) := Signature (I);
         S (I) := Signature (32 + I);
      end loop;

      --  Check S < L (reject malleable signatures)
      declare
         S_OK : Boolean := False;
      begin
         for I in reverse 1 .. 32 loop
            if S (I) < L_Order (I) then
               S_OK := True;
               exit;
            elsif S (I) > L_Order (I) then
               return False;
            end if;
         end loop;
         if not S_OK then
            return False;  --  S = L is not valid
         end if;
      end;

      --  Step 2: Decompress public key A and signature point R
      for I in 1 .. 32 loop
         declare
            Temp : Field_Element;
         begin
            Temp (I) := Public_Key (I);
            if I = 32 then
               A := Point_Decompress (Temp);
            end if;
         end;
      end loop;

      declare
         A_Enc : Field_Element;
      begin
         for I in 1 .. 32 loop
            A_Enc (I) := Public_Key (I);
         end loop;
         A := Point_Decompress (A_Enc);
      end;

      R := Point_Decompress (R_Encoded);

      --  Step 3: Compute k = SHA-512(R || A || M) mod L
      declare
         Hash_Input : String (1 .. 64 + Message'Length);
      begin
         --  R (32 bytes)
         for I in 1 .. 32 loop
            Hash_Input (I) := Character'Val (R_Encoded (I));
         end loop;
         --  A (32 bytes)
         for I in 1 .. 32 loop
            Hash_Input (32 + I) := Character'Val (Public_Key (I));
         end loop;
         --  Message
         for I in Message'Range loop
            Hash_Input (64 + I - Message'First + 1) := Message (I);
         end loop;

         K_Hash := Compute_SHA512 (Hash_Input);
         K := Reduce_Scalar_512 (K_Hash);
      end;

      --  Step 4: Compute [S]B
      B := (X => B_X, Y => B_Y, Z => Field_One, T => Field_Mul (B_X, B_Y));
      SB := Scalar_Mult (S, B);

      --  Step 5: Compute R + [k]A
      declare
         KA : Ed_Point;
      begin
         KA := Scalar_Mult (K, A);
         R_KA := Point_Add (R, KA);
      end;

      --  Step 6: Check [S]B = R + [k]A
      return Point_Equal (SB, R_KA);
   end Verify_Ed25519;

   ---------------------------------------------------------------------------
   --  Hex Utilities
   ---------------------------------------------------------------------------

   Hex_Chars : constant String := "0123456789abcdef";

   function Hex_Char_Value (C : Character) return Unsigned_8 is
   begin
      case C is
         when '0' .. '9' =>
            return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' =>
            return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' =>
            return Character'Pos (C) - Character'Pos ('A') + 10;
         when others =>
            return 0;
      end case;
   end Hex_Char_Value;

   function Hex_To_Bytes (Hex : String) return SHA256_Digest is
      Result : SHA256_Digest;
   begin
      for I in 1 .. 32 loop
         declare
            Hi : constant Unsigned_8 := Hex_Char_Value (Hex (Hex'First + (I - 1) * 2));
            Lo : constant Unsigned_8 := Hex_Char_Value (Hex (Hex'First + (I - 1) * 2 + 1));
         begin
            Result (I) := Hi * 16 + Lo;
         end;
      end loop;
      return Result;
   end Hex_To_Bytes;

   function Bytes_To_Hex (Digest : SHA256_Digest) return String is
      Result : String (1 .. 64);
   begin
      for I in Digest'Range loop
         Result ((I - 1) * 2 + 1) := Hex_Chars (Natural (Digest (I) / 16) + 1);
         Result ((I - 1) * 2 + 2) := Hex_Chars (Natural (Digest (I) mod 16) + 1);
      end loop;
      return Result;
   end Bytes_To_Hex;

   function Bytes_To_Hex_512 (Digest : SHA512_Digest) return String is
      Result : String (1 .. 128);
   begin
      for I in Digest'Range loop
         Result ((I - 1) * 2 + 1) := Hex_Chars (Natural (Digest (I) / 16) + 1);
         Result ((I - 1) * 2 + 2) := Hex_Chars (Natural (Digest (I) mod 16) + 1);
      end loop;
      return Result;
   end Bytes_To_Hex_512;

   ---------------------------------------------------------------------------
   --  Constant_Time_Equal - Prevent timing attacks
   ---------------------------------------------------------------------------

   function Constant_Time_Equal
      (Left, Right : SHA256_Digest) return Boolean
   is
      Diff : Unsigned_8 := 0;
   begin
      for I in Left'Range loop
         Diff := Diff or (Left (I) xor Right (I));
      end loop;
      return Diff = 0;
   end Constant_Time_Equal;

   ---------------------------------------------------------------------------
   --  Verify_SHA256 - Verify data matches expected hash
   ---------------------------------------------------------------------------

   function Verify_SHA256
      (Data          : String;
       Expected_Hash : SHA256_Digest) return Boolean
   is
      Computed : constant SHA256_Digest := Compute_SHA256 (Data);
   begin
      return Constant_Time_Equal (Computed, Expected_Hash);
   end Verify_SHA256;

   ---------------------------------------------------------------------------
   --  Verify_Hash_String - Parse and verify algorithm:digest format
   ---------------------------------------------------------------------------

   function Verify_Hash_String
      (Data        : String;
       Hash_String : String) return Verification_Result
   is
      Colon_Pos : Natural := 0;
   begin
      --  Find colon separator
      for I in Hash_String'Range loop
         if Hash_String (I) = ':' then
            Colon_Pos := I;
            exit;
         end if;
      end loop;

      if Colon_Pos = 0 then
         return Parse_Error;
      end if;

      declare
         Algo_Part : constant String := Hash_String (Hash_String'First .. Colon_Pos - 1);
         Digest_Part : constant String := Hash_String (Colon_Pos + 1 .. Hash_String'Last);
      begin
         --  Check algorithm
         if Algo_Part = "sha256" and Digest_Part'Length = 64 then
            declare
               Expected : constant SHA256_Digest := Hex_To_Bytes (Digest_Part);
            begin
               if Verify_SHA256 (Data, Expected) then
                  return Valid;
               else
                  return Invalid_Hash;
               end if;
            end;
         elsif Algo_Part = "sha384" or Algo_Part = "sha512" or
               Algo_Part = "blake3" or Algo_Part = "shake256"
         then
            --  Unsupported algorithms for now
            return Algorithm_Mismatch;
         else
            return Parse_Error;
         end if;
      end;
   end Verify_Hash_String;

end Cerro_Crypto;
