-------------------------------------------------------------------------------
--  CT_PQCrypto - Implementation of post-quantum cryptographic primitives
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Implements ML-DSA-87 (FIPS 204) and CT-SIG-02 hybrid signatures.
--
--  Implementation Status:
--    - LibOQS integration: COMPLETE
--    - ML-DSA-87 operations use liboqs when available
--    - Falls back to stub implementations when liboqs is not linked
--
--  LibOQS Integration:
--    - Bindings in src/bindings/liboqs.ads
--    - Requires linking with -loqs when liboqs is installed
--    - Automatically detects availability at runtime
--
--  Security Considerations:
--    - Hybrid mode provides defense-in-depth: compromise of either algorithm
--      alone does not break the combined signature
--    - ML-DSA-87 provides NIST Security Level 5 (equivalent to AES-256)
--    - Ed25519 provides ~128-bit classical security (vulnerable to quantum)
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  SPARK mode off due to FFI bindings

with LibOQS;

package body CT_PQCrypto is

   ---------------------------------------------------------------------------
   --  Internal Constants
   ---------------------------------------------------------------------------

   --  Placeholder value to indicate uninitialized keys
   Zero_Byte : constant Unsigned_8 := 0;

   --  Ed25519 signature offset in hybrid signature
   Ed25519_Offset : constant := 0;
   Ed25519_Length : constant := 64;

   --  ML-DSA-87 signature offset in hybrid signature
   ML_DSA_Offset : constant := 64;

   ---------------------------------------------------------------------------
   --  LibOQS Availability Check
   ---------------------------------------------------------------------------

   --  Cache the liboqs availability status
   LibOQS_Checked   : Boolean := False;
   LibOQS_Available : Boolean := False;

   function Check_LibOQS_Available return Boolean is
   begin
      if not LibOQS_Checked then
         LibOQS_Available := LibOQS.Is_LibOQS_Available;
         LibOQS_Checked := True;
      end if;
      return LibOQS_Available;
   end Check_LibOQS_Available;

   ---------------------------------------------------------------------------
   --  Internal: Convert between CT_PQCrypto and LibOQS types
   ---------------------------------------------------------------------------

   --  Convert CT_PQCrypto public key to LibOQS array
   function To_LibOQS_Public_Key
     (Key : ML_DSA_87_Public_Key) return LibOQS.ML_DSA_87_Public_Key_Array
   is
      Result : LibOQS.ML_DSA_87_Public_Key_Array;
   begin
      for I in Key'Range loop
         Result (I) := Key (I);
      end loop;
      return Result;
   end To_LibOQS_Public_Key;

   --  Convert LibOQS public key to CT_PQCrypto array
   function From_LibOQS_Public_Key
     (Key : LibOQS.ML_DSA_87_Public_Key_Array) return ML_DSA_87_Public_Key
   is
      Result : ML_DSA_87_Public_Key;
   begin
      for I in Key'Range loop
         Result (I) := Key (I);
      end loop;
      return Result;
   end From_LibOQS_Public_Key;

   --  Convert CT_PQCrypto secret key to LibOQS array
   function To_LibOQS_Secret_Key
     (Key : ML_DSA_87_Secret_Key) return LibOQS.ML_DSA_87_Secret_Key_Array
   is
      Result : LibOQS.ML_DSA_87_Secret_Key_Array;
   begin
      for I in Key'Range loop
         Result (I) := Key (I);
      end loop;
      return Result;
   end To_LibOQS_Secret_Key;

   --  Convert LibOQS secret key to CT_PQCrypto array
   function From_LibOQS_Secret_Key
     (Key : LibOQS.ML_DSA_87_Secret_Key_Array) return ML_DSA_87_Secret_Key
   is
      Result : ML_DSA_87_Secret_Key;
   begin
      for I in Key'Range loop
         Result (I) := Key (I);
      end loop;
      return Result;
   end From_LibOQS_Secret_Key;

   --  Convert CT_PQCrypto signature to LibOQS array
   function To_LibOQS_Signature
     (Sig : ML_DSA_87_Signature) return LibOQS.ML_DSA_87_Signature_Array
   is
      Result : LibOQS.ML_DSA_87_Signature_Array;
   begin
      for I in Sig'Range loop
         Result (I) := Sig (I);
      end loop;
      return Result;
   end To_LibOQS_Signature;

   --  Convert LibOQS signature to CT_PQCrypto array
   function From_LibOQS_Signature
     (Sig : LibOQS.ML_DSA_87_Signature_Array) return ML_DSA_87_Signature
   is
      Result : ML_DSA_87_Signature;
   begin
      for I in Sig'Range loop
         Result (I) := Sig (I);
      end loop;
      return Result;
   end From_LibOQS_Signature;

   ---------------------------------------------------------------------------
   --  Key Generation
   ---------------------------------------------------------------------------

   procedure Generate_ML_DSA_87_Keypair
     (Public_Key  : out ML_DSA_87_Public_Key;
      Secret_Key  : out ML_DSA_87_Secret_Key;
      Result      : out Operation_Result)
   is
   begin
      --  Check if liboqs is available
      if Check_LibOQS_Available then
         --  Use liboqs for key generation
         declare
            OQS_Result : LibOQS.Keypair_Result;
         begin
            OQS_Result := LibOQS.Generate_ML_DSA_87_Keypair;

            if LibOQS."=" (OQS_Result.Status, LibOQS.OQS_SUCCESS) then
               Public_Key := From_LibOQS_Public_Key (OQS_Result.Public_Key);
               Secret_Key := From_LibOQS_Secret_Key (OQS_Result.Secret_Key);
               Result := Success;
            else
               --  Key generation failed
               Public_Key := (others => Zero_Byte);
               Secret_Key := (others => Zero_Byte);
               Result := Internal_Error;
            end if;
         end;
      else
         --  LibOQS not available - return stub response
         Public_Key := (others => Zero_Byte);
         Secret_Key := (others => Zero_Byte);
         Result := Not_Implemented;
      end if;
   end Generate_ML_DSA_87_Keypair;

   procedure Generate_Hybrid_Keypair
     (Public_Key  : out Hybrid_Public_Key;
      Secret_Key  : out Hybrid_Secret_Key;
      Result      : out Operation_Result)
   is
      ML_DSA_Pub    : ML_DSA_87_Public_Key;
      ML_DSA_Sec    : ML_DSA_87_Secret_Key;
      ML_DSA_Result : Operation_Result;
   begin
      --  Initialize to zero
      Public_Key.Ed25519_Key := (others => Zero_Byte);
      Public_Key.ML_DSA_Key := (others => Zero_Byte);
      Secret_Key.Ed25519_Key := (others => Zero_Byte);
      Secret_Key.ML_DSA_Key := (others => Zero_Byte);

      --  Generate ML-DSA-87 keypair
      Generate_ML_DSA_87_Keypair (ML_DSA_Pub, ML_DSA_Sec, ML_DSA_Result);

      if ML_DSA_Result = Success then
         Public_Key.ML_DSA_Key := ML_DSA_Pub;
         Secret_Key.ML_DSA_Key := ML_DSA_Sec;

         --  TODO: Generate Ed25519 keypair (needs Ed25519 keygen in Cerro_Crypto)
         --  For now, ML-DSA-87 part works, Ed25519 is zeroed
         --  Result := Success;

         --  Until Ed25519 keygen is added, report partial implementation
         Result := Not_Implemented;
      else
         Result := ML_DSA_Result;
      end if;
   end Generate_Hybrid_Keypair;

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Signing
   ---------------------------------------------------------------------------

   procedure Sign_ML_DSA_87
     (Message    : String;
      Secret_Key : ML_DSA_87_Secret_Key;
      Signature  : out ML_DSA_87_Signature;
      Result     : out Operation_Result)
   is
   begin
      --  Check if liboqs is available
      if Check_LibOQS_Available then
         --  Use liboqs for signing
         declare
            OQS_Result : LibOQS.Sign_Result;
            OQS_SK     : LibOQS.ML_DSA_87_Secret_Key_Array;
         begin
            OQS_SK := To_LibOQS_Secret_Key (Secret_Key);
            OQS_Result := LibOQS.Sign_ML_DSA_87 (Message, OQS_SK);

            --  Clear the local copy of secret key
            OQS_SK := (others => 0);

            if LibOQS."=" (OQS_Result.Status, LibOQS.OQS_SUCCESS) then
               Signature := From_LibOQS_Signature (OQS_Result.Signature);
               Result := Success;
            else
               Signature := (others => Zero_Byte);
               Result := Internal_Error;
            end if;
         end;
      else
         --  LibOQS not available - return stub response
         Signature := (others => Zero_Byte);
         Result := Not_Implemented;
      end if;
   end Sign_ML_DSA_87;

   function Verify_ML_DSA_87
     (Message    : String;
      Signature  : ML_DSA_87_Signature;
      Public_Key : ML_DSA_87_Public_Key) return Verification_Result
   is
   begin
      --  Check if liboqs is available
      if Check_LibOQS_Available then
         --  Use liboqs for verification
         declare
            OQS_Sig : LibOQS.ML_DSA_87_Signature_Array;
            OQS_PK  : LibOQS.ML_DSA_87_Public_Key_Array;
            Valid   : Boolean;
         begin
            OQS_Sig := To_LibOQS_Signature (Signature);
            OQS_PK := To_LibOQS_Public_Key (Public_Key);

            Valid := LibOQS.Verify_ML_DSA_87 (Message, OQS_Sig, OQS_PK);

            if Valid then
               return (Valid          => True,
                       Status         => Success,
                       Algorithm_Used => ML_DSA_87);
            else
               return (Valid          => False,
                       Status         => Invalid_Signature,
                       Algorithm_Used => ML_DSA_87);
            end if;
         end;
      else
         --  LibOQS not available - return stub response
         return (Valid          => False,
                 Status         => Not_Implemented,
                 Algorithm_Used => ML_DSA_87);
      end if;
   end Verify_ML_DSA_87;

   ---------------------------------------------------------------------------
   --  CT-SIG-02 Hybrid Signing
   ---------------------------------------------------------------------------

   procedure Sign_Hybrid
     (Message    : String;
      Secret_Key : Hybrid_Secret_Key;
      Signature  : out CT_SIG_02_Signature;
      Result     : out Operation_Result)
   is
      ML_Sig        : ML_DSA_87_Signature;
      ML_Result     : Operation_Result;
   begin
      --  Initialize signature to zeros
      Signature := (others => Zero_Byte);

      --  Sign with ML-DSA-87
      Sign_ML_DSA_87 (Message, Secret_Key.ML_DSA_Key, ML_Sig, ML_Result);

      if ML_Result /= Success then
         Result := ML_Result;
         return;
      end if;

      --  TODO: Sign with Ed25519 (needs Ed25519 sign in Cerro_Crypto)
      --  For now, Ed25519 portion is zeroed

      --  Combine signatures
      --  Ed25519 portion stays zeroed until Ed25519 signing is added
      for I in ML_Sig'Range loop
         Signature (ML_DSA_Offset + I) := ML_Sig (I);
      end loop;

      --  Report not fully implemented until Ed25519 signing is added
      Result := Not_Implemented;
   end Sign_Hybrid;

   function Verify_Hybrid
     (Message    : String;
      Signature  : CT_SIG_02_Signature;
      Public_Key : Hybrid_Public_Key) return Verification_Result
   is
   begin
      --  Default mode: Both signatures must verify
      return Verify_Hybrid_With_Mode (Message, Signature, Public_Key, Both);
   end Verify_Hybrid;

   function Verify_Hybrid_With_Mode
     (Message    : String;
      Signature  : CT_SIG_02_Signature;
      Public_Key : Hybrid_Public_Key;
      Mode       : Hybrid_Verify_Mode) return Verification_Result
   is
      Ed_Sig          : Ed25519_Signature;
      ML_Sig          : ML_DSA_87_Signature;
      Ed_Valid        : Boolean := False;
      ML_Result       : Verification_Result;
   begin
      --  Extract component signatures
      Ed_Sig := Get_Ed25519_From_Hybrid (Signature);
      ML_Sig := Get_ML_DSA_From_Hybrid (Signature);

      --  Verify Ed25519 component (functional - uses Cerro_Crypto)
      Ed_Valid := Cerro_Crypto.Verify_Ed25519
        (Message, Ed_Sig, Public_Key.Ed25519_Key);

      --  Verify ML-DSA-87 component (uses liboqs when available)
      ML_Result := Verify_ML_DSA_87 (Message, ML_Sig, Public_Key.ML_DSA_Key);

      --  Combine results based on mode
      case Mode is
         when Both =>
            --  Both must verify
            if ML_Result.Status = Not_Implemented then
               --  ML-DSA not available, report not implemented
               return (Valid          => False,
                       Status         => Not_Implemented,
                       Algorithm_Used => CT_SIG_02);
            elsif Ed_Valid and ML_Result.Valid then
               return (Valid          => True,
                       Status         => Success,
                       Algorithm_Used => CT_SIG_02);
            else
               return (Valid          => False,
                       Status         => Invalid_Signature,
                       Algorithm_Used => CT_SIG_02);
            end if;

         when Ed25519_Only =>
            --  Only Ed25519 must verify (compatibility mode)
            if Ed_Valid then
               return (Valid          => True,
                       Status         => Success,
                       Algorithm_Used => Ed25519);
            else
               return (Valid          => False,
                       Status         => Invalid_Signature,
                       Algorithm_Used => Ed25519);
            end if;

         when ML_DSA_Only =>
            --  Only ML-DSA must verify (quantum-only mode)
            if ML_Result.Status = Not_Implemented then
               return (Valid          => False,
                       Status         => Not_Implemented,
                       Algorithm_Used => ML_DSA_87);
            elsif ML_Result.Valid then
               return (Valid          => True,
                       Status         => Success,
                       Algorithm_Used => ML_DSA_87);
            else
               return (Valid          => False,
                       Status         => Invalid_Signature,
                       Algorithm_Used => ML_DSA_87);
            end if;
      end case;
   end Verify_Hybrid_With_Mode;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Is_Algorithm_Available (Algo : Signature_Algorithm) return Boolean is
   begin
      case Algo is
         when Ed25519 =>
            --  Ed25519 is available via Cerro_Crypto
            return True;

         when ML_DSA_87 =>
            --  Check if liboqs is available with ML-DSA-87 support
            return Check_LibOQS_Available;

         when CT_SIG_02 =>
            --  Hybrid requires both algorithms
            return Is_Algorithm_Available (Ed25519) and
                   Is_Algorithm_Available (ML_DSA_87);
      end case;
   end Is_Algorithm_Available;

   function Algorithm_Name (Algo : Signature_Algorithm) return String is
   begin
      case Algo is
         when Ed25519 =>
            return "Ed25519";
         when ML_DSA_87 =>
            return "ML-DSA-87";
         when CT_SIG_02 =>
            return "CT-SIG-02";
      end case;
   end Algorithm_Name;

   function Signature_Size (Algo : Signature_Algorithm) return Positive is
   begin
      case Algo is
         when Ed25519 =>
            return 64;
         when ML_DSA_87 =>
            return ML_DSA_87_Signature_Length;
         when CT_SIG_02 =>
            return CT_SIG_02_Signature_Length;
      end case;
   end Signature_Size;

   function Public_Key_Size (Algo : Signature_Algorithm) return Positive is
   begin
      case Algo is
         when Ed25519 =>
            return 32;
         when ML_DSA_87 =>
            return ML_DSA_87_Public_Key_Length;
         when CT_SIG_02 =>
            --  Hybrid: Ed25519 (32) + ML-DSA-87 (2592)
            return 32 + ML_DSA_87_Public_Key_Length;
      end case;
   end Public_Key_Size;

   ---------------------------------------------------------------------------
   --  Hex Conversion Utilities
   ---------------------------------------------------------------------------

   Hex_Chars : constant String := "0123456789abcdef";

   function ML_DSA_Public_Key_To_Hex (Key : ML_DSA_87_Public_Key) return String is
      Result : String (1 .. ML_DSA_87_Public_Key_Length * 2);
   begin
      for I in Key'Range loop
         Result ((I - 1) * 2 + 1) := Hex_Chars (Natural (Key (I) / 16) + 1);
         Result ((I - 1) * 2 + 2) := Hex_Chars (Natural (Key (I) mod 16) + 1);
      end loop;
      return Result;
   end ML_DSA_Public_Key_To_Hex;

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

   function Hex_To_ML_DSA_Public_Key
     (Hex : String) return ML_DSA_87_Public_Key
   is
      Result : ML_DSA_87_Public_Key;
   begin
      for I in Result'Range loop
         declare
            Hi : constant Unsigned_8 :=
               Hex_Char_Value (Hex (Hex'First + (I - 1) * 2));
            Lo : constant Unsigned_8 :=
               Hex_Char_Value (Hex (Hex'First + (I - 1) * 2 + 1));
         begin
            Result (I) := Hi * 16 + Lo;
         end;
      end loop;
      return Result;
   end Hex_To_ML_DSA_Public_Key;

   ---------------------------------------------------------------------------
   --  Hybrid Signature Extraction/Construction
   ---------------------------------------------------------------------------

   function Get_Ed25519_From_Hybrid
     (Sig : CT_SIG_02_Signature) return Ed25519_Signature
   is
      Result : Ed25519_Signature;
   begin
      for I in Result'Range loop
         Result (I) := Sig (Ed25519_Offset + I);
      end loop;
      return Result;
   end Get_Ed25519_From_Hybrid;

   function Get_ML_DSA_From_Hybrid
     (Sig : CT_SIG_02_Signature) return ML_DSA_87_Signature
   is
      Result : ML_DSA_87_Signature;
   begin
      for I in Result'Range loop
         Result (I) := Sig (ML_DSA_Offset + I);
      end loop;
      return Result;
   end Get_ML_DSA_From_Hybrid;

   function Make_Hybrid_Signature
     (Ed_Sig    : Ed25519_Signature;
      ML_DSA_Sig : ML_DSA_87_Signature) return CT_SIG_02_Signature
   is
      Result : CT_SIG_02_Signature;
   begin
      --  Copy Ed25519 signature (bytes 1-64)
      for I in Ed_Sig'Range loop
         Result (Ed25519_Offset + I) := Ed_Sig (I);
      end loop;

      --  Copy ML-DSA-87 signature (bytes 65-4691)
      for I in ML_DSA_Sig'Range loop
         Result (ML_DSA_Offset + I) := ML_DSA_Sig (I);
      end loop;

      return Result;
   end Make_Hybrid_Signature;

end CT_PQCrypto;
