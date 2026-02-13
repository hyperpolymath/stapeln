--  Cerro Torre Post-Quantum Cryptography - SPARK-verified PQC primitives
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  This package provides post-quantum cryptographic primitives for CT-SIG-02
--  hybrid signatures (Ed25519 + ML-DSA-87) and pure ML-DSA-87 signatures.
--
--  ML-DSA-87 (FIPS 204) is a lattice-based digital signature algorithm
--  providing NIST Security Level 5 (equivalent to 256-bit classical security).
--
--  Implementation Status:
--    - API defined and stable
--    - Stub implementations pending liboqs bindings
--    - Hybrid mode combines Ed25519 + ML-DSA-87 for defense-in-depth
--
--  Key Sizes (ML-DSA-87):
--    Public key:  2592 bytes
--    Secret key:  4896 bytes
--    Signature:   4627 bytes
--
--  Hybrid CT-SIG-02 Format:
--    Bytes 0-63:     Ed25519 signature (64 bytes)
--    Bytes 64-4690:  ML-DSA-87 signature (4627 bytes)
--    Total:          4691 bytes

with Interfaces;
with Cerro_Crypto;

package CT_PQCrypto
   with SPARK_Mode => On
is
   use Interfaces;
   use Cerro_Crypto;

   ---------------------
   -- Type Definitions --
   ---------------------

   --  Signature algorithm enumeration
   type Signature_Algorithm is
     (Ed25519,            --  Classical Ed25519 (current default)
      ML_DSA_87,          --  Pure post-quantum (FIPS 204, Level 5)
      CT_SIG_02);         --  Hybrid: Ed25519 + ML-DSA-87

   --  ML-DSA-87 public key (2592 bytes)
   ML_DSA_87_Public_Key_Length : constant := 2592;
   subtype ML_DSA_Public_Key_Index is Positive range 1 .. ML_DSA_87_Public_Key_Length;
   type ML_DSA_87_Public_Key is array (ML_DSA_Public_Key_Index) of Unsigned_8;

   --  ML-DSA-87 secret key (4896 bytes)
   ML_DSA_87_Secret_Key_Length : constant := 4896;
   subtype ML_DSA_Secret_Key_Index is Positive range 1 .. ML_DSA_87_Secret_Key_Length;
   type ML_DSA_87_Secret_Key is array (ML_DSA_Secret_Key_Index) of Unsigned_8;

   --  ML-DSA-87 signature (4627 bytes)
   ML_DSA_87_Signature_Length : constant := 4627;
   subtype ML_DSA_Signature_Index is Positive range 1 .. ML_DSA_87_Signature_Length;
   type ML_DSA_87_Signature is array (ML_DSA_Signature_Index) of Unsigned_8;

   --  CT-SIG-02 hybrid signature (64 + 4627 = 4691 bytes)
   CT_SIG_02_Signature_Length : constant := 4691;
   subtype CT_SIG_Signature_Index is Positive range 1 .. CT_SIG_02_Signature_Length;
   type CT_SIG_02_Signature is array (CT_SIG_Signature_Index) of Unsigned_8;

   --  Ed25519 secret key (64 bytes - seed + public key)
   subtype Ed25519_Secret_Key_Index is Positive range 1 .. 64;
   type Ed25519_Secret_Key is array (Ed25519_Secret_Key_Index) of Unsigned_8;

   --  CT-SIG-02 hybrid keypair (combines Ed25519 and ML-DSA-87 keys)
   type Hybrid_Public_Key is record
      Ed25519_Key : Ed25519_Public_Key;
      ML_DSA_Key  : ML_DSA_87_Public_Key;
   end record;

   type Hybrid_Secret_Key is record
      Ed25519_Key : Ed25519_Secret_Key;  --  Ed25519 secret key (64 bytes)
      ML_DSA_Key  : ML_DSA_87_Secret_Key;
   end record;

   --  Maximum input size for signing (consistent with Cerro_Crypto)
   Max_Sign_Input_Length : constant := 2**32 - 1 - 64;

   -----------------------
   -- Operation Results --
   -----------------------

   type Operation_Result is
     (Success,
      Not_Implemented,     --  Stub: liboqs not yet linked
      Invalid_Key,         --  Malformed or corrupted key
      Invalid_Signature,   --  Signature verification failed
      Key_Size_Mismatch,   --  Wrong key size for algorithm
      Internal_Error);     --  Unexpected internal failure

   type Verification_Result is record
      Valid          : Boolean;
      Status         : Operation_Result;
      Algorithm_Used : Signature_Algorithm;
   end record;

   --------------------------
   -- Key Generation (Stub) --
   --------------------------

   --  Generate ML-DSA-87 keypair
   --  NOTE: Returns Not_Implemented until liboqs bindings are complete
   procedure Generate_ML_DSA_87_Keypair
     (Public_Key  : out ML_DSA_87_Public_Key;
      Secret_Key  : out ML_DSA_87_Secret_Key;
      Result      : out Operation_Result)
   with Global => null;

   --  Generate CT-SIG-02 hybrid keypair (Ed25519 + ML-DSA-87)
   --  NOTE: Returns Not_Implemented until liboqs bindings are complete
   procedure Generate_Hybrid_Keypair
     (Public_Key  : out Hybrid_Public_Key;
      Secret_Key  : out Hybrid_Secret_Key;
      Result      : out Operation_Result)
   with Global => null;

   ---------------------------
   -- ML-DSA-87 Signing (Stub) --
   ---------------------------

   --  Sign a message with ML-DSA-87
   --  NOTE: Returns Not_Implemented until liboqs bindings are complete
   procedure Sign_ML_DSA_87
     (Message    : String;
      Secret_Key : ML_DSA_87_Secret_Key;
      Signature  : out ML_DSA_87_Signature;
      Result     : out Operation_Result)
   with Global => null,
        Pre    => Message'Length <= Max_Sign_Input_Length;

   --  Verify an ML-DSA-87 signature
   --  NOTE: Returns Not_Implemented until liboqs bindings are complete
   function Verify_ML_DSA_87
     (Message    : String;
      Signature  : ML_DSA_87_Signature;
      Public_Key : ML_DSA_87_Public_Key) return Verification_Result
   with Global => null,
        Pre    => Message'Length <= Max_Sign_Input_Length;

   ----------------------------
   -- CT-SIG-02 Hybrid Signing --
   ----------------------------

   --  Sign a message with CT-SIG-02 (Ed25519 + ML-DSA-87)
   --  Creates both signatures, concatenates them in canonical format
   --  NOTE: ML-DSA-87 portion returns Not_Implemented until liboqs ready
   procedure Sign_Hybrid
     (Message    : String;
      Secret_Key : Hybrid_Secret_Key;
      Signature  : out CT_SIG_02_Signature;
      Result     : out Operation_Result)
   with Global => null,
        Pre    => Message'Length <= Max_Sign_Input_Length;

   --  Verify a CT-SIG-02 hybrid signature
   --  Both Ed25519 AND ML-DSA-87 must verify for success
   --  Returns Verification_Result with algorithm set to CT_SIG_02
   function Verify_Hybrid
     (Message    : String;
      Signature  : CT_SIG_02_Signature;
      Public_Key : Hybrid_Public_Key) return Verification_Result
   with Global => null,
        Pre    => Message'Length <= Max_Sign_Input_Length;

   --  Verify hybrid signature with fallback modes
   --  Mode determines which signatures must verify:
   --    Both    = Both must verify (recommended)
   --    Ed25519 = Only Ed25519 must verify (compatibility)
   --    ML_DSA  = Only ML-DSA-87 must verify (post-quantum only)
   type Hybrid_Verify_Mode is (Both, Ed25519_Only, ML_DSA_Only);

   function Verify_Hybrid_With_Mode
     (Message    : String;
      Signature  : CT_SIG_02_Signature;
      Public_Key : Hybrid_Public_Key;
      Mode       : Hybrid_Verify_Mode) return Verification_Result
   with Global => null,
        Pre    => Message'Length <= Max_Sign_Input_Length;

   -----------------------
   -- Utility Functions --
   -----------------------

   --  Check if algorithm is available (liboqs linked)
   function Is_Algorithm_Available (Algo : Signature_Algorithm) return Boolean
   with Global => null;

   --  Get algorithm name as string
   function Algorithm_Name (Algo : Signature_Algorithm) return String
   with Global => null,
        Post   => Algorithm_Name'Result'Length > 0;

   --  Get signature size for algorithm
   function Signature_Size (Algo : Signature_Algorithm) return Positive
   with Global => null;

   --  Get public key size for algorithm
   function Public_Key_Size (Algo : Signature_Algorithm) return Positive
   with Global => null;

   --  Convert ML-DSA-87 public key to hex string
   function ML_DSA_Public_Key_To_Hex (Key : ML_DSA_87_Public_Key) return String
   with Global => null,
        Post   => ML_DSA_Public_Key_To_Hex'Result'Length =
                  ML_DSA_87_Public_Key_Length * 2;

   --  Parse ML-DSA-87 public key from hex string
   function Hex_To_ML_DSA_Public_Key
     (Hex : String) return ML_DSA_87_Public_Key
   with Global => null,
        Pre    => Hex'Length = ML_DSA_87_Public_Key_Length * 2;

   --  Extract Ed25519 signature from hybrid signature
   function Get_Ed25519_From_Hybrid
     (Sig : CT_SIG_02_Signature) return Ed25519_Signature
   with Global => null;

   --  Extract ML-DSA-87 signature from hybrid signature
   function Get_ML_DSA_From_Hybrid
     (Sig : CT_SIG_02_Signature) return ML_DSA_87_Signature
   with Global => null;

   --  Combine Ed25519 and ML-DSA-87 signatures into hybrid
   function Make_Hybrid_Signature
     (Ed_Sig    : Ed25519_Signature;
      ML_DSA_Sig : ML_DSA_87_Signature) return CT_SIG_02_Signature
   with Global => null;

end CT_PQCrypto;
