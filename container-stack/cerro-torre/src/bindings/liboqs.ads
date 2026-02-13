-------------------------------------------------------------------------------
--  LibOQS - Ada FFI Bindings to Open Quantum Safe Library
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  This package provides Ada bindings to the liboqs C library for
--  post-quantum cryptographic signature operations.
--
--  Primary focus: ML-DSA-87 (FIPS 204, NIST Security Level 5)
--  Formerly known as: Dilithium5 (CRYSTALS-Dilithium)
--
--  Reference: https://github.com/open-quantum-safe/liboqs
--
--  Usage:
--    1. Call OQS_SIG_alg_is_enabled to check if ML-DSA-87 is available
--    2. Call OQS_SIG_new to create a signature context
--    3. Use OQS_SIG_keypair, OQS_SIG_sign, OQS_SIG_verify
--    4. Call OQS_SIG_free to release the context
--
--  Linking: Requires -loqs flag when building with liboqs installed
-------------------------------------------------------------------------------

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

package LibOQS is

   pragma Preelaborate;

   ---------------------------------------------------------------------------
   --  OQS Status Codes (from common.h)
   ---------------------------------------------------------------------------

   type OQS_STATUS is
     (OQS_ERROR,
      OQS_SUCCESS,
      OQS_EXTERNAL_LIB_ERROR_OPENSSL);

   for OQS_STATUS use
     (OQS_ERROR                      => -1,
      OQS_SUCCESS                    => 0,
      OQS_EXTERNAL_LIB_ERROR_OPENSSL => 50);

   for OQS_STATUS'Size use Interfaces.C.int'Size;

   ---------------------------------------------------------------------------
   --  Algorithm Name Constants
   ---------------------------------------------------------------------------

   --  ML-DSA algorithm identifiers (FIPS 204)
   OQS_SIG_Alg_ML_DSA_44 : constant String := "ML-DSA-44" & ASCII.NUL;
   OQS_SIG_Alg_ML_DSA_65 : constant String := "ML-DSA-65" & ASCII.NUL;
   OQS_SIG_Alg_ML_DSA_87 : constant String := "ML-DSA-87" & ASCII.NUL;

   --  Legacy Dilithium names (may still work in some liboqs versions)
   OQS_SIG_Alg_Dilithium2 : constant String := "Dilithium2" & ASCII.NUL;
   OQS_SIG_Alg_Dilithium3 : constant String := "Dilithium3" & ASCII.NUL;
   OQS_SIG_Alg_Dilithium5 : constant String := "Dilithium5" & ASCII.NUL;

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Size Constants (FIPS 204, Level 5)
   ---------------------------------------------------------------------------

   --  These match the values in CT_PQCrypto
   ML_DSA_87_Public_Key_Bytes  : constant := 2592;
   ML_DSA_87_Secret_Key_Bytes  : constant := 4896;
   ML_DSA_87_Signature_Bytes   : constant := 4627;

   ---------------------------------------------------------------------------
   --  Type Definitions for Signature Data
   ---------------------------------------------------------------------------

   --  Generic byte array type for C interop (must be defined FIRST)
   type Unsigned_8_Array is array (Positive range <>) of Unsigned_8;
   pragma Convention (C, Unsigned_8_Array);

   --  Byte arrays for keys and signatures (subtypes of generic array)
   subtype ML_DSA_87_Public_Key_Array is
     Unsigned_8_Array (1 .. ML_DSA_87_Public_Key_Bytes);

   subtype ML_DSA_87_Secret_Key_Array is
     Unsigned_8_Array (1 .. ML_DSA_87_Secret_Key_Bytes);

   subtype ML_DSA_87_Signature_Array is
     Unsigned_8_Array (1 .. ML_DSA_87_Signature_Bytes);

   --  Access types for C pointer parameters
   type Unsigned_8_Ptr is access all Unsigned_8;
   pragma Convention (C, Unsigned_8_Ptr);

   type Size_T_Ptr is access all size_t;
   pragma Convention (C, Size_T_Ptr);

   ---------------------------------------------------------------------------
   --  OQS_SIG Structure (Opaque Pointer)
   --
   --  The actual structure contains function pointers and metadata.
   --  We treat it as an opaque handle in Ada.
   ---------------------------------------------------------------------------

   type OQS_SIG_Record is limited private;
   type OQS_SIG is access all OQS_SIG_Record;
   pragma Convention (C, OQS_SIG);

   --  Null check helper
   function Is_Null (Sig : OQS_SIG) return Boolean;

   ---------------------------------------------------------------------------
   --  Algorithm Availability Check
   ---------------------------------------------------------------------------

   --  Check if a signature algorithm is enabled in the liboqs build
   --  Returns 1 if enabled, 0 if not
   function OQS_SIG_Alg_Is_Enabled (Method_Name : chars_ptr) return int;
   pragma Import (C, OQS_SIG_Alg_Is_Enabled, "OQS_SIG_alg_is_enabled");

   --  Ada-friendly wrapper
   function Algorithm_Is_Enabled (Algorithm_Name : String) return Boolean;

   --  Check if context string signatures are supported
   function OQS_SIG_Supports_Ctx_Str (Alg_Name : chars_ptr) return Interfaces.C.C_bool;
   pragma Import (C, OQS_SIG_Supports_Ctx_Str, "OQS_SIG_supports_ctx_str");

   ---------------------------------------------------------------------------
   --  Signature Context Management
   ---------------------------------------------------------------------------

   --  Create a new signature context for the specified algorithm
   --  Returns null if the algorithm is not available
   function OQS_SIG_New (Method_Name : chars_ptr) return OQS_SIG;
   pragma Import (C, OQS_SIG_New, "OQS_SIG_new");

   --  Free a signature context
   --  Safe to call with null pointer
   procedure OQS_SIG_Free (Sig : OQS_SIG);
   pragma Import (C, OQS_SIG_Free, "OQS_SIG_free");

   --  Ada-friendly wrapper to create ML-DSA-87 context
   function New_ML_DSA_87_Context return OQS_SIG;

   ---------------------------------------------------------------------------
   --  Structure Field Accessors
   --
   --  These functions access fields of the OQS_SIG structure.
   --  Since we treat OQS_SIG as opaque, we use C helper functions or
   --  direct memory access with offsets (less portable).
   --
   --  For safety, we provide high-level Ada wrappers that validate sizes.
   ---------------------------------------------------------------------------

   --  Get public key length from context
   function Get_Public_Key_Length (Sig : OQS_SIG) return size_t;

   --  Get secret key length from context
   function Get_Secret_Key_Length (Sig : OQS_SIG) return size_t;

   --  Get maximum signature length from context
   function Get_Signature_Length (Sig : OQS_SIG) return size_t;

   --  Get algorithm name from context
   function Get_Method_Name (Sig : OQS_SIG) return String;

   ---------------------------------------------------------------------------
   --  Key Generation
   ---------------------------------------------------------------------------

   --  Generate a keypair using the algorithm in the context
   --  The output buffers must be properly sized according to the context
   function OQS_SIG_Keypair
     (Sig        : OQS_SIG;
      Public_Key : System.Address;
      Secret_Key : System.Address) return OQS_STATUS;
   pragma Import (C, OQS_SIG_Keypair, "OQS_SIG_keypair");

   --  Ada-friendly keypair generation for ML-DSA-87
   type Keypair_Result is record
      Status     : OQS_STATUS;
      Public_Key : ML_DSA_87_Public_Key_Array;
      Secret_Key : ML_DSA_87_Secret_Key_Array;
   end record;

   function Generate_ML_DSA_87_Keypair return Keypair_Result;

   ---------------------------------------------------------------------------
   --  Signing
   ---------------------------------------------------------------------------

   --  Sign a message
   --  Signature buffer must be at least length_signature bytes
   --  signature_len will be set to actual signature length
   function OQS_SIG_Sign
     (Sig           : OQS_SIG;
      Signature     : System.Address;
      Signature_Len : access size_t;
      Message       : System.Address;
      Message_Len   : size_t;
      Secret_Key    : System.Address) return OQS_STATUS;
   pragma Import (C, OQS_SIG_Sign, "OQS_SIG_sign");

   --  Sign with context string (for algorithms that support it)
   function OQS_SIG_Sign_With_Ctx_Str
     (Sig           : OQS_SIG;
      Signature     : System.Address;
      Signature_Len : access size_t;
      Message       : System.Address;
      Message_Len   : size_t;
      Ctx_Str       : System.Address;
      Ctx_Str_Len   : size_t;
      Secret_Key    : System.Address) return OQS_STATUS;
   pragma Import (C, OQS_SIG_Sign_With_Ctx_Str, "OQS_SIG_sign_with_ctx_str");

   --  Ada-friendly signing for ML-DSA-87
   type Sign_Result is record
      Status    : OQS_STATUS;
      Signature : ML_DSA_87_Signature_Array;
      Sig_Len   : Natural;
   end record;

   function Sign_ML_DSA_87
     (Message    : String;
      Secret_Key : ML_DSA_87_Secret_Key_Array) return Sign_Result;

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   --  Verify a signature
   --  Returns OQS_SUCCESS if valid, OQS_ERROR if invalid
   function OQS_SIG_Verify
     (Sig           : OQS_SIG;
      Message       : System.Address;
      Message_Len   : size_t;
      Signature     : System.Address;
      Signature_Len : size_t;
      Public_Key    : System.Address) return OQS_STATUS;
   pragma Import (C, OQS_SIG_Verify, "OQS_SIG_verify");

   --  Verify with context string
   function OQS_SIG_Verify_With_Ctx_Str
     (Sig           : OQS_SIG;
      Message       : System.Address;
      Message_Len   : size_t;
      Signature     : System.Address;
      Signature_Len : size_t;
      Ctx_Str       : System.Address;
      Ctx_Str_Len   : size_t;
      Public_Key    : System.Address) return OQS_STATUS;
   pragma Import (C, OQS_SIG_Verify_With_Ctx_Str, "OQS_SIG_verify_with_ctx_str");

   --  Ada-friendly verification for ML-DSA-87
   function Verify_ML_DSA_87
     (Message    : String;
      Signature  : ML_DSA_87_Signature_Array;
      Public_Key : ML_DSA_87_Public_Key_Array) return Boolean;

   ---------------------------------------------------------------------------
   --  Library Initialization (Optional)
   ---------------------------------------------------------------------------

   --  Check if liboqs is available at runtime
   --  This attempts to load the library and check for ML-DSA-87
   function Is_LibOQS_Available return Boolean;

   --  Get liboqs version string (if available)
   function Get_LibOQS_Version return String;

private

   --  Opaque record type - actual structure defined in C
   type OQS_SIG_Record is null record;

   --  Internal: C string conversion helper
   function To_C_String (S : String) return chars_ptr;

   --  Internal: Track library availability
   LibOQS_Checked   : Boolean := False;
   LibOQS_Available : Boolean := False;

end LibOQS;
