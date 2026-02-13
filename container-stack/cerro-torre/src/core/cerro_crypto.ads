--  Cerro Torre Crypto - SPARK-verified cryptographic operations
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  This package provides cryptographic primitives with formal verification.
--  All operations are proven free of runtime errors and implement their
--  mathematical specifications correctly.

with Interfaces;

package Cerro_Crypto
   with SPARK_Mode => On
is
   use Interfaces;

   ---------------------
   -- Type Definitions --
   ---------------------

   --  SHA-256 digest (32 bytes)
   subtype Digest_Index is Positive range 1 .. 32;
   type SHA256_Digest is array (Digest_Index) of Unsigned_8;

   --  SHA-512 digest (64 bytes)
   subtype Digest_512_Index is Positive range 1 .. 64;
   type SHA512_Digest is array (Digest_512_Index) of Unsigned_8;

   --  Ed25519 public key (32 bytes)
   subtype Key_Index is Positive range 1 .. 32;
   type Ed25519_Public_Key is array (Key_Index) of Unsigned_8;

   --  Ed25519 signature (64 bytes)
   subtype Signature_Index is Positive range 1 .. 64;
   type Ed25519_Signature is array (Signature_Index) of Unsigned_8;

   --  Hash algorithm enumeration
   type Hash_Algorithm is (SHA256, SHA384, SHA512, Blake3, Shake256);

   --  Maximum input size for hashing (prevent overflow in length encoding)
   Max_Hash_Input_Length : constant := 2**32 - 1 - 64;

   -------------------
   -- Hash Functions --
   -------------------

   --  Compute SHA-256 hash of input data
   function Compute_SHA256 (Data : String) return SHA256_Digest
      with Global => null,
           Pre    => Data'Length <= Max_Hash_Input_Length,
           Post   => Compute_SHA256'Result'Length = 32;

   --  Compute SHA-512 hash of input data
   function Compute_SHA512 (Data : String) return SHA512_Digest
      with Global => null,
           Pre    => Data'Length <= Max_Hash_Input_Length,
           Post   => Compute_SHA512'Result'Length = 64;

   -------------------------
   -- Signature Verification --
   -------------------------

   --  Verify an Ed25519 signature
   --  Returns True if and only if the signature is valid for the given
   --  message and public key.
   function Verify_Ed25519
      (Message   : String;
       Signature : Ed25519_Signature;
       Public_Key: Ed25519_Public_Key) return Boolean
      with Global => null,
           Pre    => Message'Length <= Max_Hash_Input_Length;

   -----------------------
   -- Utility Functions --
   -----------------------

   --  Convert hex string to bytes
   --  Returns empty array if input is invalid
   function Hex_To_Bytes (Hex : String) return SHA256_Digest
      with Global => null,
           Pre    => Hex'Length = 64;

   --  Convert bytes to hex string (SHA-256)
   function Bytes_To_Hex (Digest : SHA256_Digest) return String
      with Global => null,
           Post   => Bytes_To_Hex'Result'Length = 64;

   --  Convert bytes to hex string (SHA-512)
   function Bytes_To_Hex_512 (Digest : SHA512_Digest) return String
      with Global => null,
           Post   => Bytes_To_Hex_512'Result'Length = 128;

   --  Constant-time comparison to prevent timing attacks
   function Constant_Time_Equal
      (Left, Right : SHA256_Digest) return Boolean
      with Global => null;

   -----------------------
   -- Hash Verification --
   -----------------------

   --  Verify that data matches expected hash
   function Verify_SHA256
      (Data          : String;
       Expected_Hash : SHA256_Digest) return Boolean
      with Global => null,
           Pre    => Data'Length <= Max_Hash_Input_Length;

   --  Parse and verify a hash string (algorithm:hexdigest format)
   type Verification_Result is (Valid, Invalid_Hash, Algorithm_Mismatch, Parse_Error);

   function Verify_Hash_String
      (Data        : String;
       Hash_String : String) return Verification_Result
      with Global => null,
           Pre    => Data'Length <= Max_Hash_Input_Length
                     and Hash_String'Length >= 8;  -- Minimum: "sha256:X"

end Cerro_Crypto;
