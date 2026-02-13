--  Cerro Torre Crypto OpenSSL - OpenSSL bindings for cryptographic operations
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  This package provides OpenSSL bindings for operations not yet formally verified.
--  Used as MVP implementation until SPARK-verified alternatives are ready.

pragma SPARK_Mode (Off);  --  External C calls cannot be verified with SPARK

with Interfaces;
with Cerro_Crypto;

package Cerro_Crypto_OpenSSL is

   --  Re-export types from Cerro_Crypto for convenience
   subtype Ed25519_Public_Key is Cerro_Crypto.Ed25519_Public_Key;
   subtype Ed25519_Signature is Cerro_Crypto.Ed25519_Signature;

   --  Ed25519 private key (32 bytes seed + 32 bytes public key = 64 bytes total)
   subtype Ed25519_Private_Key_Index is Positive range 1 .. 64;
   type Ed25519_Private_Key is array (Ed25519_Private_Key_Index) of Interfaces.Unsigned_8;

   ---------------------
   -- Key Generation --
   ---------------------

   --  Generate a new Ed25519 keypair
   --  Returns (private_key, public_key)
   procedure Generate_Ed25519_Keypair
      (Private_Key : out Ed25519_Private_Key;
       Public_Key  : out Ed25519_Public_Key;
       Success     : out Boolean);

   ----------------------
   -- Signing Operations --
   ----------------------

   --  Sign a message with Ed25519 private key
   --  Returns signature if successful
   procedure Sign_Ed25519
      (Message     : String;
       Private_Key : Ed25519_Private_Key;
       Signature   : out Ed25519_Signature;
       Success     : out Boolean);

   ----------------------------
   -- Verification Operations --
   ----------------------------

   --  Verify an Ed25519 signature using OpenSSL
   --  Valid indicates whether the signature matches the message and public key.
   --  Success is False if the verification process itself failed (OpenSSL error).
   procedure Verify_Ed25519
      (Message    : String;
       Signature  : Ed25519_Signature;
       Public_Key : Ed25519_Public_Key;
       Valid      : out Boolean;
       Success    : out Boolean);

   -----------------------
   -- Key Serialization --
   -----------------------

   --  Convert private key to hex string (128 hex chars)
   function Private_Key_To_Hex (Key : Ed25519_Private_Key) return String
      with Post => Private_Key_To_Hex'Result'Length = 128;

   --  Convert hex string to private key
   --  Returns success = False if hex is invalid
   procedure Hex_To_Private_Key
      (Hex         : String;
       Private_Key : out Ed25519_Private_Key;
       Success     : out Boolean)
      with Pre => Hex'Length = 128;

   --  Convert public key to hex string (64 hex chars)
   function Public_Key_To_Hex (Key : Ed25519_Public_Key) return String
      with Post => Public_Key_To_Hex'Result'Length = 64;

   --  Convert hex string to public key
   --  Returns success = False if hex is invalid
   procedure Hex_To_Public_Key
      (Hex        : String;
       Public_Key : out Ed25519_Public_Key;
       Success    : out Boolean)
      with Pre => Hex'Length = 64;

   --  Convert signature to hex string (128 hex chars)
   function Signature_To_Hex (Sig : Ed25519_Signature) return String
      with Post => Signature_To_Hex'Result'Length = 128;

   --  Convert hex string to signature
   --  Returns success = False if hex is invalid
   procedure Hex_To_Signature
      (Hex       : String;
       Signature : out Ed25519_Signature;
       Success   : out Boolean)
      with Pre => Hex'Length = 128;

end Cerro_Crypto_OpenSSL;
