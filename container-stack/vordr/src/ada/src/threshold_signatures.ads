-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Vörðr Gatekeeper - Threshold Signature Scheme
-- SPARK-proved (k,n) threshold authorization

pragma SPARK_Mode (On);

with Ada.Containers.Formal_Vectors;

package Threshold_Signatures is

   --  Maximum signers in a threshold scheme
   Max_Signers : constant := 16;

   --  Maximum threshold value
   Max_Threshold : constant := Max_Signers;

   --  Signature identifier (64 bytes - SHA-512 digest)
   type Signature_ID is array (1 .. 64) of Character;

   --  Empty signature ID (all zeros)
   Null_Signature_ID : constant Signature_ID := (others => Character'Val (0));

   --  Signer identifier (public key fingerprint)
   type Signer_ID is array (1 .. 32) of Character;

   --  Empty signer ID
   Null_Signer_ID : constant Signer_ID := (others => Character'Val (0));

   --  Individual signature share
   type Signature_Share is record
      Signer    : Signer_ID;
      Signature : Signature_ID;
      Valid     : Boolean;
   end record;

   --  Null share
   Null_Share : constant Signature_Share := (
      Signer    => Null_Signer_ID,
      Signature => Null_Signature_ID,
      Valid     => False
   );

   --  Collection of signature shares (up to Max_Signers)
   type Share_Index is range 1 .. Max_Signers;
   type Signature_Shares is array (Share_Index) of Signature_Share;

   --  Threshold scheme configuration
   type Threshold_Scheme is record
      Threshold     : Positive;          --  k: minimum signatures required
      Total_Signers : Positive;          --  n: total authorized signers
      Shares        : Signature_Shares;  --  Collected signatures
      Share_Count   : Natural;           --  Number of valid shares collected
   end record;

   --  GHOST PREDICATE: Valid threshold configuration (k <= n)
   function Is_Valid_Scheme (Scheme : Threshold_Scheme) return Boolean is
     (Scheme.Threshold <= Scheme.Total_Signers
      and Scheme.Total_Signers <= Max_Signers
      and Scheme.Threshold >= 1
      and Scheme.Share_Count <= Scheme.Total_Signers)
   with Ghost;

   --  GHOST PREDICATE: Threshold reached
   function Threshold_Reached (Scheme : Threshold_Scheme) return Boolean is
     (Scheme.Share_Count >= Scheme.Threshold)
   with Ghost;

   --  Authorization result
   type Authorization_Result is (
      Authorized,          --  Threshold reached, action authorized
      Pending,             --  Threshold not yet reached
      Duplicate_Signer,    --  Signer already contributed
      Invalid_Signature,   --  Signature verification failed
      Invalid_Signer,      --  Signer not in authorized set
      Scheme_Full,         --  All signers have contributed
      Invalid_Scheme       --  Scheme configuration invalid
   );

   --  Create a new (k,n) threshold scheme
   function Create_Scheme (
      Threshold     : Positive;
      Total_Signers : Positive
   ) return Threshold_Scheme
     with Pre  => Threshold <= Total_Signers
                  and Total_Signers <= Max_Signers,
          Post => Is_Valid_Scheme (Create_Scheme'Result)
                  and Create_Scheme'Result.Share_Count = 0;

   --  Add a signature share to the scheme
   procedure Add_Share (
      Scheme : in out Threshold_Scheme;
      Share  : Signature_Share;
      Result : out Authorization_Result
   )
     with Pre  => Is_Valid_Scheme (Scheme),
          Post => Is_Valid_Scheme (Scheme)
                  and (if Result = Authorized then Threshold_Reached (Scheme));

   --  Check if a signer has already contributed
   function Has_Signer (
      Scheme : Threshold_Scheme;
      Signer : Signer_ID
   ) return Boolean
     with Pre => Is_Valid_Scheme (Scheme);

   --  Get current share count
   function Get_Share_Count (Scheme : Threshold_Scheme) return Natural is
     (Scheme.Share_Count)
     with Pre => Is_Valid_Scheme (Scheme);

   --  Check if threshold is reached
   function Is_Authorized (Scheme : Threshold_Scheme) return Boolean is
     (Scheme.Share_Count >= Scheme.Threshold)
     with Pre => Is_Valid_Scheme (Scheme);

   --  Reset scheme (clear all shares)
   procedure Reset_Scheme (Scheme : in out Threshold_Scheme)
     with Pre  => Is_Valid_Scheme (Scheme),
          Post => Is_Valid_Scheme (Scheme)
                  and Scheme.Share_Count = 0;

   --  Verify a signature share (stub - actual crypto in external library)
   function Verify_Share (
      Share   : Signature_Share;
      Message : String
   ) return Boolean
     with Pre => Share.Valid and Message'Length > 0;

end Threshold_Signatures;
