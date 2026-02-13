--  ct_test_crypto - Test program for crypto implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Verifies SHA-256 and SHA-512 implementations against known test vectors

with Ada.Text_IO;
with Cerro_Crypto;

procedure CT_Test_Crypto is
   use Ada.Text_IO;
   use Cerro_Crypto;

   --  Test vectors from NIST FIPS 180-4
   --  SHA-256("") = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
   --  SHA-256("abc") = ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
   --  SHA-512("abc") = ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a...

   function Test_SHA256 (Input : String; Expected_Hex : String) return Boolean is
      Result : constant SHA256_Digest := Compute_SHA256 (Input);
      Hex    : constant String := Bytes_To_Hex (Result);
   begin
      Put ("  SHA256(""" & Input & """) = ");
      Put_Line (Hex);
      if Hex = Expected_Hex then
         Put_Line ("  ✓ PASS");
         return True;
      else
         Put_Line ("  ✗ FAIL - expected: " & Expected_Hex);
         return False;
      end if;
   end Test_SHA256;

   function Test_SHA512 (Input : String; Expected_Hex : String) return Boolean is
      Result : constant SHA512_Digest := Compute_SHA512 (Input);
      Hex    : constant String := Bytes_To_Hex_512 (Result);
   begin
      Put ("  SHA512(""" & Input & """) = ");
      Put_Line (Hex (1 .. 64) & "...");
      if Hex = Expected_Hex then
         Put_Line ("  ✓ PASS");
         return True;
      else
         Put_Line ("  ✗ FAIL");
         Put_Line ("  expected: " & Expected_Hex (1 .. 64) & "...");
         return False;
      end if;
   end Test_SHA512;

   Pass_Count : Natural := 0;
   Fail_Count : Natural := 0;
begin
   Put_Line ("=== SHA-256 Test Vectors ===");
   Put_Line ("");

   --  Test 1: Empty string
   if Test_SHA256 ("", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855") then
      Pass_Count := Pass_Count + 1;
   else
      Fail_Count := Fail_Count + 1;
   end if;
   Put_Line ("");

   --  Test 2: "abc"
   if Test_SHA256 ("abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad") then
      Pass_Count := Pass_Count + 1;
   else
      Fail_Count := Fail_Count + 1;
   end if;
   Put_Line ("");

   --  Test 3: "hello"
   if Test_SHA256 ("hello", "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824") then
      Pass_Count := Pass_Count + 1;
   else
      Fail_Count := Fail_Count + 1;
   end if;
   Put_Line ("");

   --  Test 4: Longer string (tests multi-block processing)
   if Test_SHA256 ("The quick brown fox jumps over the lazy dog",
                 "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592") then
      Pass_Count := Pass_Count + 1;
   else
      Fail_Count := Fail_Count + 1;
   end if;
   Put_Line ("");

   --  SHA-512 Test Vectors
   Put_Line ("=== SHA-512 Test Vectors ===");
   Put_Line ("");

   --  Test 5: Empty string
   --  SHA-512("") = cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e
   if Test_SHA512 ("", "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e") then
      Pass_Count := Pass_Count + 1;
   else
      Fail_Count := Fail_Count + 1;
   end if;
   Put_Line ("");

   --  Test 6: "abc"
   --  SHA-512("abc") = ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f
   if Test_SHA512 ("abc", "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f") then
      Pass_Count := Pass_Count + 1;
   else
      Fail_Count := Fail_Count + 1;
   end if;
   Put_Line ("");

   --  Test 7: Longer string
   --  SHA-512("The quick brown fox...") = 07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6
   if Test_SHA512 ("The quick brown fox jumps over the lazy dog",
                   "07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6") then
      Pass_Count := Pass_Count + 1;
   else
      Fail_Count := Fail_Count + 1;
   end if;
   Put_Line ("");

   --  Summary
   Put_Line ("=== Results ===");
   Put_Line ("Passed:" & Natural'Image (Pass_Count));
   Put_Line ("Failed:" & Natural'Image (Fail_Count));

   if Fail_Count = 0 then
      Put_Line ("✓ All tests passed!");
   else
      Put_Line ("✗ Some tests failed");
   end if;
end CT_Test_Crypto;
