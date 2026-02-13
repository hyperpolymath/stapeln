-------------------------------------------------------------------------------
--  Cerro_Provenance - Implementation
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Cerro_Provenance is

   ---------------------------------------------------------------------------
   --  Verify_Upstream_Hash
   ---------------------------------------------------------------------------

   function Verify_Upstream_Hash
      (Prov : Provenance;
       Data : String) return Boolean
   is
      Expected : constant String := To_String (Prov.Upstream_Hash.Digest);
      Computed : Digest_256;
   begin
      case Prov.Upstream_Hash.Algorithm is
         when SHA256 =>
            Computed := SHA256_Hash (Data);
            declare
               Computed_Hex : constant String := Digest_To_Hex (Computed);
            begin
               return Computed_Hex = Expected;
            end;
         when others =>
            --  TODO: Implement other hash algorithms
            return False;
      end case;
   end Verify_Upstream_Hash;

   ---------------------------------------------------------------------------
   --  Verify_Patch_Hashes
   ---------------------------------------------------------------------------

   function Verify_Patch_Hashes
      (Prov       : Provenance;
       Patch_Data : String_Vectors.Vector) return Verification_Result
   is
      Result : Verification_Result;
   begin
      Result.Status := Verified;
      Result.Failed_Item := 0;
      Result.Message_Len := 0;

      for I in 1 .. Natural (Prov.Patches.Length) loop
         declare
            Patch    : constant Patch_Entry := Prov.Patches (I);
            Expected : constant String := To_String (Patch.Hash.Digest);
            Data     : constant String := To_String (Patch_Data (I));
            Computed : Digest_256;
         begin
            case Patch.Hash.Algorithm is
               when SHA256 =>
                  Computed := SHA256_Hash (Data);
                  declare
                     Computed_Hex : constant String := Digest_To_Hex (Computed);
                  begin
                     if Computed_Hex /= Expected then
                        Result.Status := Patch_Hash_Mismatch;
                        Result.Failed_Item := I;
                        return Result;
                     end if;
                  end;
               when others =>
                  Result.Status := Patch_Hash_Mismatch;
                  Result.Failed_Item := I;
                  return Result;
            end case;
         end;
      end loop;

      return Result;
   end Verify_Patch_Hashes;

   ---------------------------------------------------------------------------
   --  Verify_Chain_Integrity
   ---------------------------------------------------------------------------

   function Verify_Chain_Integrity (M : Manifest) return Verification_Result is
      Result : Verification_Result;
   begin
      Result.Status := Verified;
      Result.Failed_Item := 0;
      Result.Message_Len := 0;

      --  Check upstream URL is present
      if Length (M.Source.Upstream_URL) = 0 then
         Result.Status := Chain_Broken;
         return Result;
      end if;

      --  Check upstream hash is valid
      if not Is_Valid_Hash (M.Source.Upstream_Hash) then
         Result.Status := Chain_Broken;
         return Result;
      end if;

      --  Check imported-from is present
      if Length (M.Source.Imported_From) = 0 then
         Result.Status := Chain_Broken;
         return Result;
      end if;

      --  Check all patch hashes are valid format
      for I in 1 .. Natural (M.Source.Patches.Length) loop
         if not Is_Valid_Hash (M.Source.Patches (I).Hash) then
            Result.Status := Patch_Hash_Mismatch;
            Result.Failed_Item := I;
            return Result;
         end if;
      end loop;

      return Result;
   end Verify_Chain_Integrity;

   ---------------------------------------------------------------------------
   --  Verify_Manifest_Signature
   ---------------------------------------------------------------------------

   function Verify_Manifest_Signature
      (M         : Manifest;
       Sig_Index : Positive) return Boolean
   is
      pragma Unreferenced (M, Sig_Index);
   begin
      --  TODO: Implement actual signature verification
      --  1. Serialize manifest without signatures section
      --  2. Hash the serialized content
      --  3. Verify Ed25519 signature against the hash

      return False;
   end Verify_Manifest_Signature;

   ---------------------------------------------------------------------------
   --  Count_Valid_Signatures
   ---------------------------------------------------------------------------

   function Count_Valid_Signatures (M : Manifest) return Natural is
      Count : Natural := 0;
   begin
      for I in 1 .. Natural (M.Signatures.Length) loop
         if Verify_Manifest_Signature (M, I) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Valid_Signatures;

   ---------------------------------------------------------------------------
   --  Has_Required_Attestations
   ---------------------------------------------------------------------------

   function Has_Required_Attestations (M : Manifest) return Boolean is
      Has_Build : Boolean := False;
   begin
      for Att of M.Signatures loop
         if Att.Kind = Build then
            if Get_Signer_Trust (To_String (Att.Public_Key)) >= Trusted then
               Has_Build := True;
            end if;
         end if;
      end loop;

      return Has_Build;
   end Has_Required_Attestations;

   ---------------------------------------------------------------------------
   --  Get_Signer_Trust
   ---------------------------------------------------------------------------

   function Get_Signer_Trust (Public_Key : String) return Trust_Level is
      pragma Unreferenced (Public_Key);
   begin
      --  TODO: Implement trust store lookup
      --  For now, return Untrusted for all keys
      return Untrusted;
   end Get_Signer_Trust;

   ---------------------------------------------------------------------------
   --  Generate_Provenance_Report
   ---------------------------------------------------------------------------

   function Generate_Provenance_Report (M : Manifest) return String is
      pragma SPARK_Mode (Off);  -- String concatenation not in SPARK subset

      LF : constant Character := Character'Val (10);
   begin
      return
         "Provenance Report for " & To_String (M.Name) & LF &
         "=================" & LF &
         LF &
         "Upstream: " & To_String (M.Source.Upstream_URL) & LF &
         "Hash: " & To_String (M.Source.Upstream_Hash.Digest) & LF &
         "Imported from: " & To_String (M.Source.Imported_From) & LF &
         "Patches applied: " & Natural'Image (Natural (M.Source.Patches.Length)) & LF;
   end Generate_Provenance_Report;

end Cerro_Provenance;
