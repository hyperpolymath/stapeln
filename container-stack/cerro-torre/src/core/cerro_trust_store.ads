--  Cerro_Trust_Store - Key and trust management
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  Manages trusted public keys for bundle verification.
--  Keys stored in ~/.config/cerro-torre/trust/
--
--  Key formats:
--    .pub   - Ed25519 public key (32 bytes, hex or base64)
--    .meta  - Key metadata (TOML-like: id, created, fingerprint, trust-level)
--
--  Trust levels:
--    untrusted  - Key known but not trusted (default on import)
--    marginal   - Partially trusted (e.g., community key)
--    full       - Fully trusted (e.g., upstream maintainer)
--    ultimate   - Ultimate trust (e.g., your own keys)

package Cerro_Trust_Store is

   --  Trust levels for keys
   type Trust_Level is (Untrusted, Marginal, Full, Ultimate);

   --  Key record
   type Key_Info is record
      Key_Id      : String (1 .. 64);   --  Identifier (e.g., "nginx-upstream")
      Key_Id_Len  : Natural := 0;
      Fingerprint : String (1 .. 64);   --  SHA-256 of public key (hex)
      Finger_Len  : Natural := 0;
      Public_Key  : String (1 .. 64);   --  Ed25519 pubkey (hex, 64 chars)
      Pubkey_Len  : Natural := 0;
      Trust       : Trust_Level := Untrusted;
      Created     : String (1 .. 24);   --  ISO 8601 timestamp
      Created_Len : Natural := 0;
   end record;

   --  Result type for operations
   type Store_Result is (OK, Not_Found, Already_Exists, IO_Error, Invalid_Key, Invalid_Format);

   --  Initialize trust store (create dirs if needed)
   procedure Initialize;
   function Is_Initialized return Boolean;

   --  Get trust store path
   function Get_Store_Path return String;

   --  Key operations
   function Import_Key (Path : String; Key_Id : String := "") return Store_Result;
   function Import_Key_Hex (Hex_Key : String; Key_Id : String) return Store_Result;
   function Export_Key (Key_Id : String; Output_Path : String) return Store_Result;
   function Delete_Key (Key_Id : String) return Store_Result;
   function Set_Trust (Key_Id : String; Level : Trust_Level) return Store_Result;

   --  Query operations
   function Key_Count return Natural;
   function Get_Key (Key_Id : String; Info : out Key_Info) return Store_Result;
   function Get_Key_By_Fingerprint (Fingerprint : String; Info : out Key_Info) return Store_Result;
   function Is_Trusted (Fingerprint : String) return Boolean;
   function Get_Trust_Level (Fingerprint : String) return Trust_Level;

   --  List keys (callback-based iteration)
   type Key_Callback is access procedure (Info : Key_Info);
   procedure For_Each_Key (Callback : Key_Callback);

   --  Default signing key
   function Get_Default_Key return String;  --  Returns Key_Id or ""
   function Set_Default_Key (Key_Id : String) return Store_Result;

   --  Utility: compute fingerprint from public key
   function Compute_Fingerprint (Public_Key_Hex : String) return String;

end Cerro_Trust_Store;
