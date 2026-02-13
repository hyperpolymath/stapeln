-------------------------------------------------------------------------------
--  LibOQS - Implementation of Ada FFI Bindings
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  This implementation provides:
--    - Memory-safe wrappers around liboqs C functions
--    - Automatic context management with RAII-like patterns
--    - Ada-friendly interfaces for ML-DSA-87 operations
--
--  Error Handling:
--    - All C function failures are mapped to Ada exceptions or status codes
--    - Memory is properly freed even when exceptions occur
--    - Null pointer checks are performed before dereferencing
--
--  Thread Safety:
--    - OQS_SIG contexts are not thread-safe; use separate contexts per thread
--    - The Is_LibOQS_Available check is not fully thread-safe on first call
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System; use System;

package body LibOQS is

   ---------------------------------------------------------------------------
   --  OQS_SIG Structure Layout (for field access)
   --
   --  struct OQS_SIG {
   --      const char *method_name;      -- offset 0
   --      const char *alg_version;      -- offset 8 (64-bit)
   --      uint8_t claimed_nist_level;   -- offset 16
   --      bool euf_cma;                 -- offset 17
   --      bool suf_cma;                 -- offset 18
   --      bool sig_with_ctx_support;    -- offset 19
   --      -- padding to align size_t    -- offset 20-23 (on 64-bit)
   --      size_t length_public_key;     -- offset 24 (64-bit)
   --      size_t length_secret_key;     -- offset 32
   --      size_t length_signature;      -- offset 40
   --      -- function pointers follow
   --  };
   --
   --  Note: These offsets assume 64-bit architecture with natural alignment.
   ---------------------------------------------------------------------------

   --  Record type matching the C structure layout (64-bit)
   type OQS_SIG_Layout is record
      Method_Name          : System.Address;  --  const char*
      Alg_Version          : System.Address;  --  const char*
      Claimed_NIST_Level   : Unsigned_8;
      EUF_CMA              : Interfaces.C.C_bool;
      SUF_CMA              : Interfaces.C.C_bool;
      Sig_With_Ctx_Support : Interfaces.C.C_bool;
      --  Padding bytes handled by alignment
      Length_Public_Key    : size_t;
      Length_Secret_Key    : size_t;
      Length_Signature     : size_t;
      --  Function pointers follow but we don't access them directly
   end record;

   pragma Convention (C, OQS_SIG_Layout);

   --  Access type for reading structure fields
   type OQS_SIG_Layout_Access is access all OQS_SIG_Layout;

   --  Convert OQS_SIG pointer to layout access for field reading
   function To_Layout (Sig : OQS_SIG) return OQS_SIG_Layout_Access is
      function Convert is new Ada.Unchecked_Conversion
        (Source => OQS_SIG, Target => OQS_SIG_Layout_Access);
   begin
      return Convert (Sig);
   end To_Layout;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Is_Null (Sig : OQS_SIG) return Boolean is
   begin
      return Sig = null;
   end Is_Null;

   function To_C_String (S : String) return chars_ptr is
   begin
      return New_String (S);
   end To_C_String;

   ---------------------------------------------------------------------------
   --  Algorithm Availability
   ---------------------------------------------------------------------------

   function Algorithm_Is_Enabled (Algorithm_Name : String) return Boolean is
      C_Name : chars_ptr := New_String (Algorithm_Name);
      Result : int;
   begin
      Result := OQS_SIG_Alg_Is_Enabled (C_Name);
      Free (C_Name);
      return Result /= 0;
   end Algorithm_Is_Enabled;

   ---------------------------------------------------------------------------
   --  Context Management
   ---------------------------------------------------------------------------

   function New_ML_DSA_87_Context return OQS_SIG is
      C_Name : chars_ptr;
      Result : OQS_SIG;
   begin
      --  Try FIPS 204 name first (ML-DSA-87)
      C_Name := New_String ("ML-DSA-87");
      Result := OQS_SIG_New (C_Name);
      Free (C_Name);

      if Result /= null then
         return Result;
      end if;

      --  Fall back to legacy Dilithium5 name for older liboqs versions
      C_Name := New_String ("Dilithium5");
      Result := OQS_SIG_New (C_Name);
      Free (C_Name);

      return Result;
   end New_ML_DSA_87_Context;

   ---------------------------------------------------------------------------
   --  Structure Field Accessors
   ---------------------------------------------------------------------------

   function Get_Public_Key_Length (Sig : OQS_SIG) return size_t is
      Layout : OQS_SIG_Layout_Access;
   begin
      if Sig = null then
         return 0;
      end if;
      Layout := To_Layout (Sig);
      return Layout.Length_Public_Key;
   end Get_Public_Key_Length;

   function Get_Secret_Key_Length (Sig : OQS_SIG) return size_t is
      Layout : OQS_SIG_Layout_Access;
   begin
      if Sig = null then
         return 0;
      end if;
      Layout := To_Layout (Sig);
      return Layout.Length_Secret_Key;
   end Get_Secret_Key_Length;

   function Get_Signature_Length (Sig : OQS_SIG) return size_t is
      Layout : OQS_SIG_Layout_Access;
   begin
      if Sig = null then
         return 0;
      end if;
      Layout := To_Layout (Sig);
      return Layout.Length_Signature;
   end Get_Signature_Length;

   function Get_Method_Name (Sig : OQS_SIG) return String is
      Layout : OQS_SIG_Layout_Access;
      C_Str  : chars_ptr;
   begin
      if Sig = null then
         return "";
      end if;

      Layout := To_Layout (Sig);

      if Layout.Method_Name = System.Null_Address then
         return "";
      end if;

      --  Overlay chars_ptr on the address
      declare
         function To_Chars_Ptr is new Ada.Unchecked_Conversion
           (Source => System.Address, Target => chars_ptr);
      begin
         C_Str := To_Chars_Ptr (Layout.Method_Name);
         if C_Str = Null_Ptr then
            return "";
         end if;
         return Value (C_Str);
      end;
   end Get_Method_Name;

   ---------------------------------------------------------------------------
   --  Key Generation
   ---------------------------------------------------------------------------

   function Generate_ML_DSA_87_Keypair return Keypair_Result is
      Context : OQS_SIG;
      Status  : OQS_STATUS;
      Result  : Keypair_Result;
   begin
      --  Initialize result with zeros
      Result.Status     := OQS_ERROR;
      Result.Public_Key := (others => 0);
      Result.Secret_Key := (others => 0);

      --  Create context
      Context := New_ML_DSA_87_Context;
      if Context = null then
         return Result;
      end if;

      --  Verify key sizes match our expectations
      if Get_Public_Key_Length (Context) /= ML_DSA_87_Public_Key_Bytes or
         Get_Secret_Key_Length (Context) /= ML_DSA_87_Secret_Key_Bytes
      then
         OQS_SIG_Free (Context);
         return Result;
      end if;

      --  Generate keypair
      Status := OQS_SIG_Keypair
        (Sig        => Context,
         Public_Key => Result.Public_Key'Address,
         Secret_Key => Result.Secret_Key'Address);

      Result.Status := Status;

      --  Clean up context
      OQS_SIG_Free (Context);

      --  Clear secret key on failure
      if Status /= OQS_SUCCESS then
         Result.Secret_Key := (others => 0);
         Result.Public_Key := (others => 0);
      end if;

      return Result;
   end Generate_ML_DSA_87_Keypair;

   ---------------------------------------------------------------------------
   --  Signing
   ---------------------------------------------------------------------------

   function Sign_ML_DSA_87
     (Message    : String;
      Secret_Key : ML_DSA_87_Secret_Key_Array) return Sign_Result
   is
      Context : OQS_SIG;
      Status  : OQS_STATUS;
      Result  : Sign_Result;
      Sig_Len : aliased size_t;

      --  Local copy of secret key for address stability
      SK_Copy : ML_DSA_87_Secret_Key_Array := Secret_Key;

      --  Message as byte array
      Msg_Bytes : Unsigned_8_Array (1 .. Message'Length);
   begin
      --  Initialize result
      Result.Status    := OQS_ERROR;
      Result.Signature := (others => 0);
      Result.Sig_Len   := 0;

      --  Convert message to bytes
      for I in Message'Range loop
         Msg_Bytes (I - Message'First + 1) := Character'Pos (Message (I));
      end loop;

      --  Create context
      Context := New_ML_DSA_87_Context;
      if Context = null then
         SK_Copy := (others => 0);
         return Result;
      end if;

      --  Verify signature size matches
      if Get_Signature_Length (Context) /= ML_DSA_87_Signature_Bytes then
         OQS_SIG_Free (Context);
         SK_Copy := (others => 0);
         return Result;
      end if;

      --  Sign the message
      Sig_Len := size_t (ML_DSA_87_Signature_Bytes);

      if Message'Length = 0 then
         --  Handle empty message
         Status := OQS_SIG_Sign
           (Sig           => Context,
            Signature     => Result.Signature'Address,
            Signature_Len => Sig_Len'Access,
            Message       => System.Null_Address,
            Message_Len   => 0,
            Secret_Key    => SK_Copy'Address);
      else
         Status := OQS_SIG_Sign
           (Sig           => Context,
            Signature     => Result.Signature'Address,
            Signature_Len => Sig_Len'Access,
            Message       => Msg_Bytes'Address,
            Message_Len   => size_t (Message'Length),
            Secret_Key    => SK_Copy'Address);
      end if;

      Result.Status  := Status;
      Result.Sig_Len := Natural (Sig_Len);

      --  Clean up
      OQS_SIG_Free (Context);

      --  Clear signature on failure
      if Status /= OQS_SUCCESS then
         Result.Signature := (others => 0);
         Result.Sig_Len   := 0;
      end if;

      --  Clear sensitive data
      SK_Copy := (others => 0);

      return Result;
   end Sign_ML_DSA_87;

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   function Verify_ML_DSA_87
     (Message    : String;
      Signature  : ML_DSA_87_Signature_Array;
      Public_Key : ML_DSA_87_Public_Key_Array) return Boolean
   is
      Context : OQS_SIG;
      Status  : OQS_STATUS;

      --  Local copies for address stability
      Sig_Copy : ML_DSA_87_Signature_Array  := Signature;
      PK_Copy  : ML_DSA_87_Public_Key_Array := Public_Key;

      --  Message as byte array
      Msg_Bytes : Unsigned_8_Array (1 .. Message'Length);
   begin
      --  Convert message to bytes
      for I in Message'Range loop
         Msg_Bytes (I - Message'First + 1) := Character'Pos (Message (I));
      end loop;

      --  Create context
      Context := New_ML_DSA_87_Context;
      if Context = null then
         return False;
      end if;

      --  Verify the signature
      if Message'Length = 0 then
         --  Handle empty message
         Status := OQS_SIG_Verify
           (Sig           => Context,
            Message       => System.Null_Address,
            Message_Len   => 0,
            Signature     => Sig_Copy'Address,
            Signature_Len => size_t (ML_DSA_87_Signature_Bytes),
            Public_Key    => PK_Copy'Address);
      else
         Status := OQS_SIG_Verify
           (Sig           => Context,
            Message       => Msg_Bytes'Address,
            Message_Len   => size_t (Message'Length),
            Signature     => Sig_Copy'Address,
            Signature_Len => size_t (ML_DSA_87_Signature_Bytes),
            Public_Key    => PK_Copy'Address);
      end if;

      --  Clean up
      OQS_SIG_Free (Context);

      return Status = OQS_SUCCESS;
   end Verify_ML_DSA_87;

   ---------------------------------------------------------------------------
   --  Library Availability
   ---------------------------------------------------------------------------

   function Is_LibOQS_Available return Boolean is
      Context : OQS_SIG;
   begin
      --  Check cached result
      if LibOQS_Checked then
         return LibOQS_Available;
      end if;

      --  Try to create an ML-DSA-87 context
      Context := New_ML_DSA_87_Context;

      if Context /= null then
         --  Verify the expected key sizes
         LibOQS_Available :=
           Get_Public_Key_Length (Context) = ML_DSA_87_Public_Key_Bytes and
           Get_Secret_Key_Length (Context) = ML_DSA_87_Secret_Key_Bytes and
           Get_Signature_Length (Context) = ML_DSA_87_Signature_Bytes;

         OQS_SIG_Free (Context);
      else
         LibOQS_Available := False;
      end if;

      LibOQS_Checked := True;
      return LibOQS_Available;
   end Is_LibOQS_Available;

   --  Version string retrieval
   --  Note: liboqs provides OQS_version_text() but we don't import it
   --  to minimize dependencies. Return a placeholder.
   function Get_LibOQS_Version return String is
   begin
      if Is_LibOQS_Available then
         return "liboqs (version unknown)";
      else
         return "liboqs not available";
      end if;
   end Get_LibOQS_Version;

end LibOQS;
