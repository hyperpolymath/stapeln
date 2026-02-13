-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Vörðr Gatekeeper - Main Authorization Implementation
-- SPARK-proved container authorization with threshold signatures

pragma SPARK_Mode (On);

package body Gatekeeper is

   --  Create a new authorization request
   function Create_Request (
      Config    : Container_Config;
      Threshold : Positive;
      Signers   : Positive
   ) return Authorization_Request
   is
      Req : Authorization_Request;
   begin
      --  Assign unique ID
      Req.ID := Next_Request_ID;
      if Next_Request_ID < Request_ID'Last then
         Next_Request_ID := Next_Request_ID + 1;
      else
         Next_Request_ID := 1;  --  Wrap around (unlikely in practice)
      end if;

      --  Initialize state
      Req.State := Created;
      Req.Config := Config;

      --  Validate policy upfront
      Req.Policy_Valid := Is_Valid_Configuration (Config);

      --  Create threshold scheme
      Req.Scheme := Create_Scheme (Threshold, Signers);

      return Req;
   end Create_Request;

   --  Submit a signature for a request
   procedure Submit_Signature (
      Req    : in Out Authorization_Request;
      Share  : Signature_Share;
      Result : out Gatekeeper_Decision
   )
   is
      Auth_Result : Authorization_Result;
   begin
      --  Check policy first
      if not Req.Policy_Valid then
         Result := Deny_Policy;
         Req.State := Denied;
         return;
      end if;

      --  Check request state
      if Req.State not in Created | Pending then
         Result := Deny_Invalid_Request;
         return;
      end if;

      --  Add signature to scheme
      Add_Share (Req.Scheme, Share, Auth_Result);

      case Auth_Result is
         when Threshold_Signatures.Authorized =>
            Req.State := Authorized;
            Result := Allow;

         when Threshold_Signatures.Pending =>
            Req.State := Pending;
            Result := Pending_Signatures;

         when Threshold_Signatures.Duplicate_Signer |
              Threshold_Signatures.Invalid_Signature |
              Threshold_Signatures.Invalid_Signer =>
            Result := Deny_Signatures;

         when Threshold_Signatures.Scheme_Full |
              Threshold_Signatures.Invalid_Scheme =>
            Result := Error;
      end case;
   end Submit_Signature;

   --  Get final decision on a request
   function Decide (Req : Authorization_Request) return Gatekeeper_Decision
   is
   begin
      --  Policy must be valid
      if not Req.Policy_Valid then
         return Deny_Policy;
      end if;

      --  Check state
      case Req.State is
         when Authorized =>
            return Allow;

         when Denied | Revoked =>
            return Deny_Policy;

         when Created | Pending =>
            if Is_Authorized (Req.Scheme) then
               return Allow;
            else
               return Pending_Signatures;
            end if;

         when Expired =>
            return Deny_Invalid_Request;
      end case;
   end Decide;

   --  Revoke an authorized request
   procedure Revoke (Req : in Out Authorization_Request)
   is
   begin
      Req.State := Revoked;
   end Revoke;

   --  Get human-readable decision description
   function Decision_Message (D : Gatekeeper_Decision) return String
   is
   begin
      case D is
         when Allow =>
            return "Authorization granted: container execution permitted";
         when Deny_Policy =>
            return "Authorization denied: policy validation failed";
         when Deny_Signatures =>
            return "Authorization denied: signature verification failed";
         when Deny_Invalid_Request =>
            return "Authorization denied: request in invalid state";
         when Pending_Signatures =>
            return "Authorization pending: awaiting additional signatures";
         when Error =>
            return "Authorization error: internal processing failure";
      end case;
   end Decision_Message;

end Gatekeeper;
