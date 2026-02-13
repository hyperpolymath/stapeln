-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Vörðr Gatekeeper - Main Authorization Coordinator
-- SPARK-proved container authorization with threshold signatures

pragma SPARK_Mode (On);

with Container_Policy;   use Container_Policy;
with Threshold_Signatures; use Threshold_Signatures;

package Gatekeeper is

   --  Maximum pending authorization requests
   Max_Pending_Requests : constant := 256;

   --  Request identifier (unique per authorization attempt)
   type Request_ID is mod 2**64;

   --  Null request
   Null_Request_ID : constant Request_ID := 0;

   --  Authorization request state
   type Request_State is (
      Created,       --  Request created, awaiting signatures
      Pending,       --  Signatures being collected
      Authorized,    --  Threshold reached, request approved
      Denied,        --  Explicitly denied (policy violation)
      Expired,       --  Request timed out
      Revoked        --  Authorization revoked after grant
   );

   --  Authorization request
   type Authorization_Request is record
      ID           : Request_ID;
      State        : Request_State;
      Config       : Container_Config;
      Policy_Valid : Boolean;
      Scheme       : Threshold_Scheme;
   end record;

   --  Gatekeeper decision result
   type Gatekeeper_Decision is (
      Allow,                --  Container execution permitted
      Deny_Policy,          --  Policy validation failed
      Deny_Signatures,      --  Insufficient signatures
      Deny_Invalid_Request, --  Request in invalid state
      Pending_Signatures,   --  Awaiting more signatures
      Error                 --  Internal error
   );

   --  GHOST PREDICATE: Request is properly formed
   function Is_Valid_Request (Req : Authorization_Request) return Boolean is
     (Req.ID /= Null_Request_ID
      and Is_Valid_Scheme (Req.Scheme))
   with Ghost;

   --  GHOST PREDICATE: Request can be authorized
   function Can_Authorize (Req : Authorization_Request) return Boolean is
     (Is_Valid_Request (Req)
      and Req.Policy_Valid
      and Is_Authorized (Req.Scheme))
   with Ghost;

   --  Create a new authorization request
   function Create_Request (
      Config    : Container_Config;
      Threshold : Positive;
      Signers   : Positive
   ) return Authorization_Request
     with Pre  => Threshold <= Signers
                  and Signers <= Max_Signers,
          Post => Is_Valid_Request (Create_Request'Result)
                  and Create_Request'Result.State = Created;

   --  Submit a signature for a request
   procedure Submit_Signature (
      Req    : in out Authorization_Request;
      Share  : Signature_Share;
      Result : out Gatekeeper_Decision
   )
     with Pre  => Is_Valid_Request (Req),
          Post => Is_Valid_Request (Req);

   --  Get final decision on a request
   function Decide (Req : Authorization_Request) return Gatekeeper_Decision
     with Pre => Is_Valid_Request (Req);

   --  Check if request is authorized
   function Is_Request_Authorized (
      Req : Authorization_Request
   ) return Boolean is
     (Req.State = Authorized)
     with Pre => Is_Valid_Request (Req);

   --  Revoke an authorized request
   procedure Revoke (Req : in Out Authorization_Request)
     with Pre  => Is_Valid_Request (Req)
                  and Req.State = Authorized,
          Post => Is_Valid_Request (Req)
                  and Req.State = Revoked;

   --  Get human-readable decision description
   function Decision_Message (D : Gatekeeper_Decision) return String;

private

   --  Internal request ID counter
   Next_Request_ID : Request_ID := 1;

end Gatekeeper;
