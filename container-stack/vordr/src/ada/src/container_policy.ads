-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Vörðr Gatekeeper
-- Container security policy specification (formally verified)

pragma SPARK_Mode (On);

package Container_Policy is

   --  Maximum length for string fields
   Max_String_Length : constant := 4096;

   --  Privilege classification for network access
   type Privilege_Level is (Unprivileged, Restricted, Admin);

   --  Linux capabilities (subset of most security-relevant ones)
   type Capability is (
      CAP_CHOWN,            --  Make arbitrary changes to file UIDs and GIDs
      CAP_DAC_OVERRIDE,     --  Bypass file read, write, and execute permission checks
      CAP_FSETID,           --  Set-user-ID and set-group-ID bits preserved
      CAP_FOWNER,           --  Bypass permission checks for operations on file owner
      CAP_MKNOD,            --  Create special files using mknod
      CAP_NET_RAW,          --  Use RAW and PACKET sockets
      CAP_SETGID,           --  Make arbitrary manipulations of process GIDs
      CAP_SETUID,           --  Make arbitrary manipulations of process UIDs
      CAP_SETFCAP,          --  Set file capabilities
      CAP_SETPCAP,          --  Add any capability to the calling process
      CAP_NET_BIND_SERVICE, --  Bind to ports below 1024
      CAP_SYS_CHROOT,       --  Use chroot
      CAP_KILL,             --  Send signals to processes
      CAP_AUDIT_WRITE,      --  Write records to kernel auditing log
      CAP_NET_ADMIN,        --  Perform network administration operations
      CAP_SYS_ADMIN         --  The dangerous catch-all capability
   );

   --  Set of capabilities (indexed boolean array)
   type Capability_Set is array (Capability) of Boolean;

   --  Default unprivileged capability set (OCI defaults minus dangerous ones)
   Default_Capabilities : constant Capability_Set := (
      CAP_CHOWN            => True,
      CAP_DAC_OVERRIDE     => True,
      CAP_FSETID           => True,
      CAP_FOWNER           => True,
      CAP_MKNOD            => True,
      CAP_NET_RAW          => True,
      CAP_SETGID           => True,
      CAP_SETUID           => True,
      CAP_SETFCAP          => True,
      CAP_SETPCAP          => True,
      CAP_NET_BIND_SERVICE => True,
      CAP_SYS_CHROOT       => True,
      CAP_KILL             => True,
      CAP_AUDIT_WRITE      => True,
      CAP_NET_ADMIN        => False,  --  Denied by default
      CAP_SYS_ADMIN        => False   --  Never without explicit privilege
   );

   --  Empty capability set (fully restricted)
   Empty_Capabilities : constant Capability_Set := (others => False);

   --  Sanitised container configuration after parsing
   type Container_Config is record
      Is_Privileged     : Boolean;        --  Run in privileged mode (bypasses checks)
      Root_Read_Only    : Boolean;        --  Root filesystem is read-only
      Capabilities      : Capability_Set; --  Effective capability set
      User_ID           : Natural;        --  UID to run as (0 = root)
      User_Namespace    : Boolean;        --  User namespace enabled
      Network_Mode      : Privilege_Level; --  Network privilege level
      No_New_Privileges : Boolean;        --  Prevent privilege escalation
      Seccomp_Enabled   : Boolean;        --  Seccomp profile applied
   end record;

   --  Default secure configuration
   Default_Config : constant Container_Config := (
      Is_Privileged     => False,
      Root_Read_Only    => True,
      Capabilities      => Default_Capabilities,
      User_ID           => 1000,
      User_Namespace    => True,
      Network_Mode      => Unprivileged,
      No_New_Privileges => True,
      Seccomp_Enabled   => True
   );

   --  GHOST PREDICATE: Mathematical definition of "secure"
   --  This is used only for proof, not at runtime
   function Is_Secure (Config : Container_Config) return Boolean is
     (--  Privileged containers bypass all checks (explicit admin decision)
      Config.Is_Privileged
      or else
      (--  SYS_ADMIN capability requires explicit privilege
       not Config.Capabilities (CAP_SYS_ADMIN)
       --  Root UID (0) only allowed with user namespaces (mapped to non-root)
       and then (Config.User_ID > 0 or Config.User_Namespace)
       --  NET_ADMIN requires at least Restricted network mode
       and then (not Config.Capabilities (CAP_NET_ADMIN)
                 or Config.Network_Mode /= Unprivileged)
       --  If running as root-like, must have no-new-privileges or user NS
       and then (Config.User_ID > 0
                 or Config.No_New_Privileges
                 or Config.User_Namespace)))
   with Ghost;

   --  Validation result codes (used for FFI)
   type Validation_Result is (
      Valid,                    --  Configuration passes all security checks
      Invalid_Capabilities,     --  Dangerous capabilities without privilege
      Invalid_User_Namespace,   --  Root UID without user namespace
      Invalid_Network_Mode,     --  NET_ADMIN without proper network mode
      Invalid_Privilege_Escape, --  Potential privilege escalation path
      Parse_Error,              --  Failed to parse input configuration
      Internal_Error            --  Unexpected internal error
   );

   --  Convert validation result to exit code for FFI
   function To_Exit_Code (Result : Validation_Result) return Integer is
     (case Result is
         when Valid                    => 0,
         when Invalid_Capabilities     => 1,
         when Invalid_User_Namespace   => 2,
         when Invalid_Network_Mode     => 3,
         when Invalid_Privilege_Escape => 4,
         when Parse_Error              => 5,
         when Internal_Error           => -1);

   --  Convert exit code back to validation result
   function From_Exit_Code (Code : Integer) return Validation_Result is
     (case Code is
         when 0      => Valid,
         when 1      => Invalid_Capabilities,
         when 2      => Invalid_User_Namespace,
         when 3      => Invalid_Network_Mode,
         when 4      => Invalid_Privilege_Escape,
         when 5      => Parse_Error,
         when others => Internal_Error);

   --  PRIMARY VALIDATION FUNCTION
   --  Validates a container configuration and returns detailed result
   --
   --  POSTCONDITION: If Valid, the configuration satisfies Is_Secure
   function Validate_Configuration (
      Config : Container_Config
   ) return Validation_Result
     with Post =>
       (if Validate_Configuration'Result = Valid then Is_Secure (Config));

   --  Simplified boolean validation
   function Is_Valid_Configuration (
      Config : Container_Config
   ) return Boolean
     with Post =>
       (if Is_Valid_Configuration'Result then Is_Secure (Config));

   --  Check if a specific capability is safe to grant
   function Is_Safe_Capability (
      Cap           : Capability;
      Is_Privileged : Boolean;
      Network_Mode  : Privilege_Level
   ) return Boolean;

   --  Apply default security hardening to a configuration
   procedure Apply_Security_Defaults (
      Config : in out Container_Config
   )
     with Post => Is_Secure (Config);

   --  Get human-readable description of validation result
   function Get_Error_Message (
      Result : Validation_Result
   ) return String;

private

   --  Internal helper to check capability set validity
   function Has_Dangerous_Capabilities (
      Caps          : Capability_Set;
      Is_Privileged : Boolean;
      Network_Mode  : Privilege_Level
   ) return Boolean is
     ((Caps (CAP_SYS_ADMIN) and not Is_Privileged)
      or (Caps (CAP_NET_ADMIN) and Network_Mode = Unprivileged));

end Container_Policy;
