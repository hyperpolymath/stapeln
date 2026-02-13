-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Vörðr Gatekeeper
-- Container security policy implementation (formally verified)

pragma SPARK_Mode (On);

package body Container_Policy is

   ---------------------------------
   -- Validate_Configuration --
   ---------------------------------

   function Validate_Configuration (
      Config : Container_Config
   ) return Validation_Result
   is
   begin
      --  Privileged containers bypass all security checks
      --  This is an explicit administrator decision
      if Config.Is_Privileged then
         return Valid;
      end if;

      --  Check 1: SYS_ADMIN capability is the most dangerous
      --  It provides almost complete system access
      if Config.Capabilities (CAP_SYS_ADMIN) then
         return Invalid_Capabilities;
      end if;

      --  Check 2: Root UID (0) requires user namespace mapping
      --  Without user namespaces, root in container = root on host
      if Config.User_ID = 0 and not Config.User_Namespace then
         return Invalid_User_Namespace;
      end if;

      --  Check 3: NET_ADMIN requires elevated network privileges
      --  This capability allows network configuration changes
      if Config.Capabilities (CAP_NET_ADMIN)
        and Config.Network_Mode = Unprivileged
      then
         return Invalid_Network_Mode;
      end if;

      --  Check 4: Privilege escalation paths
      --  If running as root-like (UID 0), must have protections
      if Config.User_ID = 0
        and not Config.No_New_Privileges
        and not Config.User_Namespace
      then
         return Invalid_Privilege_Escape;
      end if;

      --  All checks passed
      return Valid;
   end Validate_Configuration;

   ------------------------------
   -- Is_Valid_Configuration --
   ------------------------------

   function Is_Valid_Configuration (
      Config : Container_Config
   ) return Boolean
   is
   begin
      return Validate_Configuration (Config) = Valid;
   end Is_Valid_Configuration;

   -------------------------
   -- Is_Safe_Capability --
   -------------------------

   function Is_Safe_Capability (
      Cap           : Capability;
      Is_Privileged : Boolean;
      Network_Mode  : Privilege_Level
   ) return Boolean
   is
   begin
      --  In privileged mode, all capabilities are allowed
      if Is_Privileged then
         return True;
      end if;

      case Cap is
         when CAP_SYS_ADMIN =>
            --  SYS_ADMIN is never safe in unprivileged mode
            return False;

         when CAP_NET_ADMIN =>
            --  NET_ADMIN requires at least Restricted network mode
            return Network_Mode /= Unprivileged;

         when CAP_CHOWN
            | CAP_DAC_OVERRIDE
            | CAP_FSETID
            | CAP_FOWNER
            | CAP_MKNOD
            | CAP_NET_RAW
            | CAP_SETGID
            | CAP_SETUID
            | CAP_SETFCAP
            | CAP_SETPCAP
            | CAP_NET_BIND_SERVICE
            | CAP_SYS_CHROOT
            | CAP_KILL
            | CAP_AUDIT_WRITE =>
            --  These are considered safe with proper namespacing
            return True;
      end case;
   end Is_Safe_Capability;

   ------------------------------
   -- Apply_Security_Defaults --
   ------------------------------

   procedure Apply_Security_Defaults (
      Config : in out Container_Config
   )
   is
   begin
      --  If not explicitly privileged, apply security hardening
      if not Config.Is_Privileged then
         --  Remove dangerous capabilities
         Config.Capabilities (CAP_SYS_ADMIN) := False;

         --  If running as root, require user namespace
         if Config.User_ID = 0 then
            Config.User_Namespace := True;
         end if;

         --  If NET_ADMIN is requested but network mode is unprivileged,
         --  remove the capability rather than escalate network mode
         if Config.Capabilities (CAP_NET_ADMIN)
           and Config.Network_Mode = Unprivileged
         then
            Config.Capabilities (CAP_NET_ADMIN) := False;
         end if;

         --  Ensure no-new-privileges is set if running as root without NS
         if Config.User_ID = 0 and not Config.User_Namespace then
            Config.No_New_Privileges := True;
         end if;
      end if;
   end Apply_Security_Defaults;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message (
      Result : Validation_Result
   ) return String
   is
   begin
      case Result is
         when Valid =>
            return "Configuration is valid and secure";

         when Invalid_Capabilities =>
            return "SYS_ADMIN capability requires privileged mode";

         when Invalid_User_Namespace =>
            return "Root UID (0) requires user namespace to be enabled";

         when Invalid_Network_Mode =>
            return "NET_ADMIN capability requires Restricted or Admin network mode";

         when Invalid_Privilege_Escape =>
            return "Potential privilege escalation: set no_new_privileges or enable user namespace";

         when Parse_Error =>
            return "Failed to parse container configuration";

         when Internal_Error =>
            return "Internal error in security validation";
      end case;
   end Get_Error_Message;

end Container_Policy;
