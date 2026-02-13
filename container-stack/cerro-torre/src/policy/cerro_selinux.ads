-------------------------------------------------------------------------------
--  Cerro_SELinux - SELinux Policy Generation
--
--  This package handles generation and validation of SELinux policies
--  for Cerro Torre packages, ensuring proper confinement.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Cerro_SELinux is

   ---------------------------------------------------------------------------
   --  Policy Types
   ---------------------------------------------------------------------------

   type Policy_Format is (CIL, TE, PP);
   --  CIL: Common Intermediate Language (modern, preferred)
   --  TE:  Type Enforcement source
   --  PP:  Binary policy package

   type Policy_Result is record
      Success     : Boolean := False;
      Policy_Text : Unbounded_String;
      Errors      : Unbounded_String;
   end record;

   ---------------------------------------------------------------------------
   --  Policy Generation
   ---------------------------------------------------------------------------

   function Generate_Confined_Policy
      (Package_Name : String;
       Executable   : String;
       Format       : Policy_Format := CIL) return Policy_Result;
   --  Generate a basic confined policy for a package.
   --  @param Package_Name The Cerro package name
   --  @param Executable Path to the main executable
   --  @param Format Output format (CIL preferred)
   --  @return Generated policy or errors

   function Generate_Network_Policy
      (Package_Name : String;
       Ports        : String;  -- Comma-separated port list
       Protocol     : String := "tcp") return Policy_Result;
   --  Generate network access rules.
   --  @param Package_Name The package name
   --  @param Ports Ports the package needs to bind/connect
   --  @param Protocol tcp or udp
   --  @return Policy fragment

   ---------------------------------------------------------------------------
   --  Policy Validation
   ---------------------------------------------------------------------------

   type Validation_Status is
      (Valid,
       Syntax_Error,
       Type_Error,
       Conflict,
       Too_Permissive);

   function Validate_Policy
      (Policy : String;
       Format : Policy_Format := CIL) return Validation_Status;
   --  Validate a policy for correctness.
   --  @param Policy The policy text
   --  @param Format The policy format
   --  @return Validation status

   function Check_Permissions
      (Policy : String) return Boolean;
   --  Check that a policy doesn't grant excessive permissions.
   --  Rejects policies that grant:
   --  - unconfined_t access
   --  - Raw network socket access without justification
   --  - Write access to system directories
   --  @param Policy The policy text
   --  @return True if policy is appropriately restrictive

   ---------------------------------------------------------------------------
   --  Policy Installation
   ---------------------------------------------------------------------------

   function Install_Policy
      (Policy_Path : String) return Boolean;
   --  Install a policy module into the running system.
   --  Requires appropriate privileges.
   --  @param Policy_Path Path to compiled .pp file
   --  @return True if installation succeeded

   function Remove_Policy
      (Module_Name : String) return Boolean;
   --  Remove an installed policy module.
   --  @param Module_Name The SELinux module name
   --  @return True if removal succeeded

end Cerro_SELinux;
