-------------------------------------------------------------------------------
--  Cerro_Import_Fedora - Implementation (Stub)
-------------------------------------------------------------------------------

package body Cerro_Import_Fedora is

   function Import_Package
      (Package_Name : String;
       Version      : String := "") return Import_Result
   is
      pragma Unreferenced (Package_Name, Version);
   begin
      --  TODO: Implement dnf download --source equivalent
      return (Status => Package_Not_Found, others => <>);
   end Import_Package;

   function Import_From_Srpm (Srpm_Path : String) return Import_Result is
      pragma Unreferenced (Srpm_Path);
   begin
      --  TODO: Extract and parse SRPM
      return (Status => Parse_Error, others => <>);
   end Import_From_Srpm;

   function Import_From_Koji
      (Package_Name : String;
       Build_Id     : Natural := 0) return Import_Result
   is
      pragma Unreferenced (Package_Name, Build_Id);
   begin
      --  TODO: Query Koji API and import
      return (Status => Package_Not_Found, others => <>);
   end Import_From_Koji;

   function Parse_Spec (Content : String) return Spec_Info is
      pragma Unreferenced (Content);
   begin
      --  TODO: Parse RPM spec file format
      return (others => <>);
   end Parse_Spec;

end Cerro_Import_Fedora;
