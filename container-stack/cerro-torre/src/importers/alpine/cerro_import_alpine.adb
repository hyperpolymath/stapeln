-------------------------------------------------------------------------------
--  Cerro_Import_Alpine - Implementation (Stub)
-------------------------------------------------------------------------------

package body Cerro_Import_Alpine is

   function Import_Package
      (Package_Name : String;
       Version      : String := "") return Import_Result
   is
      pragma Unreferenced (Package_Name, Version);
   begin
      --  TODO: Implement aports import
      return (Status => Package_Not_Found, others => <>);
   end Import_Package;

   function Import_From_Apkbuild (Path : String) return Import_Result is
      pragma Unreferenced (Path);
   begin
      --  TODO: Parse APKBUILD shell script
      return (Status => Parse_Error, others => <>);
   end Import_From_Apkbuild;

   function Import_From_Aports
      (Package_Name : String;
       Branch       : String := "master") return Import_Result
   is
      pragma Unreferenced (Package_Name, Branch);
   begin
      --  TODO: Clone/fetch aports and import
      return (Status => Package_Not_Found, others => <>);
   end Import_From_Aports;

   function Parse_Apkbuild (Content : String) return Apkbuild_Info is
      pragma Unreferenced (Content);
   begin
      --  TODO: Parse APKBUILD (bash-like shell script)
      return (others => <>);
   end Parse_Apkbuild;

end Cerro_Import_Alpine;
