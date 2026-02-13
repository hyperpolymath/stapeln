-------------------------------------------------------------------------------
--  Cerro_Export_OSTree - Implementation (Stub)
-------------------------------------------------------------------------------

package body Cerro_Export_OSTree is

   function Export_Package
      (M      : Manifest;
       Config : Repo_Config) return Export_Result
   is
      pragma Unreferenced (M, Config);
   begin
      --  TODO: Implement OSTree export
      return (Status => Invalid_Manifest, others => <>);
   end Export_Package;

   function Init_Repository (Config : Repo_Config) return Boolean is
      pragma Unreferenced (Config);
   begin
      --  TODO: Run ostree init
      return False;
   end Init_Repository;

   function Commit_Tree
      (Repo_Path : String;
       Tree_Path : String;
       Subject   : String;
       Ref       : String) return Export_Result
   is
      pragma Unreferenced (Repo_Path, Tree_Path, Subject, Ref);
   begin
      --  TODO: Run ostree commit
      return (Status => Commit_Failed, others => <>);
   end Commit_Tree;

   function Create_Treefile (M : Manifest) return String is
      pragma Unreferenced (M);
   begin
      --  TODO: Generate rpm-ostree treefile YAML
      return "";
   end Create_Treefile;

   function Compose_Tree
      (Treefile  : String;
       Repo_Path : String) return Export_Result
   is
      pragma Unreferenced (Treefile, Repo_Path);
   begin
      --  TODO: Run rpm-ostree compose tree
      return (Status => Commit_Failed, others => <>);
   end Compose_Tree;

   function Generate_Static_Delta
      (Repo_Path : String;
       From_Rev  : String;
       To_Rev    : String) return Boolean
   is
      pragma Unreferenced (Repo_Path, From_Rev, To_Rev);
   begin
      --  TODO: Run ostree static-delta generate
      return False;
   end Generate_Static_Delta;

   function Update_Summary
      (Repo_Path : String;
       GPG_Key   : String := "") return Boolean
   is
      pragma Unreferenced (Repo_Path, GPG_Key);
   begin
      --  TODO: Run ostree summary -u
      return False;
   end Update_Summary;

end Cerro_Export_OSTree;
