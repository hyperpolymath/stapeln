--  SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later
--  MVP build pipeline (Ada orchestration)

package CT_MVP_Build is

   procedure Run (Manifest_Path : String; Out_Dir : String; Attach : Boolean);

end CT_MVP_Build;
