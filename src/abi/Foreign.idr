-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Stapeln FFI declarations (Idris2 -> Zig/C ABI)

module Stapeln.ABI.Foreign

import Data.Bits
import Stapeln.ABI.Types

%default total

public export
%foreign "C:stapeln_contract_version, libstapeln_ffi"
prim__contractVersion : PrimIO String

export
contractVersion : IO String
contractVersion = primIO prim__contractVersion

public export
%foreign "C:stapeln_create_stack_json, libstapeln_ffi"
prim__createStackJson : Bits64 -> Bits64 -> Bits64 -> Bits64 -> PrimIO Int

public export
%foreign "C:stapeln_get_stack_json, libstapeln_ffi"
prim__getStackJson : Bits64 -> Bits64 -> Bits64 -> PrimIO Int

public export
%foreign "C:stapeln_update_stack_json, libstapeln_ffi"
prim__updateStackJson : Bits64 -> Bits64 -> Bits64 -> Bits64 -> Bits64 -> PrimIO Int

public export
%foreign "C:stapeln_validate_stack_json, libstapeln_ffi"
prim__validateStackJson : Bits64 -> Bits64 -> Bits64 -> PrimIO Int

public export
%foreign "C:stapeln_list_stacks_json, libstapeln_ffi"
prim__listStacksJson : Bits64 -> Bits64 -> PrimIO Int

public export
%foreign "C:stapeln_free_buffer, libstapeln_ffi"
prim__freeBuffer : Bits64 -> Bits64 -> PrimIO ()

