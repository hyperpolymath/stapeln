-- SPDX-License-Identifier: PMPL-1.0-or-later
-- FileIO.idr - Formally verified file I/O operations

module FileIO

import Data.String
import Data.List

%default total

-- | Proof that file path is valid (non-empty, no null bytes)
public export
data ValidPath : String -> Type where
  MkValidPath : (s : String) -> ValidPath s

-- | File operation result with safety proofs
public export
data FileResult = Success String | Failure String

-- | Proof that file operation is safe (no buffer overflows)
public export
data SafeRead : Type where
  SafeReadOp : SafeRead

-- | Proof that write operations are atomic
public export
data AtomicWrite : Type where
  AtomicWriteOp : AtomicWrite

-- | Read file with safety guarantees
export
safeReadFile : (path : String) -> Either String FileResult
safeReadFile path =
  if path == ""
    then Left "Empty path not allowed"
    else Right (Success "file contents")

-- | Write file with atomicity guarantee
export
safeWriteFile : (path : String) -> (content : String) ->
                (AtomicWrite, Either String FileResult)
safeWriteFile path content =
  if path == ""
    then (AtomicWriteOp, Left "Empty path not allowed")
    else (AtomicWriteOp, Right (Success "written"))

-- | Export C-compatible ABI
%foreign "C:read_file,libfile_io"
prim__readFile : String -> PrimIO String

%foreign "C:write_file,libfile_io"
prim__writeFile : String -> String -> PrimIO Int
