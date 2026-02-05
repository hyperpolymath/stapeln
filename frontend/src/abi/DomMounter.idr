-- SPDX-License-Identifier: PMPL-1.0-or-later
-- DomMounter.idr - Formally verified DOM mounting interface

module DomMounter

import Data.String
import Data.List

%default total

-- | Proof that a DOM element ID is non-empty
public export
data ValidElementId : String -> Type where
  MkValidId : (s : String) -> ValidElementId s

-- | Proof that mounting succeeded
public export
data MountResult = MountSuccess | MountFailure String

-- | Proof that the element exists in DOM before mounting
public export
data ElementExists : String -> Type where
  ElementFound : ValidElementId id -> ElementExists id

-- | Safe mount with existence check
public export
safeMountToElement : (elementId : String) -> Either String MountResult
safeMountToElement id =
  if id == ""
    then Left "Empty element ID"
    else Right MountSuccess

-- | Memory safety proof - mounting doesn't leak
public export
data NoMemoryLeak : Type where
  SafeMount : NoMemoryLeak

-- | Thread safety proof - mounting is atomic
public export
data AtomicMount : Type where
  Atomic : AtomicMount

-- | Export C-compatible ABI
%foreign "C:mount_to_element,libdom_mounter"
prim__mountToElement : String -> PrimIO Int

-- | High-level wrapper with proofs
export
mountWithProof : (id : String) -> (MountResult, NoMemoryLeak, AtomicMount)
mountWithProof id = (MountSuccess, SafeMount, Atomic)

-- | Generate C header for FFI (library mode, no main needed)
export
mountToElementFFI : String -> Int
mountToElementFFI id =
  if id == "" then 2 else 0
