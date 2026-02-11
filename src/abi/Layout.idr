-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Memory layout notes for Stapeln ABI

module Stapeln.ABI.Layout

import Stapeln.ABI.Types

%default total

public export
serviceSpecSizeBytes : Int
serviceSpecSizeBytes = 40

public export
stackSpecHeaderSizeBytes : Int
stackSpecHeaderSizeBytes = 32

public export
validationFindingHeaderSizeBytes : Int
validationFindingHeaderSizeBytes = 32

public export
abiLayoutVersion : String
abiLayoutVersion = "stapeln-abi-layout-v1"

