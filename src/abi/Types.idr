-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Stapeln ABI type definitions (Idris2)

module Stapeln.ABI.Types

import Data.List

%default total

public export
data ResultCode = Ok | Error | InvalidParam | NotFound | OutOfMemory

public export
resultToInt : ResultCode -> Int
resultToInt Ok = 0
resultToInt Error = 1
resultToInt InvalidParam = 2
resultToInt NotFound = 3
resultToInt OutOfMemory = 4

public export
record ServiceSpec where
  constructor MkServiceSpec
  name : String
  kind : String
  port : Int

public export
record StackSpec where
  constructor MkStackSpec
  stackId : Int
  name : String
  description : String
  services : List ServiceSpec

public export
record ValidationFinding where
  constructor MkValidationFinding
  findingId : String
  severity : String
  message : String
  hint : String

public export
record ValidationReport where
  constructor MkValidationReport
  score : Int
  findings : List ValidationFinding

