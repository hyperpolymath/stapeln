// SPDX-License-Identifier: PMPL-1.0-or-later
// PacketMathWasm.res - typed bridge for a small WASM interpolation kernel

@module("./PacketMathWasm.js")
external lerpQ16Unsafe: (int, int, int) => int = "lerpQ16"

@module("./PacketMathWasm.js")
external isWasmActive: unit => bool = "isWasmActive"

@module("./PacketMathWasm.js")
external addQ16Unsafe: (int, int) => int = "addQ16"

@module("./PacketMathWasm.js")
external isAddWasmActive: unit => bool = "isAddWasmActive"

let coordScale = 1024.0
let progressScale = 65536.0
let maxProgressQ16 = 65536

let clampInt = (value: int, minValue: int, maxValue: int): int => {
  if value < minValue {
    minValue
  } else if value > maxValue {
    maxValue
  } else {
    value
  }
}

let toFixedCoord = (coord: float): int => int_of_float(coord *. coordScale)
let fromFixedCoord = (coord: int): float => float_of_int(coord) /. coordScale

let toProgressQ16 = (progress: float): int => {
  let raw = int_of_float(progress *. progressScale)
  clampInt(raw, 0, maxProgressQ16)
}

type progressAdvance = {
  progress: float,
  arrived: bool,
}

let advanceProgress = (~progress: float, ~step: float): progressAdvance => {
  let progressQ16 = toProgressQ16(progress)
  let stepQ16 = toProgressQ16(step)
  let nextQ16Raw = addQ16Unsafe(progressQ16, stepQ16)
  let nextQ16 = clampInt(nextQ16Raw, 0, maxProgressQ16)
  {
    progress: float_of_int(nextQ16) /. progressScale,
    arrived: nextQ16 >= maxProgressQ16,
  }
}

let lerpCoordinate = (~source: float, ~target: float, ~progress: float): float => {
  let sourceFixed = toFixedCoord(source)
  let targetFixed = toFixedCoord(target)
  let progressQ16 = toProgressQ16(progress)
  let nextFixed = lerpQ16Unsafe(sourceFixed, targetFixed, progressQ16)
  fromFixedCoord(nextFixed)
}

let lerpPosition = (~source: (float, float), ~target: (float, float), ~progress: float): (float, float) => {
  let (sourceX, sourceY) = source
  let (targetX, targetY) = target

  (
    lerpCoordinate(~source=sourceX, ~target=targetX, ~progress),
    lerpCoordinate(~source=sourceY, ~target=targetY, ~progress),
  )
}
