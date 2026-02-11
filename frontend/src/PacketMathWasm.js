// SPDX-License-Identifier: PMPL-1.0-or-later
// PacketMathWasm.js - small WASM-backed interpolation kernel with JS fallback

const Q16_ONE = 65536;

// Module exports:
//   (func (export "lerp_q16") (param i32 i32 i32) (result i32))
//   source + (((target - source) * progressQ16) >> 16)
const wasmBytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
  0x01, 0x08, 0x01, 0x60, 0x03, 0x7f, 0x7f, 0x7f, 0x01, 0x7f,
  0x03, 0x02, 0x01, 0x00,
  0x07, 0x0c, 0x01, 0x08, 0x6c, 0x65, 0x72, 0x70, 0x5f, 0x71, 0x31, 0x36, 0x00, 0x00,
  0x0a, 0x12, 0x01, 0x10, 0x00, 0x20, 0x01, 0x20, 0x00, 0x6b, 0x20, 0x02, 0x6c, 0x41, 0x10,
  0x75, 0x20, 0x00, 0x6a, 0x0b,
]);

const clampProgressQ16 = (progressQ16) => {
  if (progressQ16 < 0) {
    return 0;
  }
  if (progressQ16 > Q16_ONE) {
    return Q16_ONE;
  }
  return progressQ16 | 0;
};

const jsFallbackLerp = (source, target, progressQ16) => {
  const clampedProgress = clampProgressQ16(progressQ16);
  return (source + (((target - source) * clampedProgress) >> 16)) | 0;
};

let kernelFn = jsFallbackLerp;
let kernelInitialized = false;
let wasmActive = false;

const ensureKernel = () => {
  if (kernelInitialized) {
    return kernelFn;
  }
  kernelInitialized = true;

  try {
    const module = new WebAssembly.Module(wasmBytes);
    const instance = new WebAssembly.Instance(module, {});
    const wasmLerp = instance.exports.lerp_q16;

    if (typeof wasmLerp === "function") {
      kernelFn = wasmLerp;
      wasmActive = true;
      return kernelFn;
    }
  } catch (_) {
    // Keep JS fallback when WASM is unavailable.
  }

  kernelFn = jsFallbackLerp;
  wasmActive = false;
  return kernelFn;
};

export const lerpQ16 = (source, target, progressQ16) => {
  const fn = ensureKernel();
  return fn(source | 0, target | 0, clampProgressQ16(progressQ16));
};

export const isWasmActive = () => {
  ensureKernel();
  return wasmActive;
};
