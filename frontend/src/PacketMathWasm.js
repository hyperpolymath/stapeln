// SPDX-License-Identifier: PMPL-1.0-or-later
// PacketMathWasm.js - small WASM-backed packet math kernel with JS fallback

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

// Module exports:
//   (func (export "add_i32") (param i32 i32) (result i32))
const wasmAddBytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
  0x01, 0x07, 0x01, 0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f,
  0x03, 0x02, 0x01, 0x00,
  0x07, 0x0b, 0x01, 0x07, 0x61, 0x64, 0x64, 0x5f, 0x69, 0x33, 0x32, 0x00, 0x00,
  0x0a, 0x09, 0x01, 0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b,
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

const nowNs = () => {
  if (typeof performance !== "undefined" && typeof performance.now === "function") {
    return performance.now() * 1_000_000;
  }
  return Date.now() * 1_000_000;
};

let calibrationSink = 0;

const pickFasterLerpKernel = (wasmCandidate) => {
  const iterations = 40_000;
  const rounds = 3;
  for (let round = 0; round < rounds; round += 1) {
    let wasmChecksum = 0;
    const wasmStart = nowNs();
    for (let i = 0; i < iterations; i += 1) {
      const source = ((i + round) * 1103515245) | 0;
      const target = ((i + round) * 2654435761) | 0;
      const progress = (i + round) & 0xffff;
      wasmChecksum ^= wasmCandidate(source, target, progress);
    }
    const wasmDuration = nowNs() - wasmStart;

    let jsChecksum = 0;
    const jsStart = nowNs();
    for (let i = 0; i < iterations; i += 1) {
      const source = ((i + round) * 1103515245) | 0;
      const target = ((i + round) * 2654435761) | 0;
      const progress = (i + round) & 0xffff;
      jsChecksum ^= jsFallbackLerp(source, target, progress);
    }
    const jsDuration = nowNs() - jsStart;
    calibrationSink ^= wasmChecksum ^ jsChecksum;

    // WASM must win by margin on every round to avoid noise-based flips.
    if (!(wasmDuration * 1.15 < jsDuration)) {
      wasmActive = false;
      return jsFallbackLerp;
    }
  }

  wasmActive = true;
  return wasmCandidate;
};

let kernelFn = jsFallbackLerp;
let kernelInitialized = false;
let wasmActive = false;
let addFn = (left, right) => (left + right) | 0;
let addKernelInitialized = false;
let addWasmActive = false;

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
      kernelFn = pickFasterLerpKernel(wasmLerp);
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

const pickFasterAddKernel = (wasmCandidate) => {
  const iterations = 70_000;
  const rounds = 3;
  for (let round = 0; round < rounds; round += 1) {
    let wasmChecksum = 0;
    const wasmStart = nowNs();
    for (let i = 0; i < iterations; i += 1) {
      wasmChecksum ^= wasmCandidate((i + round) | 0, ((i + round) * 3) | 0);
    }
    const wasmDuration = nowNs() - wasmStart;

    let jsChecksum = 0;
    const jsStart = nowNs();
    for (let i = 0; i < iterations; i += 1) {
      jsChecksum ^= ((i + round) + ((i + round) * 3)) | 0;
    }
    const jsDuration = nowNs() - jsStart;
    calibrationSink ^= wasmChecksum ^ jsChecksum;

    // WASM must win by margin on every round to avoid noise-based flips.
    if (!(wasmDuration * 1.15 < jsDuration)) {
      addWasmActive = false;
      return (left, right) => (left + right) | 0;
    }
  }

  addWasmActive = true;
  return wasmCandidate;
};

const ensureAddKernel = () => {
  if (addKernelInitialized) {
    return addFn;
  }
  addKernelInitialized = true;

  try {
    const module = new WebAssembly.Module(wasmAddBytes);
    const instance = new WebAssembly.Instance(module, {});
    const wasmAdd = instance.exports.add_i32;

    if (typeof wasmAdd === "function") {
      addFn = pickFasterAddKernel(wasmAdd);
      return addFn;
    }
  } catch (_) {
    // Keep JS fallback when WASM is unavailable.
  }

  addFn = (left, right) => (left + right) | 0;
  addWasmActive = false;
  return addFn;
};

export const addQ16 = (left, right) => {
  const fn = ensureAddKernel();
  return fn(left | 0, right | 0);
};

export const isAddWasmActive = () => {
  ensureAddKernel();
  return addWasmActive;
};
