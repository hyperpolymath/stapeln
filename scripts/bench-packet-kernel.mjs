#!/usr/bin/env node
// SPDX-License-Identifier: PMPL-1.0-or-later

import { addQ16, isAddWasmActive, isWasmActive, lerpQ16 } from "../frontend/src/PacketMathWasm.js";

const PACKET_COUNT = Number.parseInt(process.env.BENCH_PACKETS ?? "20000", 10);
const ITERATIONS = Number.parseInt(process.env.BENCH_ITERS ?? "300", 10);
const STEP = 0.016;
const Q16_ONE = 65536;
const COORD_SCALE = 1024;

const toProgressQ16 = (progress) => {
  const value = Math.trunc(progress * Q16_ONE);
  if (value < 0) {
    return 0;
  }
  if (value > Q16_ONE) {
    return Q16_ONE;
  }
  return value;
};

const jsAddQ16 = (left, right) => (left + right) | 0;
const jsLerpQ16 = (source, target, progressQ16) =>
  (source + (((target - source) * progressQ16) >> 16)) | 0;

const makeInput = () => {
  const srcX = new Int32Array(PACKET_COUNT);
  const srcY = new Int32Array(PACKET_COUNT);
  const tgtX = new Int32Array(PACKET_COUNT);
  const tgtY = new Int32Array(PACKET_COUNT);
  const progressFloat = new Float64Array(PACKET_COUNT);
  const progressQ16 = new Int32Array(PACKET_COUNT);
  const xFloat = new Float64Array(PACKET_COUNT);
  const yFloat = new Float64Array(PACKET_COUNT);
  const xQ16 = new Int32Array(PACKET_COUNT);
  const yQ16 = new Int32Array(PACKET_COUNT);

  let seed = 0x1234567;
  const nextRand = () => {
    seed = (seed * 1664525 + 1013904223) >>> 0;
    return seed / 0xffffffff;
  };

  for (let i = 0; i < PACKET_COUNT; i += 1) {
    const sx = Math.trunc(nextRand() * 1000 * COORD_SCALE);
    const sy = Math.trunc(nextRand() * 800 * COORD_SCALE);
    const tx = Math.trunc(nextRand() * 1000 * COORD_SCALE);
    const ty = Math.trunc(nextRand() * 800 * COORD_SCALE);
    const progress = nextRand() * 0.95;

    srcX[i] = sx;
    srcY[i] = sy;
    tgtX[i] = tx;
    tgtY[i] = ty;
    progressFloat[i] = progress;
    progressQ16[i] = toProgressQ16(progress);
    xFloat[i] = sx / COORD_SCALE;
    yFloat[i] = sy / COORD_SCALE;
    xQ16[i] = sx;
    yQ16[i] = sy;
  }

  return { srcX, srcY, tgtX, tgtY, progressFloat, progressQ16, xFloat, yFloat, xQ16, yQ16 };
};

const cloneInput = (input) => ({
  srcX: new Int32Array(input.srcX),
  srcY: new Int32Array(input.srcY),
  tgtX: new Int32Array(input.tgtX),
  tgtY: new Int32Array(input.tgtY),
  progressFloat: new Float64Array(input.progressFloat),
  progressQ16: new Int32Array(input.progressQ16),
  xFloat: new Float64Array(input.xFloat),
  yFloat: new Float64Array(input.yFloat),
  xQ16: new Int32Array(input.xQ16),
  yQ16: new Int32Array(input.yQ16),
});

const runFloatKernel = (input) => {
  const step = STEP;
  const start = process.hrtime.bigint();
  let checksum = 0;

  for (let frame = 0; frame < ITERATIONS; frame += 1) {
    for (let i = 0; i < PACKET_COUNT; i += 1) {
      const nextProgress = Math.min(1, input.progressFloat[i] + step);
      input.progressFloat[i] = nextProgress;

      const sx = input.srcX[i] / COORD_SCALE;
      const sy = input.srcY[i] / COORD_SCALE;
      const tx = input.tgtX[i] / COORD_SCALE;
      const ty = input.tgtY[i] / COORD_SCALE;

      const x = sx + (tx - sx) * nextProgress;
      const y = sy + (ty - sy) * nextProgress;
      input.xFloat[i] = x;
      input.yFloat[i] = y;
      checksum += x + y + nextProgress;
    }
  }

  const durationNs = Number(process.hrtime.bigint() - start);
  return { durationNs, checksum };
};

const runFixedKernel = (input, addFn, lerpFn) => {
  const stepQ16 = toProgressQ16(STEP);
  const start = process.hrtime.bigint();
  let checksum = 0;

  for (let frame = 0; frame < ITERATIONS; frame += 1) {
    for (let i = 0; i < PACKET_COUNT; i += 1) {
      const nextQ16 = Math.min(Q16_ONE, addFn(input.progressQ16[i], stepQ16));
      input.progressQ16[i] = nextQ16;
      const x = lerpFn(input.srcX[i], input.tgtX[i], nextQ16);
      const y = lerpFn(input.srcY[i], input.tgtY[i], nextQ16);
      input.xQ16[i] = x;
      input.yQ16[i] = y;
      checksum += x + y + nextQ16;
    }
  }

  const durationNs = Number(process.hrtime.bigint() - start);
  return { durationNs, checksum };
};

const nsPerOp = (durationNs) => durationNs / (PACKET_COUNT * ITERATIONS);
const ms = (durationNs) => durationNs / 1_000_000;

const baseInput = makeInput();

// Warmup JIT and WASM instantiation.
runFixedKernel(cloneInput(baseInput), addQ16, lerpQ16);
runFixedKernel(cloneInput(baseInput), jsAddQ16, jsLerpQ16);
runFloatKernel(cloneInput(baseInput));

const floatResult = runFloatKernel(cloneInput(baseInput));
const jsFixedResult = runFixedKernel(cloneInput(baseInput), jsAddQ16, jsLerpQ16);
const adaptiveFixedResult = runFixedKernel(cloneInput(baseInput), addQ16, lerpQ16);

const summary = {
  packets: PACKET_COUNT,
  iterations: ITERATIONS,
  operations: PACKET_COUNT * ITERATIONS,
  wasmStatus: {
    lerp: isWasmActive(),
    add: isAddWasmActive(),
  },
  timings: {
    floatMs: ms(floatResult.durationNs),
    jsFixedMs: ms(jsFixedResult.durationNs),
    adaptiveFixedMs: ms(adaptiveFixedResult.durationNs),
    floatNsPerOp: nsPerOp(floatResult.durationNs),
    jsFixedNsPerOp: nsPerOp(jsFixedResult.durationNs),
    adaptiveFixedNsPerOp: nsPerOp(adaptiveFixedResult.durationNs),
  },
  speedup: {
    adaptiveVsFloat: floatResult.durationNs / adaptiveFixedResult.durationNs,
    adaptiveVsJsFixed: jsFixedResult.durationNs / adaptiveFixedResult.durationNs,
  },
  checksums: {
    float: Number(floatResult.checksum.toFixed(3)),
    jsFixed: jsFixedResult.checksum,
    adaptiveFixed: adaptiveFixedResult.checksum,
  },
};

console.log(JSON.stringify(summary, null, 2));
