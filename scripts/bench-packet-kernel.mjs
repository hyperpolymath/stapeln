#!/usr/bin/env node
// SPDX-License-Identifier: PMPL-1.0-or-later

import { addQ16, isAddWasmActive, isWasmActive, lerpQ16 } from "../frontend/src/PacketMathWasm.js";
import { isBatchWasmActive, stepPacketsBatch } from "../frontend/src/PacketBatchKernel.js";

const PACKET_COUNT = Number.parseInt(process.env.BENCH_PACKETS ?? "20000", 10);
const ITERATIONS = Number.parseInt(process.env.BENCH_ITERS ?? "300", 10);
const STEP = 0.016;
const Q16_ONE = 65536;
const COORD_SCALE = 1024;
const OBJECT_PACKETS = Number.parseInt(process.env.BENCH_OBJECT_PACKETS ?? "5000", 10);
const OBJECT_ITERS = Number.parseInt(process.env.BENCH_OBJECT_ITERS ?? "180", 10);

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

const nodesById = {
  "node-1": { x: 150, y: 250 },
  "node-2": { x: 450, y: 250 },
  "node-3": { x: 750, y: 250 },
};

const makeObjectPackets = () => {
  const packets = new Array(OBJECT_PACKETS);
  let seed = 0x9e3779b9;
  const nextRand = () => {
    seed = (seed * 1664525 + 1013904223) >>> 0;
    return seed / 0xffffffff;
  };

  for (let i = 0; i < OBJECT_PACKETS; i += 1) {
    const progress = nextRand() * 0.95;
    packets[i] = {
      id: `bench-${i}`,
      packetType: "HTTPS",
      status: "InTransit",
      sourceNode: "node-1",
      targetNode: "node-3",
      payload: "bench",
      size: 1500,
      encrypted: true,
      timestamp: 0,
      progress,
      position: [150 + (750 - 150) * progress, 250],
    };
  }
  return packets;
};

const cloneObjectPackets = (packets) =>
  packets.map((packet) => ({
    ...packet,
    position: [packet.position[0], packet.position[1]],
  }));

const runObjectFloatKernel = (inputPackets) => {
  let packets = cloneObjectPackets(inputPackets);
  let checksum = 0;
  const start = process.hrtime.bigint();

  for (let frame = 0; frame < OBJECT_ITERS; frame += 1) {
    packets = packets.map((packet) => {
      if (packet.status === "InTransit") {
        const newProgress = packet.progress + STEP;
        if (newProgress >= 1) {
          return { ...packet, progress: 1, status: "Delivered" };
        }

        const src = nodesById[packet.sourceNode];
        const tgt = nodesById[packet.targetNode];
        if (src && tgt) {
          const x = src.x + (tgt.x - src.x) * newProgress;
          const y = src.y + (tgt.y - src.y) * newProgress;
          return { ...packet, progress: newProgress, position: [x, y] };
        }

        return { ...packet, progress: newProgress };
      }

      if (packet.status === "Delivered") {
        return { ...packet, progress: packet.progress + STEP };
      }

      return packet;
    });

    const probe = packets[frame % packets.length];
    checksum += probe.progress + probe.position[0] + probe.position[1];
  }

  const durationNs = Number(process.hrtime.bigint() - start);
  return { durationNs, checksum };
};

const runObjectBatchKernel = (inputPackets) => {
  let packets = cloneObjectPackets(inputPackets);
  let checksum = 0;
  const start = process.hrtime.bigint();

  for (let frame = 0; frame < OBJECT_ITERS; frame += 1) {
    packets = stepPacketsBatch(packets, nodesById, STEP);
    const probe = packets[frame % packets.length];
    checksum += probe.progress + probe.position[0] + probe.position[1];
  }

  const durationNs = Number(process.hrtime.bigint() - start);
  return { durationNs, checksum };
};

const nsPerOp = (durationNs) => durationNs / (PACKET_COUNT * ITERATIONS);
const ms = (durationNs) => durationNs / 1_000_000;

const baseInput = makeInput();
const objectBaseInput = makeObjectPackets();

// Warmup JIT and WASM instantiation.
runFixedKernel(cloneInput(baseInput), addQ16, lerpQ16);
runFixedKernel(cloneInput(baseInput), jsAddQ16, jsLerpQ16);
runFloatKernel(cloneInput(baseInput));
runObjectFloatKernel(objectBaseInput);
runObjectBatchKernel(objectBaseInput);

const floatResult = runFloatKernel(cloneInput(baseInput));
const jsFixedResult = runFixedKernel(cloneInput(baseInput), jsAddQ16, jsLerpQ16);
const adaptiveFixedResult = runFixedKernel(cloneInput(baseInput), addQ16, lerpQ16);
const objectFloatResult = runObjectFloatKernel(objectBaseInput);
const objectBatchResult = runObjectBatchKernel(objectBaseInput);

const summary = {
  packets: PACKET_COUNT,
  iterations: ITERATIONS,
  operations: PACKET_COUNT * ITERATIONS,
  wasmStatus: {
    lerp: isWasmActive(),
    add: isAddWasmActive(),
    batch: isBatchWasmActive(),
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
  objectKernel: {
    packets: OBJECT_PACKETS,
    iterations: OBJECT_ITERS,
    operations: OBJECT_PACKETS * OBJECT_ITERS,
    floatMs: ms(objectFloatResult.durationNs),
    batchMs: ms(objectBatchResult.durationNs),
    floatNsPerOp: objectFloatResult.durationNs / (OBJECT_PACKETS * OBJECT_ITERS),
    batchNsPerOp: objectBatchResult.durationNs / (OBJECT_PACKETS * OBJECT_ITERS),
    batchVsFloat: objectFloatResult.durationNs / objectBatchResult.durationNs,
    checksums: {
      float: Number(objectFloatResult.checksum.toFixed(3)),
      batch: Number(objectBatchResult.checksum.toFixed(3)),
    },
  },
};

console.log(JSON.stringify(summary, null, 2));
