// SPDX-License-Identifier: PMPL-1.0-or-later
// PacketBatchKernel.js - batched packet stepping with WASM kernel and JS fallback

const Q16_ONE = 65536;
const COORD_SCALE = 1024;
const BYTES_PER_I32 = 4;

const wasmBytes = new Uint8Array([0,97,115,109,1,0,0,0,1,15,1,96,11,127,127,127,127,127,127,127,127,127,127,127,0,3,2,1,0,5,3,1,0,16,6,9,1,127,1,65,128,128,192,0,11,7,23,2,6,109,101,109,111,114,121,2,0,10,115,116,101,112,95,98,97,116,99,104,0,0,10,248,1,1,245,1,2,2,127,1,126,2,64,32,9,65,0,76,13,0,32,10,65,128,128,4,32,10,65,128,128,4,72,27,34,10,65,0,32,10,65,0,74,27,33,11,3,64,32,9,69,13,1,32,8,32,4,40,2,0,34,10,65,128,128,4,32,10,65,128,128,4,72,27,34,10,65,0,32,10,65,0,74,27,32,11,106,34,10,65,255,255,3,75,34,12,54,2,0,32,7,65,128,128,4,32,10,32,12,27,34,10,54,2,0,32,5,32,0,40,2,0,34,12,32,2,52,2,0,32,12,172,125,32,10,173,34,13,126,66,16,136,167,106,54,2,0,32,6,32,1,40,2,0,34,10,32,3,52,2,0,32,10,172,125,32,13,126,66,16,136,167,106,54,2,0,32,3,65,4,106,33,3,32,1,65,4,106,33,1,32,6,65,4,106,33,6,32,2,65,4,106,33,2,32,0,65,4,106,33,0,32,5,65,4,106,33,5,32,7,65,4,106,33,7,32,4,65,4,106,33,4,32,9,65,127,106,33,9,32,8,65,4,106,33,8,12,0,11,11,11]);

const clampProgressQ16 = (progressQ16) => {
  if (progressQ16 < 0) return 0;
  if (progressQ16 > Q16_ONE) return Q16_ONE;
  return progressQ16 | 0;
};

const toProgressQ16 = (progress) => clampProgressQ16(Math.round(progress * Q16_ONE));
const fromProgressQ16 = (progressQ16) => progressQ16 / Q16_ONE;

const toCoordFixed = (coord) => Math.round(coord * COORD_SCALE);
const fromCoordFixed = (coord) => coord / COORD_SCALE;

const jsLerpQ16 = (source, target, progressQ16) =>
  (source + (((target - source) * clampProgressQ16(progressQ16)) >> 16)) | 0;

let kernelInitialized = false;
let kernelAvailable = false;
let kernelFn = null;
let kernelMemory = null;
let laneCapacity = 0;

let scratchCapacity = 0;
let laneIndices = new Int32Array(0);
let laneSrcX = new Int32Array(0);
let laneSrcY = new Int32Array(0);
let laneTgtX = new Int32Array(0);
let laneTgtY = new Int32Array(0);
let laneProgress = new Int32Array(0);
let laneOutX = new Int32Array(0);
let laneOutY = new Int32Array(0);
let laneOutProgress = new Int32Array(0);
let laneOutArrived = new Int32Array(0);

const ensureKernel = () => {
  if (kernelInitialized) return;
  kernelInitialized = true;

  try {
    const module = new WebAssembly.Module(wasmBytes);
    const instance = new WebAssembly.Instance(module, {});
    if (typeof instance.exports.step_batch === "function" && instance.exports.memory) {
      kernelFn = instance.exports.step_batch;
      kernelMemory = instance.exports.memory;
      kernelAvailable = true;
    }
  } catch (_) {
    kernelAvailable = false;
  }
};

const ensureScratch = (count) => {
  if (count <= scratchCapacity) {
    return;
  }

  scratchCapacity = 1;
  while (scratchCapacity < count) {
    scratchCapacity <<= 1;
  }

  laneIndices = new Int32Array(scratchCapacity);
  laneSrcX = new Int32Array(scratchCapacity);
  laneSrcY = new Int32Array(scratchCapacity);
  laneTgtX = new Int32Array(scratchCapacity);
  laneTgtY = new Int32Array(scratchCapacity);
  laneProgress = new Int32Array(scratchCapacity);
  laneOutX = new Int32Array(scratchCapacity);
  laneOutY = new Int32Array(scratchCapacity);
  laneOutProgress = new Int32Array(scratchCapacity);
  laneOutArrived = new Int32Array(scratchCapacity);
};

const layoutFor = (laneCount) => {
  if (!kernelAvailable || laneCount <= 0) {
    return null;
  }

  if (laneCount > laneCapacity) {
    laneCapacity = 1;
    while (laneCapacity < laneCount) {
      laneCapacity <<= 1;
    }
  }

  const bytesPerArray = laneCapacity * BYTES_PER_I32;
  const totalBytes = bytesPerArray * 9;
  while (kernelMemory.buffer.byteLength < totalBytes) {
    kernelMemory.grow(1);
  }

  const buffer = kernelMemory.buffer;
  return {
    srcXPtr: 0 * bytesPerArray,
    srcYPtr: 1 * bytesPerArray,
    tgtXPtr: 2 * bytesPerArray,
    tgtYPtr: 3 * bytesPerArray,
    progressPtr: 4 * bytesPerArray,
    outXPtr: 5 * bytesPerArray,
    outYPtr: 6 * bytesPerArray,
    outProgressPtr: 7 * bytesPerArray,
    outArrivedPtr: 8 * bytesPerArray,
    srcX: new Int32Array(buffer, 0 * bytesPerArray, laneCount),
    srcY: new Int32Array(buffer, 1 * bytesPerArray, laneCount),
    tgtX: new Int32Array(buffer, 2 * bytesPerArray, laneCount),
    tgtY: new Int32Array(buffer, 3 * bytesPerArray, laneCount),
    progress: new Int32Array(buffer, 4 * bytesPerArray, laneCount),
    outX: new Int32Array(buffer, 5 * bytesPerArray, laneCount),
    outY: new Int32Array(buffer, 6 * bytesPerArray, laneCount),
    outProgress: new Int32Array(buffer, 7 * bytesPerArray, laneCount),
    outArrived: new Int32Array(buffer, 8 * bytesPerArray, laneCount),
  };
};

const stepBatchFallback = (laneCount, stepQ16) => {
  for (let i = 0; i < laneCount; i += 1) {
    const nextQ16 = clampProgressQ16((laneProgress[i] + stepQ16) | 0);
    laneOutProgress[i] = nextQ16;
    laneOutArrived[i] = nextQ16 >= Q16_ONE ? 1 : 0;
    laneOutX[i] = jsLerpQ16(laneSrcX[i], laneTgtX[i], nextQ16);
    laneOutY[i] = jsLerpQ16(laneSrcY[i], laneTgtY[i], nextQ16);
  }
};

export const stepPacketsBatch = (packets, nodesById, step) => {
  const count = packets.length;
  if (count === 0) {
    return packets;
  }

  ensureKernel();
  ensureScratch(count);

  const stepQ16 = toProgressQ16(step);
  const nextPackets = new Array(count);
  let laneCount = 0;

  for (let i = 0; i < count; i += 1) {
    const packet = packets[i];

    if (packet.status === "InTransit") {
      const sourceNode = nodesById[packet.sourceNode];
      const targetNode = nodesById[packet.targetNode];

      if (sourceNode && targetNode) {
        laneIndices[laneCount] = i;
        laneSrcX[laneCount] = toCoordFixed(sourceNode.x);
        laneSrcY[laneCount] = toCoordFixed(sourceNode.y);
        laneTgtX[laneCount] = toCoordFixed(targetNode.x);
        laneTgtY[laneCount] = toCoordFixed(targetNode.y);
        laneProgress[laneCount] = toProgressQ16(packet.progress);
        laneCount += 1;
      } else {
        const nextQ16 = clampProgressQ16((toProgressQ16(packet.progress) + stepQ16) | 0);
        nextPackets[i] =
          nextQ16 >= Q16_ONE
            ? {...packet, progress: 1.0, status: "Delivered"}
            : {...packet, progress: fromProgressQ16(nextQ16)};
      }
      continue;
    }

    if (packet.status === "Delivered") {
      // Delivered packets must keep aging beyond 1.0 so cleanup logic can remove them.
      nextPackets[i] = {...packet, progress: packet.progress + step};
      continue;
    }

    nextPackets[i] = packet;
  }

  if (laneCount > 0) {
    let wasmLayout = null;
    if (kernelAvailable) {
      const layout = layoutFor(laneCount);
      if (layout !== null) {
        layout.srcX.set(laneSrcX.subarray(0, laneCount));
        layout.srcY.set(laneSrcY.subarray(0, laneCount));
        layout.tgtX.set(laneTgtX.subarray(0, laneCount));
        layout.tgtY.set(laneTgtY.subarray(0, laneCount));
        layout.progress.set(laneProgress.subarray(0, laneCount));

        kernelFn(
          layout.srcXPtr,
          layout.srcYPtr,
          layout.tgtXPtr,
          layout.tgtYPtr,
          layout.progressPtr,
          layout.outXPtr,
          layout.outYPtr,
          layout.outProgressPtr,
          layout.outArrivedPtr,
          laneCount,
          stepQ16,
        );
        wasmLayout = layout;
      } else {
        stepBatchFallback(laneCount, stepQ16);
      }
    } else {
      stepBatchFallback(laneCount, stepQ16);
    }

    for (let i = 0; i < laneCount; i += 1) {
      const packetIndex = laneIndices[i];
      const packet = packets[packetIndex];
      const arrived = wasmLayout === null ? laneOutArrived[i] : wasmLayout.outArrived[i];
      if (arrived === 1) {
        nextPackets[packetIndex] = {...packet, progress: 1.0, status: "Delivered"};
      } else {
        const nextProgress = wasmLayout === null ? laneOutProgress[i] : wasmLayout.outProgress[i];
        const nextX = wasmLayout === null ? laneOutX[i] : wasmLayout.outX[i];
        const nextY = wasmLayout === null ? laneOutY[i] : wasmLayout.outY[i];
        nextPackets[packetIndex] = {
          ...packet,
          progress: fromProgressQ16(nextProgress),
          position: [fromCoordFixed(nextX), fromCoordFixed(nextY)],
        };
      }
    }
  }

  return nextPackets;
};

export const isBatchWasmActive = () => {
  ensureKernel();
  return kernelAvailable;
};
