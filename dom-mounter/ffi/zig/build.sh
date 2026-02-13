#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Build script for Zig FFI libraries

set -euo pipefail

echo "Building Zig FFI libraries..."

# Create lib directory if it doesn't exist
mkdir -p lib

# Detect platform
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    PLATFORM="linux"
    EXT="so"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="macos"
    EXT="dylib"
elif [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
    PLATFORM="windows"
    EXT="dll"
else
    echo "Unsupported platform: $OSTYPE"
    exit 1
fi

# Build each library
echo "Building for $PLATFORM..."

zig build-lib src/dom_mounter.zig \
    -dynamic \
    -OReleaseFast \
    -femit-bin=lib/libdom_mounter.$EXT

zig build-lib src/dom_mounter_enhanced.zig \
    -dynamic \
    -OReleaseFast \
    -femit-bin=lib/libdom_mounter_enhanced.$EXT

zig build-lib src/dom_mounter_security.zig \
    -dynamic \
    -OReleaseFast \
    -femit-bin=lib/libdom_mounter_security.$EXT

echo "Build complete! Libraries in ffi/zig/lib/"
ls -lh lib/
