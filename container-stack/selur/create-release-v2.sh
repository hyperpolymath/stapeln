#!/bin/bash
# Script to create selur v1.0.0 release artifacts
set -e

VERSION="1.0.0"
RELEASE_DIR="selur-${VERSION}"
TARBALL="selur-${VERSION}.tar.gz"

echo "=== Creating selur v${VERSION} release ==="

# Ensure WASM is built
echo "[1/5] Ensuring WASM module exists..."
if [ ! -f "zig/zig-out/bin/selur.wasm" ]; then
    echo "Building WASM module..."
    just build
fi

# Build Rust library (release mode)  
echo "[2/5] Building Rust library..."
cargo build --release --quiet

# Run quick tests
echo "[3/5] Running tests..."
cargo test --release --quiet

# Create release directory
echo "[4/5] Creating release directory..."
rm -rf dist/${RELEASE_DIR}
mkdir -p dist/${RELEASE_DIR}

# Copy files
cp zig/zig-out/bin/selur.wasm dist/${RELEASE_DIR}/
cp target/release/libselur.rlib dist/${RELEASE_DIR}/
cp -r wiki dist/${RELEASE_DIR}/
cp -r docs dist/${RELEASE_DIR}/
cp -r examples dist/${RELEASE_DIR}/
cp README.adoc ROADMAP.adoc LICENSE dist/${RELEASE_DIR}/
cp RELEASE-NOTES-v1.0.0.md dist/${RELEASE_DIR}/RELEASE-NOTES.md
cp ECOSYSTEM.scm META.scm STATE.scm dist/${RELEASE_DIR}/
cp justfile Cargo.toml dist/${RELEASE_DIR}/
cp -r ephapax dist/${RELEASE_DIR}/
cp -r zig dist/${RELEASE_DIR}/
cp -r idris dist/${RELEASE_DIR}/
cp -r src dist/${RELEASE_DIR}/
cp -r benches dist/${RELEASE_DIR}/

# Create tarball
echo "[5/5] Creating tarball..."
cd dist
tar czf ${TARBALL} ${RELEASE_DIR}/
cd ..

echo ""
echo "✓ Release created successfully!"
echo ""
echo "Artifact: dist/${TARBALL}"
echo "Size: $(du -h dist/${TARBALL} | cut -f1)"
echo ""
echo "Files included:"
echo "  - selur.wasm (WASM module)"
echo "  - libselur.rlib (Rust library)"
echo "  - Complete source code"
echo "  - Documentation (wiki/, docs/)"
echo "  - Examples"
echo "  - License and metadata"
echo ""
ls -lh dist/${RELEASE_DIR}/ | head -20
echo ""
echo "Next steps:"
echo "  1. git add RELEASE-NOTES-v1.0.0.md RELEASE-CHECKLIST.md create-release.sh"
echo "  2. git commit -m 'Add v1.0.0 release artifacts'"
echo "  3. git tag v${VERSION}"
echo "  4. git push origin main"
echo "  5. git push origin v${VERSION}"
echo "  6. Create GitHub release with dist/${TARBALL}"
