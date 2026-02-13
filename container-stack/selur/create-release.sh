#!/bin/bash
# Script to create selur v1.0.0 release artifacts
set -e

VERSION="1.0.0"
RELEASE_DIR="selur-${VERSION}"
TARBALL="selur-${VERSION}.tar.gz"

echo "=== Creating selur v${VERSION} release ==="

# Clean previous builds
echo "[1/6] Cleaning previous builds..."
cargo clean
rm -rf zig-out dist

# Build WASM module
echo "[2/6] Building WASM module..."
just build

# Build Rust library (release mode)
echo "[3/6] Building Rust library..."
cargo build --release

# Run tests
echo "[4/6] Running tests..."
cargo test --release

# Generate documentation
echo "[5/6] Generating documentation..."
cargo doc --no-deps --release

# Create release directory
echo "[6/6] Creating release tarball..."
mkdir -p dist/${RELEASE_DIR}

# Copy files
cp -r zig-out/bin/selur.wasm dist/${RELEASE_DIR}/
cp target/release/libselur.rlib dist/${RELEASE_DIR}/
cp -r wiki dist/${RELEASE_DIR}/
cp -r docs dist/${RELEASE_DIR}/
cp -r examples dist/${RELEASE_DIR}/
cp README.adoc ROADMAP.adoc LICENSE dist/${RELEASE_DIR}/
cp RELEASE-NOTES-v1.0.0.md dist/${RELEASE_DIR}/
cp ECOSYSTEM.scm META.scm STATE.scm dist/${RELEASE_DIR}/
cp justfile Cargo.toml dist/${RELEASE_DIR}/

# Create tarball
cd dist
tar czf ${TARBALL} ${RELEASE_DIR}/
cd ..

echo ""
echo "✓ Release created successfully!"
echo ""
echo "Artifact: dist/${TARBALL}"
echo "Size: $(du -h dist/${TARBALL} | cut -f1)"
echo ""
echo "Contents:"
ls -lh dist/${RELEASE_DIR}/
echo ""
echo "Next steps:"
echo "  1. git tag v${VERSION}"
echo "  2. git push origin v${VERSION}"
echo "  3. Create GitHub release with dist/${TARBALL}"
