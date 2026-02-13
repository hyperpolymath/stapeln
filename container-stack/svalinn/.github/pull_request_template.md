## Description

<!-- Brief description of what this PR does -->

## Type of Change

- [ ] Bug fix (non-breaking change that fixes an issue)
- [ ] New feature (non-breaking change that adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update
- [ ] Refactoring (no functional changes)
- [ ] CI/Build configuration

## Surface Impact

<!-- Check all that apply. See docs/surface-contract.adoc for details -->

- [ ] No surface changes
- [ ] CLI flag/command changes (Category A)
- [ ] JSON output schema changes (Category B)
- [ ] Configuration format changes (Category C)
- [ ] Exit code changes (Category D)
- [ ] Error ID changes (Category E)

## If Surface Changes Apply

<!-- Complete this section if any surface changes are checked above -->

- [ ] Updated `docs/surface-contract.adoc`
- [ ] Added CHANGELOG entry with `BREAKING:` or `DEPRECATED:` prefix if applicable
- [ ] Migration plan documented (for breaking changes)
- [ ] Deprecation warnings added (for deprecated features)
- [ ] Schema version bumped in `spec/schemas/` (for JSON changes)

## Seam Impact

<!-- Check if this affects integration points -->

- [ ] No seam impact
- [ ] Affects Verified Container Spec consumption (Seam S)
- [ ] Affects Cerro-Torre integration (Seam C)
- [ ] Changes OCI runtime/image spec usage

## If Seam Impact Applies

- [ ] Updated `spec/SPEC_VERSION` if spec dependency changed
- [ ] Conformance tests updated/verified
- [ ] Interop testing completed

## Testing

- [ ] Unit tests added/updated
- [ ] Integration tests added/updated
- [ ] `vordr doctor` still passes
- [ ] Manual testing completed

### Test Commands Run

```bash
# List commands used to verify this PR
cargo test
vordr doctor --all
```

## Documentation

- [ ] Code comments added/updated
- [ ] README updated (if applicable)
- [ ] User-facing docs updated (if applicable)

## Checklist

- [ ] My code follows the project style guidelines
- [ ] I have performed a self-review
- [ ] I have commented my code where necessary
- [ ] My changes generate no new warnings
- [ ] New and existing tests pass locally

## Related Issues

<!-- Link to related issues: Fixes #123, Relates to #456 -->
