# AGENTS.md

## Project

- This is a Rust workspace for practicing a derive macro that generates a type-state builder.
- Workspace members are `builder_pattern` (generation support), `builder_pattern_derive` (the `Builder` proc macro), and `example` (usage example).
- The root `README.md` documents the public behavior and supported field attributes. Keep it consistent with implementation changes.

## Change guidelines

- Read the relevant crate source and existing tests before changing macro behavior.
- Preserve the compile-time guarantees documented in `README.md`: required fields must be set, and ordinary fields cannot be set more than once. `bool`, `Option`, and `Vec` fields, plus `as_is`, `each`, `default`, and `fixed` attributes, have documented special behavior.
- The macro currently rejects generic structs. Do not imply generic support unless it is implemented and covered by suitable tests and documentation.
- Keep changes focused on the requested behavior. Avoid unrelated dependency, workspace layout, or public API changes.
- Add or update tests for behavior changes, including compile-fail cases when the guarantee is about whether generated code compiles.
- Update the README examples and documentation when public macro behavior changes.

## Validation

- Format Rust changes with `cargo fmt --all`.
- Run the relevant crate tests; for workspace-wide changes, use `cargo test --workspace`.
- Review `git diff` to ensure only intended files changed.
