# AGENTS.md

## Project

- This is a Rust workspace for practicing a derive macro that generates a type-state builder.
- Workspace members are `builder_pattern` (runtime support types), `builder_pattern_derive` (macro expansion and the `Builder` proc macro), and `example` (usage example).
- The root `README.md` documents the public behavior and supported field attributes. Keep it consistent with implementation changes.

## Change guidelines

- Read the relevant crate source and existing tests before changing macro behavior.
- Preserve the compile-time guarantees documented in `README.md`: required fields must be set, and ordinary fields cannot be set more than once. `bool`, `Option`, and `Vec` fields, plus `as_is`, `each`, `default`, and `fixed` attributes, have documented special behavior.
- Generic named-field structs are supported. Keep type, lifetime, const generic, default parameter, and `where`-clause coverage in tests and document any limits before changing support.
- Keep changes focused on the requested behavior. Avoid unrelated dependency, workspace layout, or public API changes.
- Add or update tests for behavior changes, including compile-fail cases when the guarantee is about whether generated code compiles.
- Update the README examples and documentation when public macro behavior changes.
- Do not add personal information or machine-specific absolute file paths to repository files. Remove them when encountered; use repository-relative paths or placeholders when a path example is needed.

## Validation

- Format Rust changes with `cargo fmt --all`.
- Run the relevant crate tests; for workspace-wide changes, use `cargo test --workspace`.
- Review `git diff` to ensure only intended files changed.

## Code Review

When performing a code review, read and follow [REVIEW.md](./REVIEW.md).
These guidelines apply specifically to code review tasks. For regular
development tasks, follow the standard instructions in this file.
