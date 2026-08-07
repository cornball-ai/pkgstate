# rdpkg

Read-only dpkg/apt introspection for Runix. **Mutations live in rapt, never here.**

API contracts, backend candidates, and testing strategy:
`~/runix/docs/phase1-introspection-contracts.md` — read it before adding functions.

Conventions:
- Exported listing functions return plain `data.frame`s with documented, stable columns.
- Fail-closed parsing: unparseable backend output is a typed error (`rdpkg_error`,
  inheriting `runix_error`), never a guess. Empty means "queried fine, nothing there".
- Injectable runners: exported functions never call `system2()` directly; tests
  substitute fakes. Fixtures in `inst/tinytest/fixtures/`, live smoke tests behind
  `tinytest::at_home()`.
- Tinyverse workflow: `tinyrox::document()`, `tinypkgr::install()`, `tinytest`.
