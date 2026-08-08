# pkgstate

Read-only introspection of dpkg and APT state from R: installed packages,
candidate versions, origins, and upgradeability, returned as plain data
frames.

Part of [Runix](https://github.com/cornball-ai/runix), an R-native Unix
systems-administration framework.

**Status: experimental.** Version 0.0.1.x; the API changes without
deprecation until 0.1.0.

Implemented so far:

- `dpkg_installed()` — one row per package entry in the dpkg database,
  including non-installed states such as `config-files`.

Design rules, per the
[Phase 1 contracts](https://github.com/cornball-ai/runix/blob/master/docs/phase1-introspection-contracts.md):
plain data frames with stable columns, fail-closed parsing (unparseable
backend output is a typed error, never a guess), injectable runners so tests
run offline against recorded fixtures. Mutations are out of scope — they
live in [rapt](https://github.com/cornball-ai/rapt).

## Install

```r
remotes::install_github("cornball-ai/pkgstate")
```

Debian-family Linux only (`OS_type: unix`; requires dpkg).

## License

MIT © cornball.ai
