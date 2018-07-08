# Revision history for `cabal-plan`

## 0.4.0.0

### `lib:cabal-plan` Library

* New `SearchPlanJson` type to specify strategy for locating `plan.json`
* Add `SearchPlanJson` parameter to `findAndDecodePlanJson` function and change return type
* Expose separate `findProjectRoot` operation

### `exe:cabal-plan` Executable

* New command `license-report` (requires Cabal flag `license-report` to be active)

## 0.3.0.0

### `lib:cabal-plan` Library

* Add support for foreign-lib components.
* Add support for `dist-dir` `plan.json` field.
* Make `Sha256` type abstract and add new `sha256{To,From}ByteString`
  conversion functions, as well as the new `parseSha256` function.
* Introduce `FlagName` newtype.
* Add `FromJSONKey`/`ToJSONKey` instances for `UnitId`, `PackageName`, and `PkgId`.

### `exe:cabal-plan` Executable

* smart completer for list-bin/list-bins pattern
* new command `topo` (printing out topographic sorting of install-plan)
* `dot` prints component dependency graph. New options:
    - `--tred` transitive reduction
    - `--tred-weights` Adjust edge thickness during transitive reduction
    - `--path-from pkgA --path-from pkgB` Highlight dependency paths from *pkgA* to *pkgB*
    - `--revdep pkg` highlight reverse dependencies of pkg in the install plan

## 0.2.0.0

* Add an optional `--builddir` argument to all commands and to `findAndDecodePlanJson` function.
* Add experimental support for underlining.
* Reimplement CLI with `optparse-applicative`.
* Add new sub-command `list-bins` and change semantics of existing `list-bin` sub-cmd.

### 0.1.1.0

* Add `cabal-plan fingerprint` command for printing
  sha256 sums of source tarballs.

## 0.1.0.0

* First version. Released on an unsuspecting world.
