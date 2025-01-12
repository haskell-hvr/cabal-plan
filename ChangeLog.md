# Revision history for `cabal-plan`

## 0.7.5.0

* No changes in the library
* Add `-f` filter flag to `tred` command to only show parts of the graph to given package(s).
  This essentially answers "why that package" is in the build plan.

## 0.7.4.0

* Use Cabal-syntax-3.12
* Support GHC-8.6.5...9.10.1

## 0.7.3.0

* Use Cabal-syntax-3.10

## 0.7.2.3

* Fix issue in previous release (license generation didn't work at all)

## 0.7.2.2

* Use Cabal-syntax-3.8.1.0

## 0.7.2.1

* Support Cabal-3.6
* Support aeson-2.0.0.0
* Drop support for GHC prior 8.2

## 0.7.2.0

### `exe:cabal-plan` Executable

* Use `cabal-install-parsers` to find and parse `~/cabal/config`
* Fix ascii/unicode output in `tred`
* Add flags to hide setup and executable components in dot command
* Update dependencies (support `base16-bytestring-1.0.0.0`)

### Library

* Update dependencies (support `base16-bytestring-1.0.0.0`)

## 0.7.1.0

### `exe:cabal-plan` Executable

* Add `--ascii` / `--unicode` flags to control output character set
* Add `dot-png` command as a version of `dot` command with different defaults
* Use `cabal-install-parsers`,
  this makes `license-report` work with non-default configurations

## 0.7.0.0

### `lib:cabal-plan` Library

* Support `local+noindex` style repositories: New `Repo` constructor: `RepoLocalNoIndex`.
* Support newer versions of dependencies (GHC-8.10, aeson-1.5, optics-core-0.3)

### `exe:cabal-plan` Executable

* Support `Cabal-3.2`

## 0.6.2.0

### `lib:cabal-plan` Library

* Add `findPlanJson` function

### `exe:cabal-plan` Executable

* Drop `process-extras` dependency

## 0.6.1.0

### `lib:cabal-plan` Library

No changes

### `exe:cabal-plan` Executable

* `dot` command got new options
    * `--root` to limit graph to specific roots
    * `--output` to write directly to some file
    * `--run-dot-png` and `--run-dot-pdf` to run `dot` for you

* `cabal-plan` executable depends on `optics-core` instead of `lens`.
  Therefore is buildable only with GHC-8.0+

## 0.6.0.0

### `lib:cabal-plan` Library

* `ExactPath` constructor to skip `find` in `findAndDecodePlanJson`.
  (Note: see also `decodePlanJson`)


### `exe:cabal-plan` Executable

* `--plan-json` for exact `plan.json` location
* `--relative` search for project root relative to that directory
* `--colors=auto|never|always` flag
* `tred` command to print transtive reduction of dependency graph
* `diff` command to compare two plans
* `list-bins` prints full selector "pkg:type:name", i.e. includes package name

## 0.5.0.0

### `lib:cabal-plan` Library

* New `dispCompNameTarget` function for pretty-printing `CompName`s in cabal's target-selector syntax.
* Add support for cabal 2.4's `pkg-src` package provenience metadata.
* Add support for cabal 2.4.1's `pkg-cabal-sha256` package description checksum field.

### `exe:cabal-plan` Executable

* Add support for including package description checksums in `fingerprint` output
* Add support for printing flag selection in `topo` output
* Fail gracefully in `license-report` when metadata cannot be found in index

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
