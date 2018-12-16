# Dependency License Report

Bold-faced **`package-name`**s denote standard libraries bundled with `ghc-8.2.2`.

## Direct dependencies of `cabal-plan:exe:cabal-plan`

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Also depended upon by |
| --- | --- | --- | --- | --- |
| `Cabal` | [`2.4.1.0`](http://hackage.haskell.org/package/Cabal-2.4.1.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/Cabal-2.4.1.0/src/LICENSE) | A framework for packaging Haskell software |  |
| `aeson` | [`1.4.2.0`](http://hackage.haskell.org/package/aeson-1.4.2.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/aeson-1.4.2.0/src/LICENSE) | Fast JSON parsing and encoding |  |
| `ansi-terminal` | [`0.8.2`](http://hackage.haskell.org/package/ansi-terminal-0.8.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ansi-terminal-0.8.2/src/LICENSE) | Simple ANSI terminal support, with Windows compatibility | `ansi-wl-pprint` |
| **`base`** | [`4.10.1.0`](http://hackage.haskell.org/package/base-4.10.1.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/base-4.10.1.0/src/LICENSE) | Basic libraries | *(core library)* |
| `base-compat` | [`0.10.5`](http://hackage.haskell.org/package/base-compat-0.10.5) | [`MIT`](http://hackage.haskell.org/package/base-compat-0.10.5/src/LICENSE) | A compatibility layer for base | `aeson` |
| `base-orphans` | [`0.8`](http://hackage.haskell.org/package/base-orphans-0.8) | [`MIT`](http://hackage.haskell.org/package/base-orphans-0.8/src/LICENSE) | Backwards-compatible orphan instances for base |  |
| `base16-bytestring` | [`0.1.1.6`](http://hackage.haskell.org/package/base16-bytestring-0.1.1.6) | [`BSD-3-Clause`](http://hackage.haskell.org/package/base16-bytestring-0.1.1.6/src/LICENSE) | Fast base16 (hex) encoding and decoding for ByteStrings |  |
| **`bytestring`** | [`0.10.8.2`](http://hackage.haskell.org/package/bytestring-0.10.8.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/bytestring-0.10.8.2/src/LICENSE) | Fast, compact, strict and lazy byte strings with a list interface | `Cabal`, `aeson`, `attoparsec`, `base16-bytestring`, `binary`, `hashable`, `parsec`, `scientific`, `tar`, `text`, `unix`, `uuid-types`, `zlib` |
| **`containers`** | [`0.5.10.2`](http://hackage.haskell.org/package/containers-0.5.10.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/containers-0.5.10.2/src/LICENSE) | Assorted concrete container types | `Cabal`, `aeson`, `attoparsec`, `binary`, `scientific`, `tar`, `th-abstraction` |
| **`directory`** | [`1.3.0.2`](http://hackage.haskell.org/package/directory-1.3.0.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/directory-1.3.0.2/src/LICENSE) | Platform-agnostic library for filesystem operations | `Cabal`, `process`, `tar` |
| **`filepath`** | [`1.4.1.2`](http://hackage.haskell.org/package/filepath-1.4.1.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/filepath-1.4.1.2/src/LICENSE) | Library for manipulating FilePaths in a cross platform way. | `Cabal`, `directory`, `process`, `tar` |
| `mtl` | [`2.2.2`](http://hackage.haskell.org/package/mtl-2.2.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/mtl-2.2.2/src/LICENSE) | Monad classes, using functional dependencies | `Cabal`, `parsec` |
| `optparse-applicative` | [`0.14.3.0`](http://hackage.haskell.org/package/optparse-applicative-0.14.3.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/optparse-applicative-0.14.3.0/src/LICENSE) | Utilities and combinators for parsing command line options |  |
| `parsec` | [`3.1.13.0`](http://hackage.haskell.org/package/parsec-3.1.13.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/parsec-3.1.13.0/src/LICENSE) | Monadic parser combinators | `Cabal` |
| `tar` | [`0.5.1.0`](http://hackage.haskell.org/package/tar-0.5.1.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/tar-0.5.1.0/src/LICENSE) | Reading, writing and manipulating ".tar" archive files. |  |
| `text` | [`1.2.3.1`](http://hackage.haskell.org/package/text-1.2.3.1) | [`BSD-2-Clause`](http://hackage.haskell.org/package/text-1.2.3.1/src/LICENSE) | An efficient packed Unicode text type. | `Cabal`, `aeson`, `attoparsec`, `hashable`, `parsec`, `scientific`, `uuid-types` |
| `vector` | [`0.12.0.2`](http://hackage.haskell.org/package/vector-0.12.0.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/vector-0.12.0.2/src/LICENSE) | Efficient Arrays | `aeson` |
| `zlib` | [`0.6.2`](http://hackage.haskell.org/package/zlib-0.6.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/zlib-0.6.2/src/LICENSE) | Compression and decompression in the gzip and zlib formats |  |

## Indirect transitive dependencies

| Name | Version | [SPDX](https://spdx.org/licenses/) License Id | Description | Depended upon by |
| --- | --- | --- | --- | --- |
| `StateVar` | [`1.1.1.1`](http://hackage.haskell.org/package/StateVar-1.1.1.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/StateVar-1.1.1.1/src/LICENSE) | State variables | `contravariant` |
| `ansi-wl-pprint` | [`0.6.8.2`](http://hackage.haskell.org/package/ansi-wl-pprint-0.6.8.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ansi-wl-pprint-0.6.8.2/src/LICENSE) | The Wadler/Leijen Pretty Printer for colored ANSI terminal output | `optparse-applicative` |
| **`array`** | [`0.5.2.0`](http://hackage.haskell.org/package/array-0.5.2.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/array-0.5.2.0/src/LICENSE) | Mutable and immutable arrays | `Cabal`, `attoparsec`, `binary`, `containers`, `deepseq`, `integer-logarithms`, `stm`, `tar`, `text` |
| `attoparsec` | [`0.13.2.2`](http://hackage.haskell.org/package/attoparsec-0.13.2.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/attoparsec-0.13.2.2/src/LICENSE) | Fast combinator parsing for bytestrings and text | `aeson` |
| **`binary`** | [`0.8.5.1`](http://hackage.haskell.org/package/binary-0.8.5.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/binary-0.8.5.1/src/LICENSE) | Binary serialisation for Haskell values using lazy ByteStrings | `Cabal`, `scientific`, `text`, `uuid-types` |
| `colour` | [`2.3.4`](http://hackage.haskell.org/package/colour-2.3.4) | [`MIT`](http://hackage.haskell.org/package/colour-2.3.4/src/LICENSE) | A model for human colour/color perception | `ansi-terminal` |
| `contravariant` | [`1.5`](http://hackage.haskell.org/package/contravariant-1.5) | [`BSD-3-Clause`](http://hackage.haskell.org/package/contravariant-1.5/src/LICENSE) | Contravariant functors | `aeson` |
| **`deepseq`** | [`1.4.3.0`](http://hackage.haskell.org/package/deepseq-1.4.3.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/deepseq-1.4.3.0/src/LICENSE) | Deep evaluation of data structures | `Cabal`, `aeson`, `attoparsec`, `bytestring`, `containers`, `dlist`, `hashable`, `pretty`, `process`, `scientific`, `tagged`, `tar`, `text`, `time`, `unordered-containers`, `uuid-types`, `vector` |
| `dlist` | [`0.8.0.5`](http://hackage.haskell.org/package/dlist-0.8.0.5) | [`BSD-3-Clause`](http://hackage.haskell.org/package/dlist-0.8.0.5/src/LICENSE) | Difference lists | `aeson` |
| **`ghc-boot-th`** | [`8.2.2`](http://hackage.haskell.org/package/ghc-boot-th-8.2.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-boot-th-8.2.2/src/LICENSE) | Shared functionality between GHC and the @template-haskell@ library | `template-haskell` |
| **`ghc-prim`** | [`0.5.1.1`](http://hackage.haskell.org/package/ghc-prim-0.5.1.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/ghc-prim-0.5.1.1/src/LICENSE) | GHC primitives | *(core library)* |
| `hashable` | [`1.2.7.0`](http://hackage.haskell.org/package/hashable-1.2.7.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/hashable-1.2.7.0/src/LICENSE) | A class for types that can be converted to a hash value | `aeson`, `scientific`, `unordered-containers`, `uuid-types` |
| **`integer-gmp`** | [`1.0.1.0`](http://hackage.haskell.org/package/integer-gmp-1.0.1.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/integer-gmp-1.0.1.0/src/LICENSE) | Integer library based on GMP | *(core library)* |
| `integer-logarithms` | [`1.0.2.2`](http://hackage.haskell.org/package/integer-logarithms-1.0.2.2) | [`MIT`](http://hackage.haskell.org/package/integer-logarithms-1.0.2.2/src/LICENSE) | Integer logarithms. | `scientific` |
| **`pretty`** | [`1.1.3.3`](http://hackage.haskell.org/package/pretty-1.1.3.3) | [`BSD-3-Clause`](http://hackage.haskell.org/package/pretty-1.1.3.3/src/LICENSE) | Pretty-printing library | `Cabal`, `template-haskell` |
| `primitive` | [`0.6.4.0`](http://hackage.haskell.org/package/primitive-0.6.4.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/primitive-0.6.4.0/src/LICENSE) | Primitive memory-related operations | `aeson`, `scientific`, `vector` |
| **`process`** | [`1.6.1.0`](http://hackage.haskell.org/package/process-1.6.1.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/process-1.6.1.0/src/LICENSE) | Process libraries | `Cabal`, `optparse-applicative` |
| `random` | [`1.1`](http://hackage.haskell.org/package/random-1.1) | [`BSD-3-Clause`](http://hackage.haskell.org/package/random-1.1/src/LICENSE) | random number library | `uuid-types` |
| `scientific` | [`0.3.6.2`](http://hackage.haskell.org/package/scientific-0.3.6.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/scientific-0.3.6.2/src/LICENSE) | Numbers represented using scientific notation | `aeson`, `attoparsec` |
| `stm` | [`2.5.0.0`](http://hackage.haskell.org/package/stm-2.5.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/stm-2.5.0.0/src/LICENSE) | Software Transactional Memory | `StateVar` |
| `tagged` | [`0.8.6`](http://hackage.haskell.org/package/tagged-0.8.6) | [`BSD-3-Clause`](http://hackage.haskell.org/package/tagged-0.8.6/src/LICENSE) | Haskell 98 phantom types to avoid unsafely passing dummy arguments | `aeson` |
| **`template-haskell`** | [`2.12.0.0`](http://hackage.haskell.org/package/template-haskell-2.12.0.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/template-haskell-2.12.0.0/src/LICENSE) | Support library for Template Haskell | `aeson`, `tagged`, `th-abstraction` |
| `th-abstraction` | [`0.2.8.0`](http://hackage.haskell.org/package/th-abstraction-0.2.8.0) | [`ISC`](http://hackage.haskell.org/package/th-abstraction-0.2.8.0/src/LICENSE) | Nicer interface for reified information about data types | `aeson` |
| **`time`** | [`1.8.0.2`](http://hackage.haskell.org/package/time-1.8.0.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/time-1.8.0.2/src/LICENSE) | A time library | `Cabal`, `aeson`, `directory`, `random`, `tar`, `time-locale-compat`, `unix` |
| `time-locale-compat` | [`0.1.1.5`](http://hackage.haskell.org/package/time-locale-compat-0.1.1.5) | [`BSD-3-Clause`](http://hackage.haskell.org/package/time-locale-compat-0.1.1.5/src/LICENSE) | Compatibile module for time-format locale | `aeson` |
| **`transformers`** | [`0.5.2.0`](http://hackage.haskell.org/package/transformers-0.5.2.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/transformers-0.5.2.0/src/LICENSE) | Concrete functor and monad transformers | `Cabal`, `StateVar`, `attoparsec`, `contravariant`, `mtl`, `optparse-applicative`, `primitive`, `tagged`, `transformers-compat` |
| `transformers-compat` | [`0.6.2`](http://hackage.haskell.org/package/transformers-compat-0.6.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/transformers-compat-0.6.2/src/LICENSE) | A small compatibility shim for the transformers library | `optparse-applicative` |
| **`unix`** | [`2.7.2.2`](http://hackage.haskell.org/package/unix-2.7.2.2) | [`BSD-3-Clause`](http://hackage.haskell.org/package/unix-2.7.2.2/src/LICENSE) | POSIX functionality | `Cabal`, `base-compat`, `directory`, `process` |
| `unordered-containers` | [`0.2.9.0`](http://hackage.haskell.org/package/unordered-containers-0.2.9.0) | [`BSD-3-Clause`](http://hackage.haskell.org/package/unordered-containers-0.2.9.0/src/LICENSE) | Efficient hashing-based container types | `aeson` |
| `uuid-types` | [`1.0.3`](http://hackage.haskell.org/package/uuid-types-1.0.3) | [`BSD-3-Clause`](http://hackage.haskell.org/package/uuid-types-1.0.3/src/LICENSE) | Type definitions for Universally Unique Identifiers | `aeson` |

