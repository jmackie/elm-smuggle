# `elm-smuggle`

[![Build Status](https://travis-ci.org/jmackie/elm-smuggle.svg?branch=master)](https://travis-ci.org/jmackie/elm-smuggle)
![GitHub release](https://img.shields.io/github/release/jmackie/elm-smuggle.svg)

Smuggle `git` dependencies into your [Elm][elm-home] apps.

## Install

### From [npm](https://www.npmjs.com/package/elm-smuggle)

```bash
npm install --global elm-smuggle
```

### From source

```bash
stack install
```

## How to use it

1. Write some repo URLs to an `.elm-smuggle`. Commit this file.
2. Run `elm-smuggle` from the project root.
3. Run `elm install <smuggled-package-name>`

## Options

```
-v, --version       Print elm-smuggle version
```

```
-h, --help          Print help information
```

```
--suppress-errors   Don't print (non-fatal) errors
```

```
--reinstall         Force reinstall package versions
```

By default `elm-smuggle` will use system installation of `elm 0.19`.
```
--elm-bin=<path>    Relative path to elm binary, e.g. to the one in your node_modules folder
```

## Caveats

Packages to be smuggled _should_ be publishable by `elm` standards. That means:

-   Packages should be properly documented: `elm-make --docs=docs.json` should pass when run from the root of the package directory
-   Packages should be (git) tagged by semantic version (`MAJOR.MINOR.PATCH`)
-   Packages have at least one tag >= 1.0.0

[elm-home]: https://elm-lang.org/

## TODO

-   [ ] Rethink `.elm-smuggle` file format
