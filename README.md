# `elm-smuggle`

[![Build Status](https://travis-ci.org/jmackie/elm-smuggle.svg?branch=master)](https://travis-ci.org/jmackie/elm-smuggle)
![GitHub release](https://img.shields.io/github/release/jmackie/elm-smuggle.svg)

Smuggle `git` dependencies into your [Elm][elm-home] apps.

# Install

### From [npm](https://www.npmjs.com/package/elm-smuggle)

```bash
npm install --global elm-smuggle
```

### From source

```bash
cd elm-smuggle
stack install
```

# How to use it

Let's assume you have two git dependencies you'd like to add to your application (and your name is joe...)

1. `joe/elm-secret` - an Elm package on your local filesystem
2. `joe/elm-magic` - an Elm package on github that you don't want to publish to [package.elm-lang.org](https://package.elm-lang.org/) for some reason

First, you need to make a couple of changes to your `elm.json`:

```json
{
    "type": "application",
    "source-directories": ["src"],
    "elm-version": "0.19.0",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.1",
            "elm/core": "1.0.2",
            "elm/html": "1.0.0",
            "joe/elm-secret": "1.0.0",
            "joe/elm-magic": "2.1.0"
        },
        "indirect": {
            "elm/json": "1.1.2",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    },
    "git-dependencies": {
        "joe/elm-secret": "file:////home/joe/elm-secret",
        "joe/elm-magic": "https://github.com/joe/elm-magic.git"
    }
}
```

Note the new `github-dependencies` field at the bottom. This field tells `elm-smuggle` a) that you want to add some extra dependencies and b) where to find them. The value of each field is passed to `git clone` as is, so should be valid in that context.

Once added to `github-dependencies` you can go ahead and add the package versions to your `depends` as normal.

Then, before you run any `elm` commands, you should run the tool from the root of your project:

```bash
elm-smuggle
```

And you're good to go.

# How it works

1. Look for an `elm.json` in the working directory
2. Parse `github-dependencies` from the `elm.json`
3. `git clone` each `github-dependency` to a temporary directory
4. Checkout each tagged version (>= 1.0.0), `elm-make --docs=docs.json`, and copy to the (global) elm package cache
5. Update the package registry

# Caveats

All packages in `github-dependencies` _should_ be publishable by `elm` standards. That means:

-   Packages should be properly documented. That is `elm-make --docs=docs.json` should pass when run from the root of the package directory
-   Packages should be tagged by semantic version (`MAJOR.MINOR.PATCH`)
-   Packages have at least one tag >= 1.0.0

[elm-home]: https://elm-lang.org/
