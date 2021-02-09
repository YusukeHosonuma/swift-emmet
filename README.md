# swift-emmet

A CLI tool that generate struct or class by simple-syntax.

```bash
$ swift-emmet 'S.Person = l.name:String, v.age:Int, v.weight:Double'
```

This will generate follows.

```swift
struct Person {
    let name: String
    var age: Int
    var weight: Double
}
```

## Usage

### Example

```bash
# Generate struct
$ swift-emmet 'S.Person = l.name:String, v.age:Int, v.weight:Double'

# Generate class
$ swift-emmet 'C.Person = l.name:String, v.age:Int, v.weight:Double'

# Ommit `let` or `var` (`var` will be used)
$ swift-emmet 'S.Person = name:String, age:Int, weight:Double'

# Basic types are supported alias
#
# S = String
# B = Bool
# I = Int
# L = Long
# F = Float
# D = Double
# U = URL
#
$ swift-emmet 'S.Person = l.name:S, v.age:I, v.weight:D'

# Any white-space (or not) are allows
$ swift-emmet 'S . Person = l . name : String'
$ swift-emmet 'S.Person=l.name:String'
```

### REPL

If you run `swift-emmet` without options, REPL will be started.

```bash
$ swift-emmet
--------------------------------------------------------------------
Welcome swift-emmet 🎉
...
--------------------------------------------------------------------

swift-emmet> S.Person = l.name:String # Please input expr

struct Person {
    let name: String
}

swift-emmet> exit # Bye
```

### Available Commands

```bash
$ swift-emmet --help

Usage: swift-emmet [EXPR] [--version] [--usage]
  Generate struct or class from short syntax

Available options:
  EXPR                     Evaluate expression and print
  --version                Show version
  --usage                  Show detail usage
  -h,--help                Show this help text
```

## Installation

### Homebrew

T.B.D

### Manual

```bash
$ stack install
$ ~/.local/bin/swift-emmet # or add `~/.local/bin` to PATH environment
```

## Development

Requirements:

- [Stack](https://docs.haskellstack.org/en/stable/README/) 2.5.1
- [GHC](https://www.haskell.org/ghc/) 8.10.3

```bash
$ stack build
$ stack test
```

## Issues and PR

You have any ideas or found bugs? Any issue or PR is welcome.<br>
Thanks!

## Author

[Yusuke Hosonuma](https://github.com/YusukeHosonuma)
