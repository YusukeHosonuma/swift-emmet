# swift-emmet

A CLI tool that generate struct or class by simple-syntax.

```bash
$ swift-emmet "s.Pserson: E, C = name: S, age: I, weight: D"
```

This will generate follows.

```swift
struct Pserson: Equatable, Codable {
    var name: String
    var age: Int
    var weight: Double
}
```

## TODO

- [ ] Decent REPL :)
- [ ] Support Apple Silicon
- [ ] Distribute binary for Linux

## Aliases

Basic types and protocols are supported aliases (case is ignored).

```bash
$ swift-emmet 's.Person: E, C = name:S, age:I, weight:D'
struct Person: Equatable, Codable {
    var name: String
    var age: Int
    var weight: Double
}
```

| Alias | Type |
|:--|:--|
| S | String |
| B | Bool   |
| I | Int    |
| L | Long   |
| F | Float  |
| D | Double |
| U | URL    |

| Alias | Protocols |
|:--|:--|
| E | Equatable |
| C | Codable   |

## Usage

### Example

```bash
# Generate struct
$ swift-emmet 's.Person = l.name:String, v.age:Int, v.weight:Double'

# Generate class (with initializer)
$ swift-emmet 'c.Person = l.name:String, v.age:Int, v.weight:Double'

# Ommit `let` or `var` (`var` will be used)
$ swift-emmet 's.Person = name:String, age:Int, weight:Double'

# Inherit protocol or class
$ swift-emmet 's.Person: Equatable, Codable = l.name:String'

# Any white-space (or not) are allows
$ swift-emmet 's . Person = l . name : String'
$ swift-emmet 's.Person=l.name:String'
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

```bash
$ brew tap YusukeHosonuma/swift-emmet
$ brew install swift-emmet
```

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
