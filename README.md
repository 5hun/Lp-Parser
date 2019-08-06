# Lp-Parser

Very small and simple parser for .LP files
into logical Python data structures.

## Requirements

- python 3
- [pyparsing](https://github.com/pyparsing/pyparsing)
- [pulp](https://github.com/coin-or/pulp)

## Usage

```
>>> import lp_parser
>>> m = lp_parser.read("<lpFile>")
```

where m is a `pulp.LpProblem` object.
