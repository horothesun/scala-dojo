# Arithmetic expressions calculator

## Grammar

Pseudo-grammar for arithmetic expressions

```
expr   = expr + term | expr - term | term
term   = term * factor | term / factor | factor
factor = power ^ factor | power
power  = - unary | unary
unary  = nonNegDecimal | natural | ( expr )

nonNegDecimal = natural.digit
natural       = 0 | [1-9]<digit*
digit         = [0-9]
```

### Operator precedence

Operator precedence (`()` grouping **>** unary `-` **>** `^` **>** `*`, `/` **>** `+`, binary `-`) is modelled by
'layering' the operators at different precedence levels in dedicated production rules as follows

- `expr` for `+` and binary `-`,
- `term` for `*` and `/`,
- `factor` for `^`,
- `power` for unary `-` and
- `unary` for `()` grouping.

### Operator associativity

- _Left-to-right_ associativity is modelled directly by left-recursive production rules
  like `expr` (for `+` and binary `-`) and `term` (for `*` and `/`), while
- _right-to-left_ associativity is modelled directly by right-recursive production rules like `factor` (for `^`).

## Removing left-recursion

Since the previous grammar contains _left-recursive_ production rules
(`expr` and `term`, which can lead to a non-terminating parser),
we'll convert them into their `right-recursive` counterparts with the algorithm illustrated in the following
[example](https://www.geeksforgeeks.org/removing-direct-and-indirect-left-recursion-in-a-grammar).

> #### Left-recursion removal example
> Given the following rules
>
> ```
> S = S a | S b | c | d
> ```
> the converted right-recursive rules are
>
> ```
> S  = cS' | dS'
> S' = ε | aS' | bS'
> ```

Applying the left-recursion removal procedure just described, the previously defined grammar becomes

```
expr   = term expr'
expr'  = ε | + term expr' | - term expr'
term   = factor term'
term'  = ε | * factor term' | / factor term'
factor = power ^ factor | power
power  = - unary | unary
unary  = nonNegDecimal | natural | ( expr )

nonNegDecimal = natural.digit
natural       = 0 | [1-9]<digit*
digit         = [0-9]
```

which BNF form is

```
<expr>   ::= <term> <exprR>
<exprR>  ::= E | "+" <term> <exprR> | "-" <term> <exprR>
<term>   ::= <factor> <termR>
<termR>  ::= E | "*" <factor> <termR> | "/" <factor> <termR>
<factor> ::= <power> "^" <factor> | <power>
<power>  ::= "-" <unary> | <unary>
<unary>  ::= <nonNegDecimal> | <natural> | "(" <expr> ")"

<nonNegDecimal> ::= <natural> "." <digit>+
<natural>       ::= "0" | [1-9] <digit>*
<digit>         ::= [0-9]
```

## Parsing

The `ExprCodec.exprP` expression parser is built on top of the [cats-parse](https://typelevel.org/cats-parse) parser combinators library.

```scala
val parsedExpr: Either[Parser.Error, Expr] = exprP.parse("1-2.0*3")
```

`Models.Expr` values represent the AST returned by successful `exprP.parse` runs.

> `exprP` supports input strings with extra white-spaces (e.g. `" 1 +( 2^ 3 )"`).

## Evaluation

The `ExprEval.eval` functions return `EvalResult[_] = Either[EvalError, _]` to model
potential errors of the supported operations, e.g.

- `0 / 0`: `EvalError.DivisionUndefined`
- `n / 0`, with `n ≠ 0`: `EvalError.DivisionByZero(n)`
- `b ^ e`, with `b < 0`: `EvalError.PowerWithNegativeBase(b, e)`
- `0 ^ e`, with `e ≤ 0`: `EvalError.PowerUndefined(b, e)`

> Note: it's important for the `eval(expr: Expr)` and `eval(term: Term)` to perform the operations
> in the right order to correctly implement `+`/`-` and `*`/`/` left-associativity.

// TODO:

- are Double big enough? (also in NonNegDecimal case class)

## Testing

// TODO:

- `ExprCodec.encode` useful for testing
- ...