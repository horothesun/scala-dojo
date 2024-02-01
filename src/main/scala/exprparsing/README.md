# Arithmetic expressions calculator

## Grammar

Pseudo-grammar for arithmetic expressions

```
expr   = expr + term | expr - term | term
term   = term * factor | term / factor | factor
factor = power ^ factor | power
power  = - unary | unary
unary  = nonNegDecimal | natural | ( expr )
```

### Operator precedence

Operator precedence (`()` grouping **>** unary `-` **>** `^` **>** `*`, `/` **>** `+`, binary `-`) is modelled by 'layering'
the operators at different precedence levels in dedicated production rules as follows

- `expr` for `+` and binary `-`,
- `term` for `*` and `/`,
- `factor` for `^`,
- `power` for unary `-` and
- `unary` for `()` grouping.

### Operator associativity

- _Left-to-right_ associativity is modelled directly by left-recursive production rules like the `expr` and `term` ones, while
- _right-to-left_ associativity is modelled directly by right-recursive production rules like the `factor` one.

## Removing left-recursion

Since the previous grammar contains _left-recursive_ production rules
(`expr` and `term`, which can lead to a non-terminating parser),
we'll convert them into their `right-recursive` counterparts with the algorithm illustrated in the following
[example](https://www.geeksforgeeks.org/removing-direct-and-indirect-left-recursion-in-a-grammar).

> ### Left-recursion removal example
> Given the following rules
>
> ```
> S ⇒ S a | S b | c | d
> ```
> the converted right-recursive rules are
>
> ```
> S ⇒ cS' | dS'
> S' ⇒ ε | aS' | bS'
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

<nonNegDecimal> ::= <digits> "." <digits>
<natural>       ::= <digits>
<digits>        ::= [0-9]+
```

## Parsing

The `exprP` expression parser is built on top of the `cats-parse` parsed combinator library.

```scala
val parsedExpr: Either[Parser.Error, Expr] = exprP.parse("1-2.0*3")
```

## Evaluation

...

- division by `0`
- `0 / 0`
- `b ^ e`, with `b` negative real
- `b ^ e`, ...
