# Null-coalescing operator

Returns \`b\` when \`a\` is \`NULL\`, otherwise returns \`a\`. Provided
here for compatibility with R versions earlier than 4.4 (where \` base
operator). Internal use only.

## Usage

``` r
a %||% b
```

## Arguments

- a, b:

  R objects.

## Value

\`a\` if not \`NULL\`, else \`b\`.
