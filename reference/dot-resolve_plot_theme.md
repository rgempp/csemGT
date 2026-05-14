# Resolve a plot theme to its concrete settings

Maps a \`theme\` name to the list of concrete settings consumed by the
plotting helpers. csemGT ships a single own theme, \`"csem"\`; the
argument is kept as a vector for forward extension.

## Usage

``` r
.resolve_plot_theme(theme = c("csem"))
```

## Arguments

- theme:

  Theme name; currently only \`"csem"\`.

## Value

A list with the palette and the structural (grid) colour.
