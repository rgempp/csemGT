# Restore a previously captured RNG seed

Restores the global \`.Random.seed\` from a value captured earlier by
\`.set_seed_restoring()\`. If \`old_seed\` is \`NULL\` the seed is
removed (returning the RNG to a "no seed set yet" state), reproducing
the state the session was in before the seed was modified.

## Usage

``` r
.restore_seed(old_seed)
```

## Arguments

- old_seed:

  Integer vector previously captured, or \`NULL\`.

## Value

Invisibly \`NULL\`.
