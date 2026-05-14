# Set the RNG seed while remembering the prior state

Captures the current value of \`.Random.seed\` (or \`NULL\` if no seed
has been set in the session), calls \`base::set.seed()\` with the
user-supplied seed, and returns the captured state. The caller is
responsible for restoring the prior state using \`.restore_seed()\` via
an \`on.exit()\` registration, e.g.:

## Usage

``` r
.set_seed_restoring(seed)
```

## Arguments

- seed:

  Integer or numeric scalar passed to \`base::set.seed()\`.

## Value

Invisibly the previous value of \`.Random.seed\`, or \`NULL\` if none
was set.

## Details


    if (!is.null(seed)) {
      old_seed <- .set_seed_restoring(seed)
      on.exit(.restore_seed(old_seed), add = TRUE)
    }

This pattern keeps the public function reproducible without leaking a
deterministic state into the user's session.
