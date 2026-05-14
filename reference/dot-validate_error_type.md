# Validate the error_type argument

Enforces the paradigm-specific set of allowed values for \`error_type\`.
For \`csem_gt()\`, both \`"absolute"\` and \`"relative"\` are valid.

## Usage

``` r
.validate_error_type(method, error_type, paradigm = "gt")
```

## Arguments

- method:

  Character vector of resolved methods.

- error_type:

  Character vector supplied by the user.

- paradigm:

  Character scalar.

## Value

The validated character vector (subset of valid values).
