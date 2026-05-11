# Generate random partition

This helper function generates a random parameter partition, which is
used for the randomized AO procedure.

## Usage

``` r
generate_random_partition(x, p, min)
```

## Arguments

- x:

  \[`integer`\]  
  The parameter indices.

- p:

  \[`numeric(1)`\]  
  The probability of generating a new block.

- min:

  \[`integer(1)`\]  
  The minimum number of blocks.

## Value

A `list`, a random partition of `x`.

## Author

Siddhartha Chib
