# Get a Situation Report on System, nflverse Package Versions and Dependencies

**\[deprecated\]**

This function was deprecated. Please use
[`nflreadr::nflverse_sitrep`](https://nflreadr.nflverse.com/reference/sitrep.html).

This function gives a quick overview of the versions of R and the
operating system as well as the versions of nflverse packages, options,
and their dependencies. It's primarily designed to help you get a quick
idea of what's going on when you're helping someone else debug a
problem.

## Usage

``` r
report(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`nflreadr::nflverse_sitrep`](https://nflreadr.nflverse.com/reference/sitrep.html)

  `pkg`

  :   a character vector naming installed packages, or `NULL` (the
      default) meaning all nflverse packages. The function checks
      internally if all packages are installed and informs if that is
      not the case.

  `recursive`

  :   a logical indicating whether dependencies of `pkg` and their
      dependencies (and so on) should be included. Can also be a
      character vector listing the types of dependencies, a subset of
      `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.
      Character string `"all"` is shorthand for that vector, character
      string `"most"` for the same vector without `"Enhances"`,
      character string `"strong"` (default) for the first three elements
      of that vector.

  `redact_path`

  :   a logical indicating whether options that contain "path" in the
      name should be redacted, default = TRUE

  `dev_repos`

  :   Developmental cran-like repos to check, e.g. r-universe repos

## Details

See
[`nflreadr::nflverse_sitrep`](https://nflreadr.nflverse.com/reference/sitrep.html)
for details.

## Examples

``` r
# \donttest{

# report(recursive = FALSE)
nflverse_sitrep(pkg = "nflreadr", recursive = TRUE)
#> ── System Info ─────────────────────────────────────────────────────────────────
#> • R version 4.6.1 (2026-06-24) • Running under: Ubuntu 24.04.4 LTS
#> ── Package Status ──────────────────────────────────────────────────────────────
#>    package installed  cran   dev behind
#> 1 nflreadr     1.5.1 1.5.1 1.5.1       
#> ── Package Options ─────────────────────────────────────────────────────────────
#> • No options set for above packages
#> ── Package Dependencies ────────────────────────────────────────────────────────
#> • cachem     (1.1.0)   • lifecycle (1.0.5)  • methods (4.6.1)  
#> • cli        (3.6.6)   • memoise   (2.0.1)  • stats   (4.6.1)  
#> • curl       (7.1.0)   • rappdirs  (0.3.4)  • tools   (4.6.1)  
#> • data.table (1.18.4)  • rlang     (1.3.0)  • utils   (4.6.1)  
#> • fastmap    (1.2.0)   • grDevices (4.6.1)    
#> • glue       (1.8.1)   • graphics  (4.6.1)    
#> ────────────────────────────────────────────────────────────────────────────────

# }
```
