
<!-- README.md is generated from README.Rmd. Please edit that file -->

# avertr

<!-- badges: start -->

<!-- badges: end -->

AVERT is an EPA tool which

avertr is very much still under development. It is also the first R
package I’ve developed and the biggest software project I’ve undertaken.
All of that is to say: set your expectations low and you’ll be
pleasantly surprised. I’m working on making it more user-friendly.

Why use avertr?

- It’s easy to integrate into R code.

- It’s faster than AVERT’s Main Module and (seemingly, often) faster
  than the Web Version.

- It’s more easily modifiable.

- Working with avertr can help you get a better understanding of how
  AVERT works (especially if you’re more familiar with R than VBA).

**NOTE**: avertr is based on EPA’s AVERT, but it is completely
independent. I do not work for the EPA.

## Installation

You can install the development version of avertr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("josephpendleton/avertr")
```

## Requirements

Files are big. You need 16 GB. Haven’t yet tested on Windows.

## Set Up

The current version of avertr requires some manual setup by the user.
Hop

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(avertr)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
