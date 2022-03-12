
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regulationsgov

<!-- badges: start -->

<!-- badges: end -->

The package regulationsgov is to enable users to access and engage with
textual data from the [Regulations.gov
API](https://open.gsa.gov/api/regulationsgov/).

## Installation

You can install the development version of regulationsgov from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("q-w-a/regulationsgov")
```

## First Steps

To access the [Regulations.gov
API](https://open.gsa.gov/api/regulationsgov/), you will need to obtain
an API key from [data.gov](https://data.gov/), which you can do
[here](https://api.data.gov/signup/).

Once you have this key, you can provide it to every time you call one of
the functions requiring authentication using the `key` argument, but it
may be easiest to set it up as an environmental variable. You can do
this with the following function.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(regulationsgov)
## basic example code
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
