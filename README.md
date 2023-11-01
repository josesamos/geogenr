
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geogenr <a href="https://josesamos.github.io/geogenr/"><img src="man/figures/logo.png" align="right" height="139" alt="geogenr website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/geogenr)](https://CRAN.R-project.org/package=geogenr)
[![R-CMD-check](https://github.com/josesamos/geogenr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/josesamos/geogenr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<!-- [![Downloads](http://cranlogs.r-pkg.org/badges/geogenr?color=brightgreen)](https://www.r-pkg.org:443/pkg/geogenr) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/josesamos/geogenr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/josesamos/geogenr?branch=master) -->

The [American Community Survey
(ACS)](https://www.census.gov/programs-surveys/acs) offers geodatabases
with geographic information and associated data of interest to
researchers in the area. The goal of `geogenr` is to generate
`geomultistar` objects from those geodatabases automatically, once the
focus of attention is selected.

## Installation

You can install the released version of geogenr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("geogenr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josesamos/geogenr")
```

## Example

Each ACS geodatabase is structured in layers: a geographic layer, a
metadata layer, and the rest are data layers. The data layers have a
matrix form, the rows are indexed by instances of the geographic layer,
the columns by variables defined in the metadata layer, the cells are
numeric values. Here we have an example:

`GEOID          B01001e1 B01001m1 B01001e2 B01001m2 B01001e3 B01001m3 B01001e4 B01001m4 ...`

`16000US0100100      218      165       92      114       10       16       18       30`

`16000US0100124     2582       24     1313       98       45       37       14       19`

`16000US0100460     4374       24     1963      158      144       76      105       68`

`16000US0100484      641      159      326       89       10       17       16       11`

`16000US0100676      295      102      143       55        7       11       14       17`

`16000US0100820    32878       57    16236      453     1159      257     1151      209`

`...`

Some of the defined variables are shown below.

`Short Name Full Name`

`B01001e1   SEX BY AGE: Total: Total Population -- (Estimate)`

`B01001m1   SEX BY AGE: Total: Total Population -- (Margin of Error)`

`B01001e2   SEX BY AGE: Male: Total Population -- (Estimate)`

`B01001m2   SEX BY AGE: Male: Total Population -- (Margin of Error)`

`B01001e3   SEX BY AGE: Male: Under 5 years: Total Population -- (Estimate)`

`B01001m3   SEX BY AGE: Male: Under 5 years: Total Population -- (Margin of Error)`

`B01001e4   SEX BY AGE: Male: 5 to 9 years: Total Population -- (Estimate)`

`B01001m4   SEX BY AGE: Male: 5 to 9 years: Total Population -- (Margin of Error)`

`...`

First, we select and download the ACS geodatabases using the functions
offered by the package. Once we have them in a folder (in this case some
examples are included in the package), this is a basic example which
shows you how to solve a common problem:

``` r
library(geogenr)
```

For a folder, we get the years for which we have one area geodatabases
downloaded. We select the geodatabase for a specific year
(`uscb_layer`). From among the layers and groups of variables available,
we select a layer (`get_layer`) and one of its groups
(`get_layer_group`). From the selected variables we generate a
`geomultistar` object (`get_geomultistar`).

The first rows of the dimension and fact tables are shown below.

Once we have a `geomultistar` object, we can use the functionality of
[`starschemar`](https://CRAN.R-project.org/package=starschemar) and
[`geomultistar`](https://CRAN.R-project.org/package=geomultistar)
packages to define multidimensional queries with geographic information.

The result is a vector layer that we can save, perform spatial analysis
or queries on it, or we can see it as a map, using the functions
associated with the `sf` class.

Once we have verified that the data for the reference year is what we
need, we can expand our database considering the rest of the years
available in the folder. The only requirement to consider a year is that
its variable structure be the same as that of the reference year.

Instead of displaying all the tables, we focus on the table in the
*when* dimension.

Includes data for all available years.
