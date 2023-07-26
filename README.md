
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readODS <img src="man/figures/read_ods_logo.png"  align="right" height="200" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/readODS)](https://CRAN.R-project.org/package=readODS)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/readODS/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ropensci/readODS?branch=master)
[![rOpenSci](https://badges.ropensci.org/302_status.svg)](https://github.com/ropensci/software-review/issues/386)
[![R-CMD-check](https://github.com/ropensci/readODS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/readODS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The only goal of readODS is to enable R to read and write OpenDocument
Spreadsheet (ODS) files.

## Installation

Install the latest stable version from CRAN:

``` r
install.packages("readODS")
```

from R-universe:

``` r
install.packages("readODS", repos = "https://ropensci.r-universe.dev")
```

Or install the development version from Github:

``` r
devtools::install_github("ropensci/readODS")
```

## Usage

In almost all use cases, you only need two functions: `read_ods` and
`write_ods`. Simple.

#### Reading

``` r
library(readODS)
read_ods("starwars.ods")
#> # A tibble: 10 × 3
#>    Name               homeworld species
#>    <chr>              <chr>     <chr>  
#>  1 Luke Skywalker     Tatooine  Human  
#>  2 C-3PO              Tatooine  Human  
#>  3 R2-D2              Alderaan  Human  
#>  4 Darth Vader        Tatooine  Human  
#>  5 Leia Organa        Tatooine  Human  
#>  6 Owen Lars          Tatooine  Human  
#>  7 Beru Whitesun lars Stewjon   Human  
#>  8 R5-D4              Tatooine  Human  
#>  9 Biggs Darklighter  Kashyyyk  Wookiee
#> 10 Obi-Wan Kenobi     Corellia  Human
```

Reading from the 2nd sheet

``` r
read_ods("starwars.ods", sheet = 2)
#> # A tibble: 10 × 8
#>    Name           height  mass hair_color skin_color eye_color birth_year gender
#>    <chr>           <dbl> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> 
#>  1 Luke Skywalker    172    77 blond      fair       blue            19   male  
#>  2 C-3PO             202   136 none       white      yellow          41.9 male  
#>  3 R2-D2             150    49 brown      light      brown           19   female
#>  4 Darth Vader       178   120 brown, gr… light      blue            52   male  
#>  5 Leia Organa       165    75 brown      light      blue            47   female
#>  6 Owen Lars         183    84 black      light      brown           24   male  
#>  7 Beru Whitesun…    182    77 auburn, w… fair       blue-gray       57   male  
#>  8 R5-D4             188    84 blond      fair       blue            41.9 male  
#>  9 Biggs Darklig…    228   112 brown      unknown    blue           200   male  
#> 10 Obi-Wan Kenobi    180    80 brown      fair       brown           29   male
```

Reading from a specific range

``` r
read_ods("starwars.ods", sheet = 2, range = "A1:C11")
#> # A tibble: 10 × 3
#>    Name               height  mass
#>    <chr>               <dbl> <dbl>
#>  1 Luke Skywalker        172    77
#>  2 C-3PO                 202   136
#>  3 R2-D2                 150    49
#>  4 Darth Vader           178   120
#>  5 Leia Organa           165    75
#>  6 Owen Lars             183    84
#>  7 Beru Whitesun lars    182    77
#>  8 R5-D4                 188    84
#>  9 Biggs Darklighter     228   112
#> 10 Obi-Wan Kenobi        180    80
```

Reading as a dataframe

``` r
read_ods("starwars.ods", range="Sheet1!A2:C11", as_tibble = FALSE)
#>       Luke.Skywalker Tatooine   Human
#> 1              C-3PO Tatooine   Human
#> 2              R2-D2 Alderaan   Human
#> 3        Darth Vader Tatooine   Human
#> 4        Leia Organa Tatooine   Human
#> 5          Owen Lars Tatooine   Human
#> 6 Beru Whitesun lars  Stewjon   Human
#> 7              R5-D4 Tatooine   Human
#> 8  Biggs Darklighter Kashyyyk Wookiee
#> 9     Obi-Wan Kenobi Corellia   Human
```

#### Writing

``` r
## preserve the row names
write_ods(mtcars, "mtcars.ods", row_names = TRUE)
```

Appending a sheet

``` r
write_ods(PlantGrowth, "mtcars.ods", append = TRUE, sheet = "plant")
```

``` r
## Default: First sheet
read_ods("mtcars.ods")
#> New names:
#> • `` -> `...1`
#> # A tibble: 32 × 12
#>    ...1          mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <chr>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Mazda RX4    21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2 Mazda RX4 …  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3 Datsun 710   22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4 Hornet 4 D…  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5 Hornet Spo…  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6 Valiant      18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7 Duster 360   14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8 Merc 240D    24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9 Merc 230     22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10 Merc 280     19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
```

``` r
read_ods("mtcars.ods", sheet = "plant", range = "A1:B10")
#> # A tibble: 9 × 2
#>   weight group
#>    <dbl> <chr>
#> 1   4.17 ctrl 
#> 2   5.58 ctrl 
#> 3   5.18 ctrl 
#> 4   6.11 ctrl 
#> 5   4.5  ctrl 
#> 6   4.61 ctrl 
#> 7   5.17 ctrl 
#> 8   4.53 ctrl 
#> 9   5.33 ctrl
```

### Text Encoding

In older versions of R (\<4.2) on Windows, the default encoding for text
is not UTF-8, and instead depends on your locale. This can cause
problems processing characters that are not part of the character set R
is using (usually
[Windows-1252](https://en.wikipedia.org/wiki/Windows-1252)). Sheets
written using these characters generally contains errors. The problem
can be fixed by upgrading to a version of R \>= 4.2.

**Radian:** Even for up-to-date versions of R, these issues with
character encoding are still a known issue with Radian. Their suggested
workaround is
[here](https://github.com/randy3k/radian/issues/269#issuecomment-1169663251).

### Misc

The logo of readODS is a remix of LibreOffice Calc v6.1 icon created by
the Document Foundation. The original LibreOffice logo is licensed under
the [Creative Commons Attribution Share-Alike 3.0 Unported
License](https://wiki.documentfoundation.org/File:LibO6_MIME.svg).
readODS is not a product of the Document Foundation. The logo of readODS
is licensed under the [Creative Commons Attribution Share-Alike 3.0
Unported License](https://creativecommons.org/licenses/by-sa/3.0/).

The creator of this package is Gerrit-Jan Schutten. The current
maintainer is Chung-hong Chan. This package benefits from contributions
by Peter Brohan, Thomas J. Leeper, John Foster, Sergio Oller, Jim
Hester, Stephen Watts, Arthur Katossky, Stas Malavin, Duncan Garmonsway,
Mehrad Mahmoudian, Matt Kerlogue, Detlef Steuer, Michal Lauer, and Till
Straube.

This package emulates the behaviours of `readxl::read_xlsx`,
`writexl::write_xlsx` and `xlsx::write.xlsx`.

This package should be a silent member of `rio`, so that you don’t need
to care about file format any more.

### License

GPL3

### Contributing

Contributions in the form of feedback, comments, code, and bug report
are welcome.

  - Fork the source code, modify, and issue a [pull
    request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork).
  - Issues, bug reports: [File a Github
    issue](https://github.com/ropensci/readODS).

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
