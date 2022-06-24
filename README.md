
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readODS <img src="man/figures/read_ods_logo.png"  align="right" height="200" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/readODS)](https://CRAN.R-project.org/package=readODS)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/readODS/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ropensci/readODS?branch=master)
[![R-CMD-check](https://github.com/ropensci/readODS/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/readODS/actions)
[![rOpenSci](https://badges.ropensci.org/302_status.svg)](https://github.com/ropensci/software-review/issues/386)
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
#>                  Name homeworld species
#> 1      Luke Skywalker  Tatooine   Human
#> 2               C-3PO  Tatooine   Human
#> 3               R2-D2  Alderaan   Human
#> 4         Darth Vader  Tatooine   Human
#> 5         Leia Organa  Tatooine   Human
#> 6           Owen Lars  Tatooine   Human
#> 7  Beru Whitesun lars   Stewjon   Human
#> 8               R5-D4  Tatooine   Human
#> 9   Biggs Darklighter  Kashyyyk Wookiee
#> 10     Obi-Wan Kenobi  Corellia   Human
```

Reading from the 2nd sheet

``` r
read_ods("starwars.ods", sheet = 2)
#>                  Name height mass    hair_color skin_color eye_color birth_year
#> 1      Luke Skywalker    172   77         blond       fair      blue       19.0
#> 2               C-3PO    202  136          none      white    yellow       41.9
#> 3               R2-D2    150   49         brown      light     brown       19.0
#> 4         Darth Vader    178  120   brown, grey      light      blue       52.0
#> 5         Leia Organa    165   75         brown      light      blue       47.0
#> 6           Owen Lars    183   84         black      light     brown       24.0
#> 7  Beru Whitesun lars    182   77 auburn, white       fair blue-gray       57.0
#> 8               R5-D4    188   84         blond       fair      blue       41.9
#> 9   Biggs Darklighter    228  112         brown    unknown      blue      200.0
#> 10     Obi-Wan Kenobi    180   80         brown       fair     brown       29.0
#>    gender
#> 1    male
#> 2    male
#> 3  female
#> 4    male
#> 5  female
#> 6    male
#> 7    male
#> 8    male
#> 9    male
#> 10   male
```

Reading from a specific range

``` r
read_ods("starwars.ods", sheet = 2, range = "A1:C11")
#>                  Name height mass
#> 1      Luke Skywalker    172   77
#> 2               C-3PO    202  136
#> 3               R2-D2    150   49
#> 4         Darth Vader    178  120
#> 5         Leia Organa    165   75
#> 6           Owen Lars    183   84
#> 7  Beru Whitesun lars    182   77
#> 8               R5-D4    188   84
#> 9   Biggs Darklighter    228  112
#> 10     Obi-Wan Kenobi    180   80
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
#> Warning: Missing column names filled in: 'X1' [1]
#>                     NA  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> 1            Mazda RX4 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> 2        Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> 3           Datsun 710 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> 4       Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> 5    Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> 6              Valiant 18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> 7           Duster 360 14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> 8            Merc 240D 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> 9             Merc 230 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> 10            Merc 280 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> 11           Merc 280C 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> 12          Merc 450SE 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> 13          Merc 450SL 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> 14         Merc 450SLC 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> 15  Cadillac Fleetwood 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> 16 Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> 17   Chrysler Imperial 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> 18            Fiat 128 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> 19         Honda Civic 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> 20      Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> 21       Toyota Corona 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> 22    Dodge Challenger 15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> 23         AMC Javelin 15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> 24          Camaro Z28 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> 25    Pontiac Firebird 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> 26           Fiat X1-9 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> 27       Porsche 914-2 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> 28        Lotus Europa 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> 29      Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> 30        Ferrari Dino 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> 31       Maserati Bora 15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> 32          Volvo 142E 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
```

``` r
read_ods("mtcars.ods", sheet = "plant", range = "A1:B10")
#>   weight group
#> 1   4.17  ctrl
#> 2   5.58  ctrl
#> 3   5.18  ctrl
#> 4   6.11  ctrl
#> 5   4.50  ctrl
#> 6   4.61  ctrl
#> 7   5.17  ctrl
#> 8   4.53  ctrl
#> 9   5.33  ctrl
```

### About the speed and file size

This package is written entirely in R. Although the efficiency has been
improved, please don’t expect the heavily optimized performance of
[readxl](https://readxl.tidyverse.org/),
[readr](https://readr.tidyverse.org/) and data.table’s
[fread](https://cran.r-project.org/package=data.table).

Also, this package can’t handle ODS files larger than “medium size”. See
[issue \#71](https://github.com/ropensci/readODS/issues/71).

If you need to read and write large ODS files efficiently, the [headless
interface of
LibreOffice](https://help.libreoffice.org/Common/Starting_the_Software_With_Parameters)
is recommended. See [issue
\#49](https://github.com/ropensci/readODS/issues/49) for an example.

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
by Thomas J. Leeper, John Foster, Sergio Oller, Jim Hester, Stephen
Watts, Arthur Katossky, Stas Malavin, Duncan Garmonsway, Mehrad
Mahmoudian, and Matt Kerlogue.

This package emulates the behaviors of `readxl::read_xlsx`,
`writexl::write_xlsx` and `xlsx::write.xlsx`.

This package should be a silent member of `rio`, so that you don’t need
to care about file format anymore.

### License

GPL3

### Contributing

Contributions in the form of feedback, comments, code, and bug report
are welcome.

  - Fork the source code, modify, and issue a [pull
    request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork).
  - Issues, bug reports: [File a Github
    issue](https://github.com/chainsawriot/readODS).
  - Github is not your thing? Contact Chung-hong Chan by e-mail, post,
    or other methods listed on this
    [page](https://www.mzes.uni-mannheim.de/d7/en/profiles/chung-hong-chan).

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
