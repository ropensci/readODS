# Testing the speed of nycflights roundtrip

nycflights round trip is an example in the `writexl` documentation. The
original code looks like so:

``` r
library(nycflights13)
out <- readxl::read_xlsx(writexl::write_xlsx(flights))
all.equal(out, flights)
```

Let’s break it down

``` r
date()
```

    [1] "Wed Aug  9 23:38:22 2023"

``` r
library(nycflights13)
system.time(path <- writexl::write_xlsx(flights))
```

       user  system elapsed 
      5.949   0.304   6.262 

``` r
system.time(out <- readxl::read_xlsx(path))
```

       user  system elapsed 
      2.188   0.640   2.837 

``` r
all.equal(out, flights)
```

    [1] "Component \"time_hour\": 'tzone' attributes are inconsistent ('UTC' and 'America/New_York')"

The ODS version

``` r
devtools::load_all()
```

    ℹ Loading readODS

``` r
system.time(path <- readODS::write_ods(flights))
```

       user  system elapsed 
     94.480   0.452  95.059 

``` r
system.time(out <- readODS::read_ods(path))
```

       user  system elapsed 
     26.438   1.660  28.104 

``` r
all.equal(out, flights)
```

    [1] "Component \"time_hour\": 'tzone' attributes are inconsistent ('UTC' and 'America/New_York')"
    [2] "Component \"time_hour\": Mean absolute difference: 15618.15"                                

``` r
sessionInfo()
```

    R version 4.3.1 (2023-06-16)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu 22.04.3 LTS

    Matrix products: default
    BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0 
    LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0

    locale:
     [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
     [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
     [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
     [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
     [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

    time zone: Europe/Berlin
    tzcode source: system (glibc)

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
    [1] readODS_2.0.0      testthat_3.1.10    nycflights13_1.0.2

    loaded via a namespace (and not attached):
     [1] utf8_1.2.3        xml2_1.3.5        stringi_1.7.12    hms_1.1.3        
     [5] digest_0.6.33     magrittr_2.0.3    evaluate_0.21     pkgload_1.3.2    
     [9] fastmap_1.1.1     rprojroot_2.0.3   cellranger_1.1.0  jsonlite_1.8.7   
    [13] zip_2.3.0         writexl_1.4.2     processx_3.8.2    pkgbuild_1.4.2   
    [17] sessioninfo_1.2.2 brio_1.1.3        urlchecker_1.0.1  ps_1.7.5         
    [21] promises_1.2.0.1  purrr_1.0.1       fansi_1.0.4       cli_3.6.1        
    [25] shiny_1.7.4       rlang_1.1.1       crayon_1.5.2      ellipsis_0.3.2   
    [29] withr_2.5.0       remotes_2.4.2.1   cachem_1.0.8      yaml_2.3.7       
    [33] devtools_2.4.5    tools_4.3.1       tzdb_0.4.0        memoise_2.0.1    
    [37] httpuv_1.6.11     vctrs_0.6.3       R6_2.5.1          mime_0.12        
    [41] lifecycle_1.0.3   stringr_1.5.0     fs_1.6.3          htmlwidgets_1.6.2
    [45] usethis_2.1.6     miniUI_0.1.1.1    desc_1.4.2        pkgconfig_2.0.3  
    [49] callr_3.7.3       pillar_1.9.0      later_1.3.1       glue_1.6.2       
    [53] profvis_0.3.7     Rcpp_1.0.11       xfun_0.39         tibble_3.2.1     
    [57] rstudioapi_0.14   knitr_1.43        xtable_1.8-4      htmltools_0.5.5  
    [61] rmarkdown_2.22    readr_2.1.4       compiler_4.3.1    prettyunits_1.1.1
    [65] readxl_1.4.2     
