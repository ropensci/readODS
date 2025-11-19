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

    [1] "Wed Nov 19 20:05:32 2025"

``` r
library(nycflights13)
system.time(path <- writexl::write_xlsx(flights))
```

       user  system elapsed 
      6.389   0.325   6.726 

``` r
system.time(out <- readxl::read_xlsx(path))
```

       user  system elapsed 
      2.248   0.819   3.072 

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
     13.057   0.564  13.826 

``` r
system.time(out <- readODS::read_ods(path))
```

       user  system elapsed 
     26.574   2.184  28.959 

``` r
all.equal(out, flights)
```

    [1] "Component \"time_hour\": 'tzone' attributes are inconsistent ('UTC' and 'America/New_York')"
    [2] "Component \"time_hour\": Mean absolute difference: 15618.15"                                

``` r
sessionInfo()
```

    R version 4.5.2 (2025-10-31)
    Platform: x86_64-pc-linux-gnu
    Running under: Ubuntu 22.04.5 LTS

    Matrix products: default
    BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.20.so;  LAPACK version 3.10.0

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
    [1] readODS_2.3.4      testthat_3.2.3     nycflights13_1.0.2

    loaded via a namespace (and not attached):
     [1] stringi_1.8.4     digest_0.6.37     magrittr_2.0.3    evaluate_1.0.5   
     [5] pkgload_1.4.0     fastmap_1.2.0     cellranger_1.1.0  rprojroot_2.1.1  
     [9] jsonlite_2.0.0    zip_2.3.3         writexl_1.5.1     pkgbuild_1.4.8   
    [13] sessioninfo_1.2.3 brio_1.1.5        urlchecker_1.0.1  promises_1.3.3   
    [17] purrr_1.0.2       cli_3.6.3         shiny_1.11.1      rlang_1.1.4      
    [21] ellipsis_0.3.2    remotes_2.5.0     withr_3.0.1       cachem_1.1.0     
    [25] yaml_2.3.10       devtools_2.4.5    tools_4.5.2       tzdb_0.4.0       
    [29] memoise_2.0.1     httpuv_1.6.16     vctrs_0.6.5       R6_2.5.1         
    [33] mime_0.13         lifecycle_1.0.4   minty_0.0.5       fs_1.6.6         
    [37] htmlwidgets_1.6.4 usethis_3.1.0     miniUI_0.1.2      pkgconfig_2.0.3  
    [41] desc_1.4.3        pillar_1.11.1     later_1.4.4       glue_1.8.0       
    [45] profvis_0.4.0     Rcpp_1.1.0        xfun_0.53         tibble_3.2.1     
    [49] rstudioapi_0.17.1 knitr_1.50        xtable_1.8-4      htmltools_0.5.8.1
    [53] rmarkdown_2.29    compiler_4.5.2    readxl_1.4.3     
