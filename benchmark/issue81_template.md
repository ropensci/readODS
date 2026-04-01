# Testing the speed of reading the ODS file `issue81.ods`


``` r
date()
```

    [1] "Wed Apr  1 12:07:29 2026"

``` r
devtools::load_all()
```

    ℹ Loading readODS

``` r
file <- here::here("tests/testdata/issue81.ods")
system.time(x <- read_ods(file, sheet = 2, skip = 4))
```

       user  system elapsed 
      0.432   0.071   0.503 

``` r
dim(x)
```

    [1] 5539   11

``` r
sessionInfo()
```

    R version 4.5.3 (2026-03-11)
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
    [1] readODS_2.3.4  testthat_3.2.3

    loaded via a namespace (and not attached):
     [1] miniUI_0.1.2      jsonlite_2.0.0    compiler_4.5.3    brio_1.1.5       
     [5] promises_1.3.3    zip_2.3.3         Rcpp_1.1.0        later_1.4.4      
     [9] yaml_2.3.10       fastmap_1.2.0     here_1.0.1        mime_0.13        
    [13] R6_2.5.1          knitr_1.50        htmlwidgets_1.6.4 tibble_3.3.1     
    [17] desc_1.4.3        profvis_0.4.0     rprojroot_2.1.1   shiny_1.11.1     
    [21] tzdb_0.5.0        pillar_1.11.1     rlang_1.1.7       stringi_1.8.4    
    [25] cachem_1.1.0      httpuv_1.6.16     xfun_0.56         fs_1.6.6         
    [29] pkgload_1.4.0     memoise_2.0.1     cli_3.6.3         withr_3.0.1      
    [33] magrittr_2.0.4    digest_0.6.37     rstudioapi_0.17.1 xtable_1.8-4     
    [37] remotes_2.5.0     devtools_2.4.5    lifecycle_1.0.5   vctrs_0.7.1      
    [41] minty_0.0.5       evaluate_1.0.5    glue_1.8.0        cellranger_1.1.0 
    [45] urlchecker_1.0.1  sessioninfo_1.2.3 pkgbuild_1.4.8    rmarkdown_2.30   
    [49] purrr_1.2.1       pkgconfig_2.0.3   tools_4.5.3       usethis_3.1.0    
    [53] ellipsis_0.3.2    htmltools_0.5.8.1
