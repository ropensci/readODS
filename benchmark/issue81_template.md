Testing the speed of reading the ODS file `issue81.ods`
================

``` r
date()
```

    [1] "Wed Dec 28 16:55:07 2022"

``` r
devtools::load_all()
```

    ℹ Loading readODS

``` r
file <- here::here("tests/testdata/issue81.ods")
system.time(x <- read_ods(file, sheet = 2, skip = 4))
```

       user  system elapsed 
      8.954   0.027   8.983 

``` r
dim(x)
```

    [1] 5539   11

``` r
sessionInfo()
```

    R version 4.2.2 Patched (2022-11-10 r83330)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu 22.04.1 LTS

    Matrix products: default
    BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0
    LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0

    locale:
     [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
     [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=en_US.UTF-8    
     [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=en_US.UTF-8   
     [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
     [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    [11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
    [1] readODS_1.7.3  testthat_3.1.4

    loaded via a namespace (and not attached):
     [1] pillar_1.8.1      cellranger_1.1.0  compiler_4.2.2    prettyunits_1.1.1
     [5] remotes_2.4.2     tools_4.2.2       digest_0.6.30     pkgbuild_1.3.1   
     [9] pkgload_1.3.0     tibble_3.1.8      jsonlite_1.8.4    evaluate_0.15    
    [13] memoise_2.0.1     lifecycle_1.0.3   pkgconfig_2.0.3   rlang_1.0.6      
    [17] cli_3.4.1         rstudioapi_0.14   yaml_2.3.6        xfun_0.31        
    [21] fastmap_1.1.0     withr_2.5.0       stringr_1.4.1     knitr_1.39       
    [25] xml2_1.3.3        vctrs_0.5.1       hms_1.1.2         desc_1.4.2       
    [29] fs_1.5.2          devtools_2.4.3    rprojroot_2.0.3   here_1.0.1       
    [33] glue_1.6.2        R6_2.5.1          processx_3.7.0    fansi_1.0.3      
    [37] rmarkdown_2.14    sessioninfo_1.2.2 tzdb_0.3.0        readr_2.1.3      
    [41] callr_3.7.1       purrr_0.3.5       magrittr_2.0.3    ps_1.7.1         
    [45] ellipsis_0.3.2    htmltools_0.5.3   usethis_2.1.6     utf8_1.2.2       
    [49] stringi_1.7.8     cachem_1.0.6      crayon_1.5.2      brio_1.1.3       
