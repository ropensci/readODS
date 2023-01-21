Testing the speed of write_ods
================

``` r
date()
```

    [1] "Sat Jan 21 20:49:38 2023"

``` r
devtools::load_all()
```

    ℹ Loading readODS

``` r
## generate a 3000 x 8 data.frame
set.seed(721831)
df1 <- data.frame(a1 = sample(c(1:100), size = 3000, replace = TRUE),
                  b1 = sample(c(3.14, 3.1416, 12.345, 721.831), size = 3000, replace = TRUE),
                  c1 = sample(LETTERS, size = 3000, replace = TRUE),
                  d1 = sample(c(1L:100L), size = 3000, replace = TRUE),
                  a2 = sample(c(1:100), size = 3000, replace = TRUE),
                  b2 = sample(c(3.14, 3.1416, 12.345, 99.831), size = 3000, replace = TRUE),
                  c2 = sample(LETTERS, size = 3000, replace = TRUE),
                  d2 = sample(c(1L:100L), size = 3000, replace = TRUE))
system.time(write_ods(df1, path = tempfile(fileext = ".ods")))
```

       user  system elapsed 
      0.459   0.100   0.561 

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
     [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
     [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
     [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
     [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
    [1] readODS_1.8.0  testthat_3.1.4

    loaded via a namespace (and not attached):
     [1] pillar_1.8.1      cellranger_1.1.0  compiler_4.2.2    prettyunits_1.1.1
     [5] remotes_2.4.2     tools_4.2.2       digest_0.6.30     pkgbuild_1.3.1   
     [9] pkgload_1.3.0     tibble_3.1.8      jsonlite_1.8.4    evaluate_0.15    
    [13] memoise_2.0.1     lifecycle_1.0.3   pkgconfig_2.0.3   rlang_1.0.6      
    [17] cli_3.6.0         rstudioapi_0.14   yaml_2.3.6        xfun_0.31        
    [21] fastmap_1.1.0     xml2_1.3.3        withr_2.5.0       stringr_1.4.1    
    [25] knitr_1.39        hms_1.1.2         desc_1.4.2        fs_1.5.2         
    [29] vctrs_0.5.1       devtools_2.4.3    rprojroot_2.0.3   glue_1.6.2       
    [33] R6_2.5.1          processx_3.7.0    fansi_1.0.3       rmarkdown_2.14   
    [37] sessioninfo_1.2.2 tzdb_0.3.0        readr_2.1.3       callr_3.7.1      
    [41] purrr_1.0.1       magrittr_2.0.3    ps_1.7.1          ellipsis_0.3.2   
    [45] htmltools_0.5.3   usethis_2.1.6     utf8_1.2.2        stringi_1.7.12   
    [49] cachem_1.0.6      crayon_1.5.2      brio_1.1.3       
