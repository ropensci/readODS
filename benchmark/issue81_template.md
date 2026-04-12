# Testing the speed of reading the ODS file `issue81.ods`


``` r
date()
```

    [1] "Sun Apr 12 20:49:43 2026"

``` r
library(readODS)
file <- here::here("tests/testdata/issue81.ods")
system.time(x <- read_ods(file, sheet = 2, skip = 4))
```

       user  system elapsed 
      0.264   0.081   0.346 

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
    [1] readODS_2.3.5

    loaded via a namespace (and not attached):
     [1] digest_0.6.37     fastmap_1.2.0     xfun_0.56         cellranger_1.1.0 
     [5] tzdb_0.5.0        magrittr_2.0.5    glue_1.8.0        tibble_3.3.1     
     [9] knitr_1.50        pkgconfig_2.0.3   htmltools_0.5.8.1 rmarkdown_2.30   
    [13] lifecycle_1.0.5   cli_3.6.6         zip_2.3.3         vctrs_0.7.3      
    [17] compiler_4.5.3    rprojroot_2.1.1   here_1.0.1        tools_4.5.3      
    [21] pillar_1.11.1     evaluate_1.0.5    yaml_2.3.10       minty_0.0.6      
    [25] rlang_1.2.0       jsonlite_2.0.0    stringi_1.8.7    
