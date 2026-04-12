# Testing the speed of write_ods (append / update)


``` r
date()
```

    [1] "Sun Apr 12 20:49:26 2026"

``` r
library(readODS)
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
path <- tempfile(fileext = ".ods")
write_ods(df1, path = path)
system.time(write_ods(df1, path = path, sheet = "aaaa", append = TRUE))
```

       user  system elapsed 
      0.206   0.037   0.243 

``` r
system.time(write_ods(df1, path = path, sheet = "aaaa", update = TRUE))
```

       user  system elapsed 
      0.180   0.026   0.205 

``` r
system.time(write_ods(mtcars, path = path, sheet = "aaaa", update = TRUE))
```

       user  system elapsed 
      0.112   0.006   0.118 

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
     [1] digest_0.6.37     fastmap_1.2.0     xfun_0.56         glue_1.8.0       
     [5] knitr_1.50        htmltools_0.5.8.1 rmarkdown_2.30    lifecycle_1.0.5  
     [9] cli_3.6.6         zip_2.3.3         vctrs_0.7.3       withr_3.0.2      
    [13] compiler_4.5.3    tools_4.5.3       evaluate_1.0.5    pillar_1.11.1    
    [17] yaml_2.3.10       rlang_1.2.0       jsonlite_2.0.0    stringi_1.8.7    
