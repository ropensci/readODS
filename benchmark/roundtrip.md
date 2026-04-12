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

    [1] "Sun Apr 12 20:48:48 2026"

``` r
library(nycflights13)
system.time(path <- writexl::write_xlsx(flights))
```

       user  system elapsed 
      6.641   0.321   6.967 

``` r
system.time(out <- readxl::read_xlsx(path))
```

       user  system elapsed 
      2.366   0.862   3.229 

``` r
all.equal(out, flights)
```

    [1] "Component \"time_hour\": 'tzone' attributes are inconsistent ('UTC' and 'America/New_York')"

The ODS version

``` r
library(readODS)
system.time(path <- readODS::write_ods(flights))
```

       user  system elapsed 
     12.642   0.611  13.367 

``` r
system.time(out <- readODS::read_ods(path))
```

       user  system elapsed 
      9.744   2.123  11.868 

``` r
all.equal(out, flights)
```

    [1] "Component \"time_hour\": 'tzone' attributes are inconsistent ('UTC' and 'America/New_York')"
    [2] "Component \"time_hour\": Mean absolute difference: 15618.15"                                

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
    [1] readODS_2.3.5      nycflights13_1.0.2

    loaded via a namespace (and not attached):
     [1] vctrs_0.7.3       zip_2.3.3         cli_3.6.6         knitr_1.50       
     [5] rlang_1.2.0       xfun_0.56         stringi_1.8.7     jsonlite_2.0.0   
     [9] glue_1.8.0        htmltools_0.5.8.1 readxl_1.4.3      writexl_1.5.1    
    [13] rmarkdown_2.30    cellranger_1.1.0  evaluate_1.0.5    tibble_3.3.1     
    [17] tzdb_0.5.0        fastmap_1.2.0     yaml_2.3.10       lifecycle_1.0.5  
    [21] compiler_4.5.3    pkgconfig_2.0.3   digest_0.6.37     pillar_1.11.1    
    [25] magrittr_2.0.5    minty_0.0.6       tools_4.5.3       withr_3.0.2      
