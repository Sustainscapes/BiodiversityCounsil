Dataset generation for the Danish Biodiversity council
================
Derek Corcoran
09/06, 2022

# 1 Objective

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

``` r
library(terra)
library(magrittr)
library(geodata)
library(sf)
```

The goal of this repository is to generate the data sets necessary to
evaluate the current and potential area that is strictly and partially
protected in Denmark

Sea and land are treated separately

-   How much is 30% and 10% according to EU
    -   All natura 2.000 is in the 30%
-   How much is 30% and 10% according to Biodiversity counsel
    (Biodiversitetsradet)
    -   What has to be improved for natura 2.000 can be part of the 30%

## 1.1 Potential areas

-   NATURA 2000
    -   Habitat types
    -   How much is paragraph 3
    -   how much area of different categories

## 1.2 For each layer

-   Total Area by habitat type (SqKm or meters)
-   Proportion of Denmark
-   Source
-   Column 10% strictly protected EU
-   Column 10% strictly protected Biodiversitetsradet
-   Column 30% protected EU
-   Column 30% protected Biodiversitetsradet
-   Intersection (matrix NxN intersections?)

## 1.3 For sea

Map of preasures for each polygon

## 1.4 Deadlines

-   **31 may:** First figures

## 1.5 Test files

-   Natura 2000
-   BES_NATURETYPER_SHAPE
-   NATUR_VILDT_RESERVATER
-   Markblokke2021 Overlap with natura 2000 and paragraph 3

## 1.6 How interactive should we make this

<details style="\&quot;margin-bottom:10px;\&quot;">
<summary>
Session info
</summary>

``` r
sessioninfo::session_info()
#> - Session info ---------------------------------------------------------------
#>  setting  value
#>  version  R version 4.1.2 (2021-11-01)
#>  os       Windows Server 2012 R2 x64 (build 9600)
#>  system   x86_64, mingw32
#>  ui       RTerm
#>  language en
#>  collate  Danish_Denmark.1252
#>  ctype    Danish_Denmark.1252
#>  tz       Europe/Paris
#>  date     2022-06-09
#>  pandoc   2.14.0.3 @ C:/Program Files/RStudio/bin/pandoc/ (via rmarkdown)
#> 
#> - Packages -------------------------------------------------------------------
#>  package     * version date (UTC) lib source
#>  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.1.2)
#>  bookdown      0.24    2021-09-02 [1] CRAN (R 4.1.2)
#>  class         7.3-19  2021-05-03 [2] CRAN (R 4.1.2)
#>  classInt      0.4-3   2020-04-07 [1] CRAN (R 4.1.2)
#>  cli           3.1.0   2021-10-27 [1] CRAN (R 4.1.2)
#>  codetools     0.2-18  2020-11-04 [2] CRAN (R 4.1.2)
#>  crayon        1.4.2   2021-10-29 [1] CRAN (R 4.1.2)
#>  DBI           1.1.2   2021-12-20 [1] CRAN (R 4.1.2)
#>  digest        0.6.29  2021-12-01 [1] CRAN (R 4.1.2)
#>  dplyr         1.0.7   2021-06-18 [1] CRAN (R 4.1.2)
#>  e1071         1.7-9   2021-09-16 [1] CRAN (R 4.1.2)
#>  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.1.2)
#>  evaluate      0.14    2019-05-28 [1] CRAN (R 4.1.2)
#>  fansi         0.5.0   2021-05-25 [1] CRAN (R 4.1.2)
#>  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.1.2)
#>  generics      0.1.1   2021-10-25 [1] CRAN (R 4.1.2)
#>  geodata     * 0.3-5   2021-12-03 [1] CRAN (R 4.1.2)
#>  glue          1.5.1   2021-11-30 [1] CRAN (R 4.1.2)
#>  htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.1.2)
#>  KernSmooth    2.23-20 2021-05-03 [2] CRAN (R 4.1.2)
#>  knitr         1.37    2021-12-16 [1] CRAN (R 4.1.2)
#>  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.1.2)
#>  magrittr    * 2.0.1   2020-11-17 [1] CRAN (R 4.1.2)
#>  pillar        1.6.4   2021-10-18 [1] CRAN (R 4.1.2)
#>  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.1.2)
#>  proxy         0.4-26  2021-06-07 [1] CRAN (R 4.1.2)
#>  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.1.2)
#>  R6            2.5.1   2021-08-19 [1] CRAN (R 4.1.2)
#>  Rcpp          1.0.7   2021-07-07 [1] CRAN (R 4.1.2)
#>  rlang         0.4.12  2021-10-18 [1] CRAN (R 4.1.2)
#>  rmarkdown     2.11    2021-09-14 [1] CRAN (R 4.1.2)
#>  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.1.2)
#>  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.1.2)
#>  sf          * 1.0-4   2021-11-14 [1] CRAN (R 4.1.2)
#>  stringi       1.7.6   2021-11-29 [1] CRAN (R 4.1.2)
#>  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.1.2)
#>  terra       * 1.5-35  2022-05-18 [1] https://rspatial.r-universe.dev (R 4.1.3)
#>  tibble        3.1.6   2021-11-07 [1] CRAN (R 4.1.2)
#>  tidyselect    1.1.1   2021-04-30 [1] CRAN (R 4.1.2)
#>  units         0.7-2   2021-06-08 [1] CRAN (R 4.1.2)
#>  utf8          1.2.2   2021-07-24 [1] CRAN (R 4.1.2)
#>  vctrs         0.3.8   2021-04-29 [1] CRAN (R 4.1.2)
#>  xfun          0.29    2021-12-14 [1] CRAN (R 4.1.2)
#>  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.1.1)
#> 
#>  [1] C:/Users/au687614/Documents/R/win-library/4.1
#>  [2] C:/Program Files/R/R-4.1.2/library
#> 
#> ------------------------------------------------------------------------------
```

</details>
