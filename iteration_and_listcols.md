Iteration and listcols
================
E. Brennan Bollman
20-11-17

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 10,
  fig.asp = 0.6,
  out.width = "90%"
)

theme_set(theme_bw() + theme(legend.position = "bottom")) 

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d()
scale_fill_discrete = scale_fill_viridis_d()
```

## Lists

Can put anything in list

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)

l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.3811 -0.7711 -0.1630 -0.1850  0.4463  1.8925

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = 0.2),
    d = rnorm(20, mean = -3, sd = 1)
  )

list_norm
```

    ## $a
    ##  [1] 2.7134197 3.8958913 1.0951564 2.5076160 2.8823743 3.2006259 3.4587717
    ##  [8] 3.3794001 1.4293678 1.5459811 0.5569044 2.2427372 3.6149594 2.4513555
    ## [15] 1.9610908 1.6965723 3.9603488 1.9852388 1.8652617 2.8387012
    ## 
    ## $b
    ##  [1]   6.71963739   2.22891932   5.05551185   2.96493987   0.27442746
    ##  [6]   4.27304678   1.34395506  -1.51794219  -1.80857503   0.09334675
    ## [11]   6.71038532   0.25157532   3.92205593  -0.38523104   4.55365857
    ## [16]  10.59512144  -2.54531199  -2.13635528  -3.99223611   2.31685926
    ## [21]  -1.73657956   3.98802055   0.22153838  -4.48186127  -9.50020548
    ## [26]   5.88538875  -4.05646868 -10.96796342   8.10710511   2.55240294
    ## 
    ## $c
    ##  [1] 10.172531  9.850326 10.025016 10.203576 10.028717 10.118963  9.946716
    ##  [8] 10.293811 10.002314 10.253622 10.084821 10.114020  9.989403  9.852216
    ## [15]  9.998877 10.041101  9.990789 10.069552 10.388584  9.853018 10.176577
    ## [22]  9.780531  9.792693  9.956437 10.000370  9.756379 10.005848  9.852524
    ## [29]  9.988244  9.914040 10.145850 10.146566  9.701008 10.381947 10.089609
    ## [36]  9.786257  9.764270  9.667517 10.136129  9.982785
    ## 
    ## $d
    ##  [1] -2.2805447 -2.5000729 -4.0676812 -5.2339600 -1.7612050 -3.8264228
    ##  [7] -3.0223584 -3.4729256 -2.3083801 -0.5775543 -4.1847643 -1.5346512
    ## [13] -3.7750096 -3.7904553 -1.4360717 -3.2797841 -4.5105288 -3.2439017
    ## [19] -2.0529415 -3.3209168

Pause and get my old function

``` r
mean_and_sd = function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.46 0.953

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.964  4.85

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.176

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.01  1.18

Let’s use a for loop

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.46 0.953
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.964  4.85
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.176
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.01  1.18
