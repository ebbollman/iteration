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
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.12621 -0.54996  0.12491  0.09266  0.74130  2.44106

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
    ##  [1] 5.035988 2.180007 4.476954 2.427216 3.291051 1.271419 2.437689 3.646101
    ##  [9] 3.017483 1.640681 2.752368 4.553871 4.462952 4.295873 3.117999 2.574034
    ## [17] 3.687763 2.826945 2.321891 4.648804
    ## 
    ## $b
    ##  [1]   4.1994268   2.5122981   4.7243320  -4.3075661  -5.1720417  -3.7944123
    ##  [7] -13.7479879   0.3001158   0.8015154   5.6080232   2.0421029   8.9630041
    ## [13]   6.4217063   4.6826544   5.5961275   4.9795673  -0.2281122   3.5976022
    ## [19]  -2.0238040   4.5153159  -3.4998545  -7.9617914   4.5044114   2.4516285
    ## [25]  -0.3826817  -2.4760227   4.3759115  -1.9660151   4.7765690   1.0755347
    ## 
    ## $c
    ##  [1] 10.278121  9.893682 10.122313  9.720287 10.013703  9.866979  9.969101
    ##  [8] 10.221565 10.089286  9.713381 10.303148 10.330428 10.079994 10.093292
    ## [15] 10.126733  9.877417  9.856916  9.672840 10.281275  9.960103  9.909689
    ## [22] 10.255145 10.006637 10.021095  9.857210 10.383004 10.133902  9.673419
    ## [29]  9.981466  9.762301 10.105687  9.787436  9.795578  9.982427 10.293552
    ## [36] 10.021319 10.141584 10.058266 10.323677 10.051185
    ## 
    ## $d
    ##  [1] -1.9837102 -4.2884544 -2.1637486 -2.5179172 -3.2833528 -2.7529269
    ##  [7] -1.9317548 -3.9790508 -1.7517160 -5.2349311 -4.0711790  0.2441168
    ## [13] -2.0667040 -3.9842734 -5.7550725 -3.7779504 -1.6591440 -2.2565327
    ## [19] -4.4724741 -1.6361456

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
    ## 1  3.23  1.08

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.02  4.89

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.197

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.97  1.46

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
    ## 1  3.23  1.08
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.02  4.89
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.197
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.97  1.46

## Let’s try purr::map

``` r
output = map(list_norm, mean_and_sd)
```

what if you want a different function?

``` r
output = map(list_norm, median)
output = map(list_norm, IQR)
```
