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
    ## -2.13984 -0.52536  0.16135  0.09846  0.61218  2.79822

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
    ##  [1] 2.5492070 2.3369878 3.1191053 0.6352955 4.3149090 2.1137999 1.4632252
    ##  [8] 2.4859152 2.7688613 1.9819195 4.6793449 3.5234786 3.6069964 3.2862355
    ## [15] 4.2331960 3.8887269 1.6347633 2.8162119 1.7372908 4.7801990
    ## 
    ## $b
    ##  [1]  4.61612919 -2.86601116 -2.81077582 -4.18450891  1.23660807 -4.07050339
    ##  [7]  2.58861602 -3.01018996 -2.47977306 -1.93984833 -5.18813741 -7.75697105
    ## [13] -0.55392515 -0.01880601  2.59530283 -0.13870257 -0.67400962 -6.60662055
    ## [19]  3.90358110 -5.02999663  4.67677773 -1.30683015  3.00064356 -1.84310055
    ## [25]  6.94012969 -4.53123088 -4.28880313  4.03145596  5.08259388  2.96980026
    ## 
    ## $c
    ##  [1]  9.672905  9.877276  9.856360  9.946594  9.937010 10.336503 10.044540
    ##  [8] 10.176681 10.129192 10.072760  9.804927 10.084522 10.282771  9.955561
    ## [15] 10.020533  9.873126 10.153831  9.952129  9.645074  9.802981 10.218162
    ## [22] 10.126461 10.129466 10.431835  9.833360  9.938262 10.093928 10.225165
    ## [29]  9.940554 10.109376  9.951150 10.142726 10.145847 10.077994  9.988051
    ## [36]  9.778869 10.016936 10.012046 10.226783 10.266424
    ## 
    ## $d
    ##  [1] -3.1734729 -3.5299197 -2.0869163 -4.2667805 -3.1361823 -1.8733494
    ##  [7] -3.8902325 -0.8281675 -2.6374193 -2.1651276 -3.4345325 -3.1413842
    ## [13] -1.9578103 -2.4029256 -3.0139483 -2.8374245 -4.0249294 -2.9073539
    ## [19] -1.7039249 -1.6446526

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
    ## 1  2.90  1.14

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.589  3.90

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
    ## 1 -2.73 0.894

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
    ## 1  2.90  1.14
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.589  3.90
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
    ## 1 -2.73 0.894

## Let’s try purr::map

``` r
output = map(list_norm, mean_and_sd)
```

what if you want a different function?

``` r
output = map(list_norm, median)
output = map(list_norm, IQR)
```

## Variants on map

``` r
output = map_dbl(list_norm, median)
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## List col

Want something that keeps track of input and output at same time.

First, create df that has listcolumn

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% 
  pull(samp)
```

    ## $a
    ##  [1] 2.5492070 2.3369878 3.1191053 0.6352955 4.3149090 2.1137999 1.4632252
    ##  [8] 2.4859152 2.7688613 1.9819195 4.6793449 3.5234786 3.6069964 3.2862355
    ## [15] 4.2331960 3.8887269 1.6347633 2.8162119 1.7372908 4.7801990
    ## 
    ## $b
    ##  [1]  4.61612919 -2.86601116 -2.81077582 -4.18450891  1.23660807 -4.07050339
    ##  [7]  2.58861602 -3.01018996 -2.47977306 -1.93984833 -5.18813741 -7.75697105
    ## [13] -0.55392515 -0.01880601  2.59530283 -0.13870257 -0.67400962 -6.60662055
    ## [19]  3.90358110 -5.02999663  4.67677773 -1.30683015  3.00064356 -1.84310055
    ## [25]  6.94012969 -4.53123088 -4.28880313  4.03145596  5.08259388  2.96980026
    ## 
    ## $c
    ##  [1]  9.672905  9.877276  9.856360  9.946594  9.937010 10.336503 10.044540
    ##  [8] 10.176681 10.129192 10.072760  9.804927 10.084522 10.282771  9.955561
    ## [15] 10.020533  9.873126 10.153831  9.952129  9.645074  9.802981 10.218162
    ## [22] 10.126461 10.129466 10.431835  9.833360  9.938262 10.093928 10.225165
    ## [29]  9.940554 10.109376  9.951150 10.142726 10.145847 10.077994  9.988051
    ## [36]  9.778869 10.016936 10.012046 10.226783 10.266424
    ## 
    ## $d
    ##  [1] -3.1734729 -3.5299197 -2.0869163 -4.2667805 -3.1361823 -1.8733494
    ##  [7] -3.8902325 -0.8281675 -2.6374193 -2.1651276 -3.4345325 -3.1413842
    ## [13] -1.9578103 -2.4029256 -3.0139483 -2.8374245 -4.0249294 -2.9073539
    ## [19] -1.7039249 -1.6446526

``` r
listcol_df %>% 
  pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Try operations

``` r
listcol_df$samp[[1]]
```

    ##  [1] 2.5492070 2.3369878 3.1191053 0.6352955 4.3149090 2.1137999 1.4632252
    ##  [8] 2.4859152 2.7688613 1.9819195 4.6793449 3.5234786 3.6069964 3.2862355
    ## [15] 4.2331960 3.8887269 1.6347633 2.8162119 1.7372908 4.7801990

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.90  1.14

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.589  3.90

Can I just map to iterate the function through?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.90  1.14
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.589  3.90
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.176
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.73 0.894

listcol is in df, but still map across an element of this list

## Want to apply map function to each element of listcol and save output in my df

Can I add a list column?

``` r
listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd)
  )
```

    ## # A tibble: 4 x 3
    ##   name  samp         summary         
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>
    ## 2 b     <dbl [30]>   <tibble [1 × 2]>
    ## 3 c     <dbl [40]>   <tibble [1 × 2]>
    ## 4 d     <dbl [20]>   <tibble [1 × 2]>

``` r
listcol_df %>% 
  mutate(
    summary = map_df(samp, mean_and_sd)
  )
```

    ## # A tibble: 4 x 3
    ##   name  samp         summary$mean   $sd
    ##   <chr> <named list>        <dbl> <dbl>
    ## 1 a     <dbl [20]>          2.90  1.14 
    ## 2 b     <dbl [30]>         -0.589 3.90 
    ## 3 c     <dbl [40]>         10.0   0.176
    ## 4 d     <dbl [20]>         -2.73  0.894

``` r
listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median)
  )
```

    ## # A tibble: 4 x 4
    ##   name  samp         summary          medians
    ##   <chr> <named list> <named list>       <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>   2.79 
    ## 2 b     <dbl [30]>   <tibble [1 × 2]>  -0.990
    ## 3 c     <dbl [40]>   <tibble [1 × 2]>  10.0  
    ## 4 d     <dbl [20]>   <tibble [1 × 2]>  -2.87
