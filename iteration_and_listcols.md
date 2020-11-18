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
    ## -2.12009 -0.69887  0.10606  0.09173  0.81211  2.67136

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
    ##  [1] 2.450945 2.479394 1.660813 2.817215 3.943318 2.920186 3.051391 2.847946
    ##  [9] 2.575894 1.955505 1.460819 4.889065 2.134994 2.423166 2.647952 3.285467
    ## [17] 2.356755 3.736553 4.867187 4.200916
    ## 
    ## $b
    ##  [1] -7.4218873 -2.1209851  7.1736136  3.5005289 -5.2566897 -2.0162184
    ##  [7] -8.9658106  4.5669695 -6.2936425  4.0395836 -2.7072197  4.9431438
    ## [13]  3.5357131 -2.2529312  0.2913331  3.6624368  3.7315278  2.3364241
    ## [19] -9.1571179  2.2653198 -1.2810384 -3.6928138  5.4359974  7.0605514
    ## [25] -3.8893868  0.4466828 -4.1564236  1.7017794 -4.3306701 15.0119664
    ## 
    ## $c
    ##  [1] 10.215559  9.824841 10.064115 10.018612 10.155448  9.991293  9.877885
    ##  [8]  9.897616 10.197024  9.866104 10.084137  9.734001  9.917506 10.252737
    ## [15]  9.968354 10.116385  9.848383  9.690826 10.305368 10.061104 10.018177
    ## [22]  9.870077  9.949974  9.689198 10.039972 10.099829 10.092580  9.710872
    ## [29] 10.141036 10.141645  9.885575  9.755653 10.172216 10.118638 10.072410
    ## [36]  9.837506  9.732833 10.011742 10.033312  9.716307
    ## 
    ## $d
    ##  [1] -2.3683998 -3.5256283 -0.2400808 -3.9764071 -2.4459960 -3.5370581
    ##  [7] -1.3723292 -2.9431287 -3.4489993 -3.5323296 -2.3946441 -2.1296095
    ## [13] -4.1528756 -4.6771522 -3.3787694 -0.6345601 -1.2761560 -3.7762354
    ## [19] -4.5632709 -4.2975578

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
    ## 1  2.94 0.963

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.205  5.43

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.169

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.93  1.29

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
    ## 1  2.94 0.963
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.205  5.43
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.169
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.93  1.29

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
    ##  [1] 2.450945 2.479394 1.660813 2.817215 3.943318 2.920186 3.051391 2.847946
    ##  [9] 2.575894 1.955505 1.460819 4.889065 2.134994 2.423166 2.647952 3.285467
    ## [17] 2.356755 3.736553 4.867187 4.200916
    ## 
    ## $b
    ##  [1] -7.4218873 -2.1209851  7.1736136  3.5005289 -5.2566897 -2.0162184
    ##  [7] -8.9658106  4.5669695 -6.2936425  4.0395836 -2.7072197  4.9431438
    ## [13]  3.5357131 -2.2529312  0.2913331  3.6624368  3.7315278  2.3364241
    ## [19] -9.1571179  2.2653198 -1.2810384 -3.6928138  5.4359974  7.0605514
    ## [25] -3.8893868  0.4466828 -4.1564236  1.7017794 -4.3306701 15.0119664
    ## 
    ## $c
    ##  [1] 10.215559  9.824841 10.064115 10.018612 10.155448  9.991293  9.877885
    ##  [8]  9.897616 10.197024  9.866104 10.084137  9.734001  9.917506 10.252737
    ## [15]  9.968354 10.116385  9.848383  9.690826 10.305368 10.061104 10.018177
    ## [22]  9.870077  9.949974  9.689198 10.039972 10.099829 10.092580  9.710872
    ## [29] 10.141036 10.141645  9.885575  9.755653 10.172216 10.118638 10.072410
    ## [36]  9.837506  9.732833 10.011742 10.033312  9.716307
    ## 
    ## $d
    ##  [1] -2.3683998 -3.5256283 -0.2400808 -3.9764071 -2.4459960 -3.5370581
    ##  [7] -1.3723292 -2.9431287 -3.4489993 -3.5323296 -2.3946441 -2.1296095
    ## [13] -4.1528756 -4.6771522 -3.3787694 -0.6345601 -1.2761560 -3.7762354
    ## [19] -4.5632709 -4.2975578

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

    ##  [1] 2.450945 2.479394 1.660813 2.817215 3.943318 2.920186 3.051391 2.847946
    ##  [9] 2.575894 1.955505 1.460819 4.889065 2.134994 2.423166 2.647952 3.285467
    ## [17] 2.356755 3.736553 4.867187 4.200916

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.94 0.963

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.205  5.43

Can I just map to iterate the function through?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.94 0.963
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.205  5.43
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.169
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.93  1.29

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
    ## 1 a     <dbl [20]>          2.94  0.963
    ## 2 b     <dbl [30]>          0.205 5.43 
    ## 3 c     <dbl [40]>          9.98  0.169
    ## 4 d     <dbl [20]>         -2.93  1.29

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
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>   2.73 
    ## 2 b     <dbl [30]>   <tibble [1 × 2]>   0.369
    ## 3 c     <dbl [40]>   <tibble [1 × 2]>  10.0  
    ## 4 d     <dbl [20]>   <tibble [1 × 2]>  -3.41
