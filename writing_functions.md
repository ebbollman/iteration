Writing functions
================
E. Brennan Bollman
20-11-14

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

## Do something simple

Convert sample into collection of Z scores (subtract mean and divide by
standard deviation). Start by writing code to do the thing you want to
do and then make it a function.

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.4389383  0.4718136  0.6141437 -0.6787765 -0.4918469 -1.5326910
    ##  [7] -0.2318412  0.1562173 -0.5960789 -0.1519431  2.3611326  0.9227813
    ## [13] -0.9117963  0.4903994 -0.2931818 -0.9868442 -0.3179364  2.0962787
    ## [19]  0.1166139 -0.4099510 -1.4198906 -0.3282203 -0.1988691  0.7030933
    ## [25] -0.5097228 -1.0587539 -1.5321687  0.1252489  1.7876702  1.3661816

Want function to compute z-scores

``` r
z_scores = function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x_vec)
```

    ##  [1]  0.4389383  0.4718136  0.6141437 -0.6787765 -0.4918469 -1.5326910
    ##  [7] -0.2318412  0.1562173 -0.5960789 -0.1519431  2.3611326  0.9227813
    ## [13] -0.9117963  0.4903994 -0.2931818 -0.9868442 -0.3179364  2.0962787
    ## [19]  0.1166139 -0.4099510 -1.4198906 -0.3282203 -0.1988691  0.7030933
    ## [25] -0.5097228 -1.0587539 -1.5321687  0.1252489  1.7876702  1.3661816

Input = x\_vec function operates on x

Try function on other things. These should give errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

Can’t take mean of character vector or dataset
