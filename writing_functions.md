Writing functions
================
E. Brennan Bollman
20-11-15

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

    ##  [1]  0.6426001  0.6398961 -0.1880981 -1.4379481  0.6375554 -1.3665341
    ##  [7] -0.1386112  1.3585557  1.6669875  0.4736211  0.4564066 -0.1332025
    ## [13]  0.1015292 -1.1387137 -0.2192703  0.1490290 -2.1523565 -0.7270045
    ## [19]  1.9775640  0.6830790  1.4172915 -1.0459551 -0.3113223 -0.4863328
    ## [25]  0.5573051 -1.0579080 -1.5144784  0.2652405  0.7317118  0.1593630

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

    ##  [1]  0.6426001  0.6398961 -0.1880981 -1.4379481  0.6375554 -1.3665341
    ##  [7] -0.1386112  1.3585557  1.6669875  0.4736211  0.4564066 -0.1332025
    ## [13]  0.1015292 -1.1387137 -0.2192703  0.1490290 -2.1523565 -0.7270045
    ## [19]  1.9775640  0.6830790  1.4172915 -1.0459551 -0.3113223 -0.4863328
    ## [25]  0.5573051 -1.0579080 -1.5144784  0.2652405  0.7317118  0.1593630

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

Can’t take mean of character vector or dataset.

## Multiple outputs

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

Check function

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.46  2.91

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.10  4.32

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.86  2.95

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 4) {
  
  sim_data = 
    tibble(
    x = rnorm(n = samp_size, mean = mu, sd = sigma)
    )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.11  3.16

``` r
sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.67  3.17

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.25  3.46

Named matching versus positional matching.

Also note that can set defaults in function (which can overwrite if want
to)

## Function reading URL

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

``` r
read_page_reviews = function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 Boo                                     1 "We rented this movie because our …
    ##  2 Movie is still silly fun....amazon…     1 "We are getting really frustrated …
    ##  3 Brilliant and awkwardly funny.          5 "I've watched this movie repeatedl…
    ##  4 Great purchase price for great mov…     5 "Great movie and real good digital…
    ##  5 Movie for memories                      5 "I've been looking for this movie …
    ##  6 Love!                                   5 "Love this movie. Great quality"   
    ##  7 Hilarious!                              5 "Such a funny movie, definitely br…
    ##  8 napoleon dynamite                       5 "cool movie"                       
    ##  9 Top 5                                   5 "Best MOVIE ever! Funny one liners…
    ## 10 👍                                      5 "Exactly as described and came on …

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

all_reviews = 
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5]),
  )

all_reviews
```

    ## # A tibble: 10 x 3
    ##    title                     stars text                                         
    ##    <chr>                     <dbl> <chr>                                        
    ##  1 A top favorite movie !!       5 "Love this movie, needed to add it to my col…
    ##  2 Best.Movie!                   5 "I enjoyed showing my children this \"classi…
    ##  3 Great Movie                   5 "I love this movie. Showed it to my middle s…
    ##  4 Tina, you fat lard, come…     5 "A very quotable, awkard and hilarious movie…
    ##  5 Funny!                        4 "It is a great movie although it’s a little …
    ##  6 Excellent for families        5 "Highly recommend for family entertainment"  
    ##  7 Hilarious!                    5 "Hilarious!"                                 
    ##  8 Excellent in all fronts.      5 "Excellent in all fronts."                   
    ##  9 good                          5 "good"                                       
    ## 10 Buy                           5 "Very good movie not very expensive"

## Functions as arguments

``` r
my_summary = function(x, summ_func) {
  
  summ_func(x)
  
}

x_vec = rnorm(100, 3, 7)
mean(x_vec)
```

    ## [1] 2.814209

``` r
median(x_vec)
```

    ## [1] 2.322501

``` r
my_summary(x_vec, sd)
```

    ## [1] 7.097542

``` r
my_summary(x_vec, IQR)
```

    ## [1] 9.48047
