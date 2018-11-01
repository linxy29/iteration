Iteration\_listcols
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

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
set.seed(1)
```

``` r
vec_numeric = 5:8
vec_char = c("My", "name", "is", "Jeff")
vec_logical = c(TRUE, TRUE, TRUE, FALSE)

l = list(vec_numeric = 5:8,
         mat         = matrix(1:8, 2, 4),
         vec_logical = c(TRUE, FALSE),
         summary     = summary(rnorm(1000)))

l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

for loops
=========

``` r
df = data_frame(
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

is.list(df)
```

    ## [1] TRUE

``` r
df[[2]]   
```

    ##  [1] -1.63244797  3.87002606  3.92503200  3.81623040  1.47404380
    ##  [6] -6.26177962 -5.04751876  3.75695597 -6.54176756  2.63770049
    ## [11] -2.66769787 -1.99188007 -3.94784725 -1.15070568  4.38592421
    ## [16]  2.26866589 -1.16232074  4.35002762  8.28001867 -0.03184464

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  c(mean_x, sd_x)
}
```

apply function to `df`

``` r
mean_and_sd(df[[1]])
```

    ## [1] 2.700153 1.120893

``` r
mean_and_sd(df[[2]])
```

    ## [1] 0.4164407 4.0787028

``` r
mean_and_sd(df[[3]])
```

    ## [1] 10.0728152  0.1905608

``` r
mean_and_sd(df[[4]])
```

    ## [1] -3.431261  1.181946

write a for loop

``` r
output1 = vector("list", length = 4)     #????????two square brackets

for (i in 1:4){
  output1[[i]] = mean_and_sd(df[[i]])     
}

output1
```

    ## [[1]]
    ## [1] 2.700153 1.120893
    ## 
    ## [[2]]
    ## [1] 0.4164407 4.0787028
    ## 
    ## [[3]]
    ## [1] 10.0728152  0.1905608
    ## 
    ## [[4]]
    ## [1] -3.431261  1.181946

map statement
=============

let's replace `for` with `map`

``` r
output2 = map(df, mean_and_sd)

output2[1]
```

    ## $a
    ## [1] 2.700153 1.120893

other map function

``` r
output = map_df(df, mean_and_sd)
```

code syntax
===========

be clear about arguments

``` r
output = map(.x = df, ~ mean(x = .x, na.rm = TRUE))
output
```

    ## $a
    ## [1] 2.700153
    ## 
    ## $b
    ## [1] 0.4164407
    ## 
    ## $c
    ## [1] 10.07282
    ## 
    ## $d
    ## [1] -3.431261

more complicate function

``` r
lotr_data = map2_df(
  .x = cell_ranges, .y = movie_names, 
  ~lotr_load_and_tidy(path = "./data/LotR_Words.xlsx", range = .x, movie_name = .y))
```

assessment

``` r
read_page_reviews <- function(url) {
  
  library(rvest)
  
  h = read_html(url)
  
  title = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  data_frame(title, stars, text)
}

url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

reviews_output = map(vec_urls, read_page_reviews)
```

List colums ...
===============

``` r
library(rnoaa)

weather = 
  meteo_pull_monitors(c("USW00094728", "USC00519397", "USS0023B17S"),
                      var = c("PRCP", "TMIN", "TMAX"), 
                      date_min = "2016-01-01",
                      date_max = "2016-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY", 
                      USC00519397 = "Waikiki_HA",
                      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

``` r
#centralpark = weather %>% filter(...)

weather_nest =
  weather %>% 
  nest(date:tmin)
```

``` r
weather_nest %>% 
  pull(data)
```

    ## [[1]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   5.6   1.1
    ##  2 2016-01-02     0   4.4   0  
    ##  3 2016-01-03     0   7.2   1.7
    ##  4 2016-01-04     0   2.2  -9.9
    ##  5 2016-01-05     0  -1.6 -11.6
    ##  6 2016-01-06     0   5    -3.8
    ##  7 2016-01-07     0   7.8  -0.5
    ##  8 2016-01-08     0   7.8  -0.5
    ##  9 2016-01-09     0   8.3   4.4
    ## 10 2016-01-10   457  15     4.4
    ## # ... with 356 more rows
    ## 
    ## [[2]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0  29.4  16.7
    ##  2 2016-01-02     0  28.3  16.7
    ##  3 2016-01-03     0  28.3  16.7
    ##  4 2016-01-04     0  28.3  16.1
    ##  5 2016-01-05     0  27.2  16.7
    ##  6 2016-01-06     0  27.2  20  
    ##  7 2016-01-07    46  27.8  18.3
    ##  8 2016-01-08     3  28.3  17.8
    ##  9 2016-01-09     8  27.8  19.4
    ## 10 2016-01-10     3  28.3  18.3
    ## # ... with 356 more rows
    ## 
    ## [[3]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   1.7  -5.9
    ##  2 2016-01-02    25  -0.1  -6  
    ##  3 2016-01-03     0  -5   -10  
    ##  4 2016-01-04    25   0.3  -9.8
    ##  5 2016-01-05    25   1.9  -1.8
    ##  6 2016-01-06    25   1.4  -2.6
    ##  7 2016-01-07     0   1.4  -3.9
    ##  8 2016-01-08     0   1.1  -4  
    ##  9 2016-01-09     0   1.4  -4.5
    ## 10 2016-01-10     0   2.3  -3.8
    ## # ... with 356 more rows

``` r
lm(tmax ~ tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045

write a function

``` r
weather_lm = function(df){
  
  lm(tmax ~ tmin, data = df)
}

weather_lm(df = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

Save output as a new list column...

``` r
weather_nest %>% 
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 x 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [366 × 4]> <S3: lm>  
    ## 2 Waikiki_HA     USC00519397 <tibble [366 × 4]> <S3: lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [366 × 4]> <S3: lm>

``` r
weather_nest %>% 
  unnest()
```

    ## # A tibble: 1,098 x 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2016-01-01     0   5.6   1.1
    ##  2 CentralPark_NY USW00094728 2016-01-02     0   4.4   0  
    ##  3 CentralPark_NY USW00094728 2016-01-03     0   7.2   1.7
    ##  4 CentralPark_NY USW00094728 2016-01-04     0   2.2  -9.9
    ##  5 CentralPark_NY USW00094728 2016-01-05     0  -1.6 -11.6
    ##  6 CentralPark_NY USW00094728 2016-01-06     0   5    -3.8
    ##  7 CentralPark_NY USW00094728 2016-01-07     0   7.8  -0.5
    ##  8 CentralPark_NY USW00094728 2016-01-08     0   7.8  -0.5
    ##  9 CentralPark_NY USW00094728 2016-01-09     0   8.3   4.4
    ## 10 CentralPark_NY USW00094728 2016-01-10   457  15     4.4
    ## # ... with 1,088 more rows

``` r
dynamite_reviews = 
  tibble(page = 1:5,
         urls = str_c(url_base, page)) %>% 
  mutate(reviews = map(vec_urls, read_page_reviews)) %>% 
  unnest()
```

``` r
lotr_cell_ranges = 
  tibble(
    movie = c("fellowship_ring", "two_towers", "return_king"),
    cells = c("B3:D6", "F3:H6", "J3:L6")
  )

lotr_tidy = 
  lotr_cell_ranges %>% 
  mutate(
    word_data = map(cells, ~readxl::read_excel("./data/LotR_Words.xlsx", range = .x))
  ) %>% 
  unnest() %>% 
  janitor::clean_names() %>% 
  gather(key = sex, value = words, female:male) %>%
  mutate(race = tolower(race)) %>% 
  select(movie, everything(), -cells) 
```
