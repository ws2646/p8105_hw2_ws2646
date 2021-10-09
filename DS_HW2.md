Homework 2
================
Weize Sun
10/6/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## Problem 1

``` r
trash_wheel_df = readxl::read_excel(
  "./data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",
  sheet = "Mr. Trash Wheel",
  range = "A2:N534") %>% 
  janitor::clean_names() %>% 
  drop_na(dumpster) %>% 
  mutate(sports_balls = round(sports_balls))
```

``` r
pre_19 = readxl::read_excel(
  "./data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",
  sheet = "2019 Precipitation",
  range = "A2:B14") %>% 
  janitor::clean_names() %>% 
  drop_na(total) %>% 
  mutate(year = 2019) %>% 
  relocate(year)

pre_18 = readxl::read_excel(
  "./data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",
  sheet = "2018 Precipitation",
  range = "A2:B14") %>% 
  janitor::clean_names() %>% 
  drop_na(total) %>% 
  mutate(year = 2018) %>% 
  relocate(year)

pre_1819 = full_join(pre_18, pre_19) %>% 
  mutate(month = month.name[month])
```

    ## Joining, by = c("year", "month", "total")

``` r
pre_1819
```

    ## # A tibble: 24 × 3
    ##     year month     total
    ##    <dbl> <chr>     <dbl>
    ##  1  2018 January    0.94
    ##  2  2018 February   4.8 
    ##  3  2018 March      2.69
    ##  4  2018 April      4.69
    ##  5  2018 May        9.27
    ##  6  2018 June       4.77
    ##  7  2018 July      10.2 
    ##  8  2018 August     6.45
    ##  9  2018 September 10.5 
    ## 10  2018 October    2.12
    ## # … with 14 more rows

``` r
sum(pull(pre_18, total))
```

    ## [1] 70.33

``` r
ball_19 = filter(trash_wheel_df, year == 2019)
median(pull(ball_19, sports_balls))
```

    ## [1] 9

\*\* The observation of trash\_wheel\_df is 345 with 14 variables, and
we are specifically paying attention to variable *sports\_balls*. The
observation of pre\_1819 is 24, and we are specifically looking at
variable *total*. For available data, the total precipitation in 2018 is
70.33, and the median number of sports balls in a dumpster in 2019 is 9.
\*\*
