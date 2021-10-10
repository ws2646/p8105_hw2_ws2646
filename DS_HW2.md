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

trash_wheel_df
```

    ## # A tibble: 453 × 14
    ##    dumpster month  year date                weight_tons volume_cubic_yards
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31                 18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74                 13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45                 15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                  15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06                 18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71                 13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                  8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                  16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52                 14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76                 18
    ## # … with 443 more rows, and 8 more variables: plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   grocery_bags <dbl>, chip_bags <dbl>, sports_balls <dbl>,
    ## #   homes_powered <dbl>

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

## Problem 2

``` r
pols_month_df = read.csv("./data/fivethirtyeight_datasets/pols-month.csv") %>% 
  janitor::clean_names() %>% 
  separate(mon, into = c("year", "month", "day"), sep = "-") %>% 
  mutate(month = as.numeric(month)) %>%
  mutate(month = month.name[month]) %>% 
  rename(republic = prez_gop, democratic = prez_dem) %>% 
  pivot_longer(
    c(republic, democratic),
    names_to = "president",
    values_to = "number") %>% 
  filter(number != 0) %>% 
  select(-day, -number) 

pols_month_df
```

    ## # A tibble: 822 × 9
    ##    year  month     gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president 
    ##    <chr> <chr>       <int>   <int>   <int>   <int>   <int>   <int> <chr>     
    ##  1 1947  January        23      51     253      23      45     198 democratic
    ##  2 1947  February       23      51     253      23      45     198 democratic
    ##  3 1947  March          23      51     253      23      45     198 democratic
    ##  4 1947  April          23      51     253      23      45     198 democratic
    ##  5 1947  May            23      51     253      23      45     198 democratic
    ##  6 1947  June           23      51     253      23      45     198 democratic
    ##  7 1947  July           23      51     253      23      45     198 democratic
    ##  8 1947  August         23      51     253      23      45     198 democratic
    ##  9 1947  September      23      51     253      23      45     198 democratic
    ## 10 1947  October        23      51     253      23      45     198 democratic
    ## # … with 812 more rows

``` r
snp_df = read.csv("./data/fivethirtyeight_datasets/snp.csv") %>% 
  separate(date, into = c("month", "day", "year"), sep = "/") %>% 
  relocate(year, month) %>% 
  mutate(month = as.numeric(month)) %>% 
  mutate(month = month.name[month]) %>% 
  select(-day) 

snp_before_2000 = filter(snp_df, year >= 20) %>% 
  mutate(year = as.character(as.numeric(year) + 1900))
snp_after_2000 = filter(snp_df, year <= 20) %>% 
  mutate(year = as.character(as.numeric(year) + 2000))

snp_final = bind_rows(snp_before_2000, snp_after_2000)

snp_final
```

    ##     year     month   close
    ## 1   1999  December 1469.25
    ## 2   1999  November 1388.91
    ## 3   1999   October 1362.93
    ## 4   1999 September 1282.71
    ## 5   1999    August 1320.41
    ## 6   1999      July 1328.72
    ## 7   1999      June 1372.71
    ## 8   1999       May 1301.84
    ## 9   1999     April 1335.18
    ## 10  1999     March 1286.37
    ## 11  1999  February 1238.33
    ## 12  1999   January 1279.64
    ## 13  1998  December 1229.23
    ## 14  1998  November 1163.63
    ## 15  1998   October 1098.67
    ## 16  1998 September 1017.01
    ## 17  1998    August  957.28
    ## 18  1998      July 1120.67
    ## 19  1998      June 1133.84
    ## 20  1998       May 1090.82
    ## 21  1998     April 1111.75
    ## 22  1998     March 1101.75
    ## 23  1998  February 1049.34
    ## 24  1998   January  980.28
    ## 25  1997  December  970.43
    ## 26  1997  November  955.40
    ## 27  1997   October  914.62
    ## 28  1997 September  947.28
    ## 29  1997    August  899.47
    ## 30  1997      July  954.31
    ## 31  1997      June  885.14
    ## 32  1997       May  848.28
    ## 33  1997     April  801.34
    ## 34  1997     March  757.12
    ## 35  1997  February  790.82
    ## 36  1997   January  786.16
    ## 37  1996  December  740.74
    ## 38  1996  November  757.02
    ## 39  1996   October  705.27
    ## 40  1996 September  687.33
    ## 41  1996    August  651.99
    ## 42  1996      July  639.95
    ## 43  1996      June  670.63
    ## 44  1996       May  669.12
    ## 45  1996     April  654.17
    ## 46  1996     March  645.50
    ## 47  1996  February  640.43
    ## 48  1996   January  636.02
    ## 49  1995  December  615.93
    ## 50  1995  November  605.37
    ## 51  1995   October  581.50
    ## 52  1995 September  584.41
    ## 53  1995    August  561.88
    ## 54  1995      July  562.06
    ## 55  1995      June  544.75
    ## 56  1995       May  533.40
    ## 57  1995     April  514.71
    ## 58  1995     March  500.71
    ## 59  1995  February  487.39
    ## 60  1995   January  470.42
    ## 61  1994  December  459.27
    ## 62  1994  November  453.69
    ## 63  1994   October  472.35
    ## 64  1994 September  462.71
    ## 65  1994    August  475.49
    ## 66  1994      July  458.26
    ## 67  1994      June  444.27
    ## 68  1994       May  456.50
    ## 69  1994     April  450.91
    ## 70  1994     March  445.77
    ## 71  1994  February  467.14
    ## 72  1994   January  481.61
    ## 73  1993  December  466.45
    ## 74  1993  November  461.79
    ## 75  1993   October  467.83
    ## 76  1993 September  458.93
    ## 77  1993    August  463.56
    ## 78  1993      July  448.13
    ## 79  1993      June  450.53
    ## 80  1993       May  450.19
    ## 81  1993     April  440.19
    ## 82  1993     March  451.67
    ## 83  1993  February  443.38
    ## 84  1993   January  438.78
    ## 85  1992  December  435.71
    ## 86  1992  November  431.35
    ## 87  1992   October  418.68
    ## 88  1992 September  417.80
    ## 89  1992    August  414.03
    ## 90  1992      July  424.21
    ## 91  1992      June  408.14
    ## 92  1992       May  415.35
    ## 93  1992     April  414.95
    ## 94  1992     March  403.69
    ## 95  1992  February  412.70
    ## 96  1992   January  408.78
    ## 97  1991  December  417.09
    ## 98  1991  November  375.22
    ## 99  1991   October  392.45
    ## 100 1991 September  387.86
    ## 101 1991    August  395.43
    ## 102 1991      July  387.81
    ## 103 1991      June  371.16
    ## 104 1991       May  389.83
    ## 105 1991     April  375.34
    ## 106 1991     March  375.22
    ## 107 1991  February  367.07
    ## 108 1991   January  343.93
    ## 109 1990  December  330.22
    ## 110 1990  November  322.22
    ## 111 1990   October  304.00
    ## 112 1990 September  306.05
    ## 113 1990    August  322.56
    ## 114 1990      July  356.15
    ## 115 1990      June  358.02
    ## 116 1990       May  361.23
    ## 117 1990     April  330.80
    ## 118 1990     March  339.94
    ## 119 1990  February  331.89
    ## 120 1990   January  329.08
    ## 121 1989  December  353.40
    ## 122 1989  November  345.99
    ## 123 1989   October  340.36
    ## 124 1989 September  349.15
    ## 125 1989    August  351.45
    ## 126 1989      July  346.08
    ## 127 1989      June  317.98
    ## 128 1989       May  320.52
    ## 129 1989     April  309.64
    ## 130 1989     March  294.87
    ## 131 1989  February  288.86
    ## 132 1989   January  297.47
    ## 133 1988  December  277.72
    ## 134 1988  November  273.70
    ## 135 1988   October  278.97
    ## 136 1988 September  271.91
    ## 137 1988    August  261.52
    ## 138 1988      July  272.02
    ## 139 1988      June  273.50
    ## 140 1988       May  262.16
    ## 141 1988     April  261.33
    ## 142 1988     March  258.89
    ## 143 1988  February  267.82
    ## 144 1988   January  257.07
    ## 145 1987  December  247.08
    ## 146 1987  November  230.30
    ## 147 1987   October  251.79
    ## 148 1987 September  321.83
    ## 149 1987    August  329.80
    ## 150 1987      July  318.66
    ## 151 1987      June  304.00
    ## 152 1987       May  290.10
    ## 153 1987     April  288.36
    ## 154 1987     March  291.70
    ## 155 1987  February  284.20
    ## 156 1987   January  274.08
    ## 157 1986  December  242.17
    ## 158 1986  November  249.22
    ## 159 1986   October  243.98
    ## 160 1986 September  231.32
    ## 161 1986    August  252.93
    ## 162 1986      July  236.12
    ## 163 1986      June  250.84
    ## 164 1986       May  247.35
    ## 165 1986     April  235.52
    ## 166 1986     March  238.90
    ## 167 1986  February  226.92
    ## 168 1986   January  211.78
    ## 169 1985  December  211.28
    ## 170 1985  November  202.17
    ## 171 1985   October  189.82
    ## 172 1985 September  182.08
    ## 173 1985    August  188.63
    ## 174 1985      July  190.92
    ## 175 1985      June  191.85
    ## 176 1985       May  189.55
    ## 177 1985     April  179.83
    ## 178 1985     March  180.66
    ## 179 1985  February  181.18
    ## 180 1985   January  179.63
    ## 181 1984  December  167.24
    ## 182 1984  November  163.58
    ## 183 1984   October  166.09
    ## 184 1984 September  166.10
    ## 185 1984    August  166.68
    ## 186 1984      July  150.66
    ## 187 1984      June  153.18
    ## 188 1984       May  150.55
    ## 189 1984     April  160.05
    ## 190 1984     March  159.18
    ## 191 1984  February  157.06
    ## 192 1984   January  163.41
    ## 193 1983  December  164.93
    ## 194 1983  November  166.40
    ## 195 1983   October  163.55
    ## 196 1983 September  166.07
    ## 197 1983    August  164.40
    ## 198 1983      July  162.56
    ## 199 1983      June  167.64
    ## 200 1983       May  162.39
    ## 201 1983     April  164.43
    ## 202 1983     March  152.96
    ## 203 1983  February  148.06
    ## 204 1983   January  145.30
    ## 205 1982  December  140.64
    ## 206 1982  November  138.53
    ## 207 1982   October  133.72
    ## 208 1982 September  120.42
    ## 209 1982    August  119.51
    ## 210 1982      July  107.09
    ## 211 1982      June  109.61
    ## 212 1982       May  111.88
    ## 213 1982     April  116.44
    ## 214 1982     March  111.96
    ## 215 1982  February  113.11
    ## 216 1982   January  120.40
    ## 217 1981  December  122.55
    ## 218 1981  November  126.35
    ## 219 1981   October  121.89
    ## 220 1981 September  116.18
    ## 221 1981    August  122.79
    ## 222 1981      July  130.92
    ## 223 1981      June  131.21
    ## 224 1981       May  132.59
    ## 225 1981     April  132.81
    ## 226 1981     March  136.00
    ## 227 1981  February  131.27
    ## 228 1981   January  129.55
    ## 229 1980  December  135.76
    ## 230 1980  November  140.52
    ## 231 1980   October  127.47
    ## 232 1980 September  125.46
    ## 233 1980    August  122.38
    ## 234 1980      July  121.67
    ## 235 1980      June  114.24
    ## 236 1980       May  111.24
    ## 237 1980     April  106.29
    ## 238 1980     March  102.09
    ## 239 1980  February  113.66
    ## 240 1980   January  114.16
    ## 241 1979  December  107.94
    ## 242 1979  November  106.16
    ## 243 1979   October  101.82
    ## 244 1979 September  109.32
    ## 245 1979    August  109.32
    ## 246 1979      July  103.81
    ## 247 1979      June  102.91
    ## 248 1979       May   99.08
    ## 249 1979     April  101.76
    ## 250 1979     March  101.59
    ## 251 1979  February   96.28
    ## 252 1979   January   99.93
    ## 253 1978  December   96.11
    ## 254 1978  November   94.70
    ## 255 1978   October   93.15
    ## 256 1978 September  102.54
    ## 257 1978    August  103.29
    ## 258 1978      July  100.68
    ## 259 1978      June   95.53
    ## 260 1978       May   97.24
    ## 261 1978     April   96.83
    ## 262 1978     March   89.21
    ## 263 1978  February   87.04
    ## 264 1978   January   89.25
    ## 265 1977  December   95.10
    ## 266 1977  November   94.83
    ## 267 1977   October   92.34
    ## 268 1977 September   96.53
    ## 269 1977    August   96.77
    ## 270 1977      July   98.85
    ## 271 1977      June  100.48
    ## 272 1977       May   96.12
    ## 273 1977     April   98.44
    ## 274 1977     March   98.42
    ## 275 1977  February   99.82
    ## 276 1977   January  102.03
    ## 277 1976  December  107.46
    ## 278 1976  November  102.10
    ## 279 1976   October  102.90
    ## 280 1976 September  105.24
    ## 281 1976    August  102.91
    ## 282 1976      July  103.44
    ## 283 1976      June  104.28
    ## 284 1976       May  100.18
    ## 285 1976     April  101.64
    ## 286 1976     March  102.77
    ## 287 1976  February   99.71
    ## 288 1976   January  100.86
    ## 289 1975  December   90.19
    ## 290 1975  November   91.24
    ## 291 1975   October   89.04
    ## 292 1975 September   83.87
    ## 293 1975    August   86.88
    ## 294 1975      July   88.75
    ## 295 1975      June   95.19
    ## 296 1975       May   91.15
    ## 297 1975     April   87.30
    ## 298 1975     March   83.36
    ## 299 1975  February   81.59
    ## 300 1975   January   76.98
    ## 301 1974  December   68.56
    ## 302 1974  November   69.97
    ## 303 1974   October   73.90
    ## 304 1974 September   63.54
    ## 305 1974    August   72.15
    ## 306 1974      July   79.31
    ## 307 1974      June   86.00
    ## 308 1974       May   87.28
    ## 309 1974     April   90.31
    ## 310 1974     March   93.98
    ## 311 1974  February   96.22
    ## 312 1974   January   96.57
    ## 313 1973  December   97.55
    ## 314 1973  November   95.96
    ## 315 1973   October  108.29
    ## 316 1973 September  108.43
    ## 317 1973    August  104.25
    ## 318 1973      July  108.22
    ## 319 1973      June  104.26
    ## 320 1973       May  104.95
    ## 321 1973     April  106.97
    ## 322 1973     March  111.52
    ## 323 1973  February  111.68
    ## 324 1973   January  116.03
    ## 325 1972  December  118.05
    ## 326 1972  November  116.67
    ## 327 1972   October  111.58
    ## 328 1972 September  110.55
    ## 329 1972    August  111.09
    ## 330 1972      July  107.39
    ## 331 1972      June  107.14
    ## 332 1972       May  109.53
    ## 333 1972     April  107.67
    ## 334 1972     March  107.20
    ## 335 1972  February  106.57
    ## 336 1972   January  103.94
    ## 337 1971  December  102.09
    ## 338 1971  November   93.99
    ## 339 1971   October   94.23
    ## 340 1971 September   98.34
    ## 341 1971    August   99.03
    ## 342 1971      July   95.58
    ## 343 1971      June   98.70
    ## 344 1971       May   99.63
    ## 345 1971     April  103.95
    ## 346 1971     March  100.31
    ## 347 1971  February   96.75
    ## 348 1971   January   95.88
    ## 349 1970  December   92.15
    ## 350 1970  November   87.20
    ## 351 1970   October   83.25
    ## 352 1970 September   84.30
    ## 353 1970    August   81.52
    ## 354 1970      July   78.05
    ## 355 1970      June   72.72
    ## 356 1970       May   76.55
    ## 357 1970     April   81.52
    ## 358 1970     March   89.63
    ## 359 1970  February   89.50
    ## 360 1970   January   85.02
    ## 361 1969  December   92.06
    ## 362 1969  November   93.81
    ## 363 1969   October   97.12
    ## 364 1969 September   93.12
    ## 365 1969    August   95.51
    ## 366 1969      July   91.83
    ## 367 1969      June   97.71
    ## 368 1969       May  103.46
    ## 369 1969     April  103.69
    ## 370 1969     March  101.51
    ## 371 1969  February   98.13
    ## 372 1969   January  103.01
    ## 373 1968  December  103.86
    ## 374 1968  November  108.37
    ## 375 1968   October  103.41
    ## 376 1968 September  102.67
    ## 377 1968    August   98.86
    ## 378 1968      July   97.74
    ## 379 1968      June   99.58
    ## 380 1968       May   98.68
    ## 381 1968     April   97.46
    ## 382 1968     March   90.20
    ## 383 1968  February   89.36
    ## 384 1968   January   92.24
    ## 385 1967  December   96.47
    ## 386 1967  November   94.00
    ## 387 1967   October   93.30
    ## 388 1967 September   96.71
    ## 389 1967    August   93.64
    ## 390 1967      July   94.75
    ## 391 1967      June   90.64
    ## 392 1967       May   89.08
    ## 393 1967     April   94.01
    ## 394 1967     March   90.20
    ## 395 1967  February   86.78
    ## 396 1967   January   86.61
    ## 397 1966  December   80.33
    ## 398 1966  November   80.45
    ## 399 1966   October   80.20
    ## 400 1966 September   76.56
    ## 401 1966    August   77.10
    ## 402 1966      July   83.60
    ## 403 1966      June   84.74
    ## 404 1966       May   86.13
    ## 405 1966     April   91.06
    ## 406 1966     March   89.23
    ## 407 1966  February   91.22
    ## 408 1966   January   92.88
    ## 409 1965  December   92.43
    ## 410 1965  November   91.61
    ## 411 1965   October   92.42
    ## 412 1965 September   89.96
    ## 413 1965    August   87.17
    ## 414 1965      July   85.25
    ## 415 1965      June   84.12
    ## 416 1965       May   88.42
    ## 417 1965     April   89.11
    ## 418 1965     March   86.16
    ## 419 1965  February   87.43
    ## 420 1965   January   87.56
    ## 421 1964  December   84.75
    ## 422 1964  November   84.42
    ## 423 1964   October   84.86
    ## 424 1964 September   84.18
    ## 425 1964    August   81.83
    ## 426 1964      July   83.18
    ## 427 1964      June   81.69
    ## 428 1964       May   80.37
    ## 429 1964     April   79.46
    ## 430 1964     March   78.98
    ## 431 1964  February   77.80
    ## 432 1964   January   77.04
    ## 433 1963  December   75.02
    ## 434 1963  November   73.23
    ## 435 1963   October   74.01
    ## 436 1963 September   71.70
    ## 437 1963    August   72.50
    ## 438 1963      July   69.13
    ## 439 1963      June   69.37
    ## 440 1963       May   70.80
    ## 441 1963     April   69.80
    ## 442 1963     March   66.57
    ## 443 1963  February   64.29
    ## 444 1963   January   66.20
    ## 445 1962  December   63.10
    ## 446 1962  November   62.26
    ## 447 1962   October   56.52
    ## 448 1962 September   56.27
    ## 449 1962    August   59.12
    ## 450 1962      July   58.23
    ## 451 1962      June   54.75
    ## 452 1962       May   59.63
    ## 453 1962     April   65.24
    ## 454 1962     March   69.55
    ## 455 1962  February   69.96
    ## 456 1962   January   68.84
    ## 457 1961  December   71.55
    ## 458 1961  November   71.32
    ## 459 1961   October   68.62
    ## 460 1961 September   66.73
    ## 461 1961    August   68.07
    ## 462 1961      July   66.76
    ## 463 1961      June   64.64
    ## 464 1961       May   66.56
    ## 465 1961     April   65.31
    ## 466 1961     March   65.06
    ## 467 1961  February   63.44
    ## 468 1961   January   61.78
    ## 469 1960  December   58.11
    ## 470 1960  November   55.54
    ## 471 1960   October   53.39
    ## 472 1960 September   53.52
    ## 473 1960    August   56.96
    ## 474 1960      July   55.51
    ## 475 1960      June   56.92
    ## 476 1960       May   55.83
    ## 477 1960     April   54.37
    ## 478 1960     March   55.34
    ## 479 1960  February   56.12
    ## 480 1960   January   55.61
    ## 481 1959  December   59.89
    ## 482 1959  November   58.28
    ## 483 1959   October   57.52
    ## 484 1959 September   56.88
    ## 485 1959    August   59.60
    ## 486 1959      July   60.51
    ## 487 1959      June   58.47
    ## 488 1959       May   58.68
    ## 489 1959     April   57.59
    ## 490 1959     March   55.44
    ## 491 1959  February   55.41
    ## 492 1959   January   55.45
    ## 493 1958  December   55.21
    ## 494 1958  November   52.48
    ## 495 1958   October   51.33
    ## 496 1958 September   50.06
    ## 497 1958    August   47.75
    ## 498 1958      July   47.19
    ## 499 1958      June   45.24
    ## 500 1958       May   44.09
    ## 501 1958     April   43.44
    ## 502 1958     March   42.10
    ## 503 1958  February   40.84
    ## 504 1958   January   41.70
    ## 505 1957  December   39.99
    ## 506 1957  November   41.72
    ## 507 1957   October   41.06
    ## 508 1957 September   42.42
    ## 509 1957    August   45.22
    ## 510 1957      July   47.91
    ## 511 1957      June   47.37
    ## 512 1957       May   47.43
    ## 513 1957     April   45.74
    ## 514 1957     March   44.11
    ## 515 1957  February   43.26
    ## 516 1957   January   44.72
    ## 517 1956  December   46.67
    ## 518 1956  November   45.08
    ## 519 1956   October   45.58
    ## 520 1956 September   45.35
    ## 521 1956    August   47.51
    ## 522 1956      July   49.39
    ## 523 1956      June   46.97
    ## 524 1956       May   45.20
    ## 525 1956     April   48.38
    ## 526 1956     March   48.48
    ## 527 1956  February   45.34
    ## 528 1956   January   43.82
    ## 529 1955  December   45.48
    ## 530 1955  November   45.51
    ## 531 1955   October   42.34
    ## 532 1955 September   43.67
    ## 533 1955    August   43.18
    ## 534 1955      July   43.52
    ## 535 1955      June   41.03
    ## 536 1955       May   37.91
    ## 537 1955     April   37.96
    ## 538 1955     March   36.58
    ## 539 1955  February   36.76
    ## 540 1955   January   36.63
    ## 541 1954  December   35.98
    ## 542 1954  November   34.24
    ## 543 1954   October   31.68
    ## 544 1954 September   32.31
    ## 545 1954    August   29.83
    ## 546 1954      July   30.88
    ## 547 1954      June   29.21
    ## 548 1954       May   29.19
    ## 549 1954     April   28.26
    ## 550 1954     March   26.94
    ## 551 1954  February   26.15
    ## 552 1954   January   26.08
    ## 553 1953  December   24.81
    ## 554 1953  November   24.76
    ## 555 1953   October   24.54
    ## 556 1953 September   23.35
    ## 557 1953    August   23.32
    ## 558 1953      July   24.75
    ## 559 1953      June   24.14
    ## 560 1953       May   24.54
    ## 561 1953     April   24.62
    ## 562 1953     March   25.29
    ## 563 1953  February   25.90
    ## 564 1953   January   26.38
    ## 565 1952  December   26.57
    ## 566 1952  November   25.66
    ## 567 1952   October   24.52
    ## 568 1952 September   24.54
    ## 569 1952    August   25.03
    ## 570 1952      July   25.40
    ## 571 1952      June   24.96
    ## 572 1952       May   23.86
    ## 573 1952     April   23.32
    ## 574 1952     March   24.37
    ## 575 1952  February   23.26
    ## 576 1952   January   24.14
    ## 577 1951  December   23.77
    ## 578 1951  November   22.88
    ## 579 1951   October   22.94
    ## 580 1951 September   23.26
    ## 581 1951    August   23.28
    ## 582 1951      July   22.40
    ## 583 1951      June   20.96
    ## 584 1951       May   21.52
    ## 585 1951     April   22.43
    ## 586 1951     March   21.48
    ## 587 1951  February   21.80
    ## 588 1951   January   21.66
    ## 589 1950  December   20.43
    ## 590 1950  November   19.51
    ## 591 1950   October   19.53
    ## 592 1950 September   19.45
    ## 593 1950    August   18.42
    ## 594 1950      July   17.84
    ## 595 1950      June   17.69
    ## 596 1950       May   18.78
    ## 597 1950     April   17.96
    ## 598 1950     March   17.29
    ## 599 1950  February   17.22
    ## 600 1950   January   17.05
    ## 601 2015      July 2079.65
    ## 602 2015      June 2063.11
    ## 603 2015       May 2107.39
    ## 604 2015     April 2085.51
    ## 605 2015     March 2067.89
    ## 606 2015  February 2104.50
    ## 607 2015   January 1994.99
    ## 608 2014  December 2058.90
    ## 609 2014  November 2067.56
    ## 610 2014   October 2018.05
    ## 611 2014 September 1972.29
    ## 612 2014    August 2003.37
    ## 613 2014      July 1930.67
    ## 614 2014      June 1960.23
    ## 615 2014       May 1923.57
    ## 616 2014     April 1883.95
    ## 617 2014     March 1872.34
    ## 618 2014  February 1859.45
    ## 619 2014   January 1782.59
    ## 620 2013  December 1848.36
    ## 621 2013  November 1805.81
    ## 622 2013   October 1756.54
    ## 623 2013 September 1681.55
    ## 624 2013    August 1632.97
    ## 625 2013      July 1685.73
    ## 626 2013      June 1606.28
    ## 627 2013       May 1630.74
    ## 628 2013     April 1597.57
    ## 629 2013     March 1569.19
    ## 630 2013  February 1514.68
    ## 631 2013   January 1498.11
    ## 632 2012  December 1426.19
    ## 633 2012  November 1416.18
    ## 634 2012   October 1412.16
    ## 635 2012 September 1440.67
    ## 636 2012    August 1406.58
    ## 637 2012      July 1379.32
    ## 638 2012      June 1362.16
    ## 639 2012       May 1310.33
    ## 640 2012     April 1397.91
    ## 641 2012     March 1408.47
    ## 642 2012  February 1365.68
    ## 643 2012   January 1312.41
    ## 644 2011  December 1257.60
    ## 645 2011  November 1246.96
    ## 646 2011   October 1253.30
    ## 647 2011 September 1131.42
    ## 648 2011    August 1218.89
    ## 649 2011      July 1292.28
    ## 650 2011      June 1320.64
    ## 651 2011       May 1345.20
    ## 652 2011     April 1363.61
    ## 653 2011     March 1325.83
    ## 654 2011  February 1327.22
    ## 655 2011   January 1286.12
    ## 656 2010  December 1257.64
    ## 657 2010  November 1180.55
    ## 658 2010   October 1183.26
    ## 659 2010 September 1141.20
    ## 660 2010    August 1049.33
    ## 661 2010      July 1101.60
    ## 662 2010      June 1030.71
    ## 663 2010       May 1089.41
    ## 664 2010     April 1186.69
    ## 665 2010     March 1169.43
    ## 666 2010  February 1104.49
    ## 667 2010   January 1073.87
    ## 668 2009  December 1115.10
    ## 669 2009  November 1095.63
    ## 670 2009   October 1036.19
    ## 671 2009 September 1057.08
    ## 672 2009    August 1020.62
    ## 673 2009      July  987.48
    ## 674 2009      June  919.32
    ## 675 2009       May  919.14
    ## 676 2009     April  872.81
    ## 677 2009     March  797.87
    ## 678 2009  February  735.09
    ## 679 2009   January  825.88
    ## 680 2008  December  903.25
    ## 681 2008  November  896.24
    ## 682 2008   October  968.75
    ## 683 2008 September 1166.36
    ## 684 2008    August 1282.83
    ## 685 2008      July 1267.38
    ## 686 2008      June 1280.00
    ## 687 2008       May 1400.38
    ## 688 2008     April 1385.59
    ## 689 2008     March 1322.70
    ## 690 2008  February 1330.63
    ## 691 2008   January 1378.55
    ## 692 2007  December 1468.36
    ## 693 2007  November 1481.14
    ## 694 2007   October 1549.38
    ## 695 2007 September 1526.75
    ## 696 2007    August 1473.99
    ## 697 2007      July 1455.27
    ## 698 2007      June 1503.35
    ## 699 2007       May 1530.62
    ## 700 2007     April 1482.37
    ## 701 2007     March 1420.86
    ## 702 2007  February 1406.82
    ## 703 2007   January 1438.24
    ## 704 2006  December 1418.30
    ## 705 2006  November 1400.63
    ## 706 2006   October 1377.94
    ## 707 2006 September 1335.85
    ## 708 2006    August 1303.82
    ## 709 2006      July 1276.66
    ## 710 2006      June 1270.20
    ## 711 2006       May 1270.09
    ## 712 2006     April 1310.61
    ## 713 2006     March 1294.87
    ## 714 2006  February 1280.66
    ## 715 2006   January 1280.08
    ## 716 2005  December 1248.29
    ## 717 2005  November 1249.48
    ## 718 2005   October 1207.01
    ## 719 2005 September 1228.81
    ## 720 2005    August 1220.33
    ## 721 2005      July 1234.18
    ## 722 2005      June 1191.33
    ## 723 2005       May 1191.50
    ## 724 2005     April 1156.85
    ## 725 2005     March 1180.59
    ## 726 2005  February 1203.60
    ## 727 2005   January 1181.27
    ## 728 2004  December 1211.92
    ## 729 2004  November 1173.82
    ## 730 2004   October 1130.20
    ## 731 2004 September 1114.58
    ## 732 2004    August 1104.24
    ## 733 2004      July 1101.72
    ## 734 2004      June 1140.84
    ## 735 2004       May 1120.68
    ## 736 2004     April 1107.30
    ## 737 2004     March 1126.21
    ## 738 2004  February 1144.94
    ## 739 2004   January 1131.13
    ## 740 2003  December 1111.92
    ## 741 2003  November 1058.20
    ## 742 2003   October 1050.71
    ## 743 2003 September  995.97
    ## 744 2003    August 1008.01
    ## 745 2003      July  990.31
    ## 746 2003      June  974.50
    ## 747 2003       May  963.59
    ## 748 2003     April  916.92
    ## 749 2003     March  848.18
    ## 750 2003  February  841.15
    ## 751 2003   January  855.70
    ## 752 2002  December  879.82
    ## 753 2002  November  936.31
    ## 754 2002   October  885.76
    ## 755 2002 September  815.28
    ## 756 2002    August  916.07
    ## 757 2002      July  911.62
    ## 758 2002      June  989.82
    ## 759 2002       May 1067.14
    ## 760 2002     April 1076.92
    ## 761 2002     March 1147.39
    ## 762 2002  February 1106.73
    ## 763 2002   January 1130.20
    ## 764 2001  December 1148.08
    ## 765 2001  November 1139.45
    ## 766 2001   October 1059.78
    ## 767 2001 September 1040.94
    ## 768 2001    August 1133.58
    ## 769 2001      July 1211.23
    ## 770 2001      June 1224.38
    ## 771 2001       May 1255.82
    ## 772 2001     April 1249.46
    ## 773 2001     March 1160.33
    ## 774 2001  February 1239.94
    ## 775 2001   January 1366.01
    ## 776 2000  December 1320.28
    ## 777 2000  November 1314.95
    ## 778 2000   October 1429.40
    ## 779 2000 September 1436.51
    ## 780 2000    August 1517.68
    ## 781 2000      July 1430.83
    ## 782 2000      June 1454.60
    ## 783 2000       May 1420.60
    ## 784 2000     April 1452.43
    ## 785 2000     March 1498.58
    ## 786 2000  February 1366.42
    ## 787 2000   January 1394.46

``` r
unemployment_df = read.csv("./data/fivethirtyeight_datasets/unemployment.csv") %>% 
  pivot_longer(
    Jan:Dec,
    names_to = "month",
    values_to = "unemployment"
  ) %>% 
  mutate(month = month.name[match(month,month.abb)]) %>% 
  rename(year = Year) %>% 
  mutate(year = as.character(year))

unemployment_df
```

    ## # A tibble: 816 × 3
    ##    year  month     unemployment
    ##    <chr> <chr>            <dbl>
    ##  1 1948  January            3.4
    ##  2 1948  February           3.8
    ##  3 1948  March              4  
    ##  4 1948  April              3.9
    ##  5 1948  May                3.5
    ##  6 1948  June               3.6
    ##  7 1948  July               3.6
    ##  8 1948  August             3.9
    ##  9 1948  September          3.8
    ## 10 1948  October            3.7
    ## # … with 806 more rows

``` r
pols_snp = left_join(pols_month_df, snp_final) 
```

    ## Joining, by = c("year", "month")

``` r
final_df = left_join(pols_snp, unemployment_df)
```

    ## Joining, by = c("year", "month")

``` r
final_df
```

    ## # A tibble: 822 × 11
    ##    year  month   gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president close
    ##    <chr> <chr>     <int>   <int>   <int>   <int>   <int>   <int> <chr>     <dbl>
    ##  1 1947  January      23      51     253      23      45     198 democrat…    NA
    ##  2 1947  Februa…      23      51     253      23      45     198 democrat…    NA
    ##  3 1947  March        23      51     253      23      45     198 democrat…    NA
    ##  4 1947  April        23      51     253      23      45     198 democrat…    NA
    ##  5 1947  May          23      51     253      23      45     198 democrat…    NA
    ##  6 1947  June         23      51     253      23      45     198 democrat…    NA
    ##  7 1947  July         23      51     253      23      45     198 democrat…    NA
    ##  8 1947  August       23      51     253      23      45     198 democrat…    NA
    ##  9 1947  Septem…      23      51     253      23      45     198 democrat…    NA
    ## 10 1947  October      23      51     253      23      45     198 democrat…    NA
    ## # … with 812 more rows, and 1 more variable: unemployment <dbl>

``` r
dim(pols_month_df)
```

    ## [1] 822   9

``` r
range(pull(pols_month_df, year))
```

    ## [1] "1947" "2015"

``` r
names(pols_month_df)
```

    ## [1] "year"      "month"     "gov_gop"   "sen_gop"   "rep_gop"   "gov_dem"  
    ## [7] "sen_dem"   "rep_dem"   "president"

``` r
dim(snp_final)
```

    ## [1] 787   3

``` r
range(pull(snp_final, year))
```

    ## [1] "1950" "2015"

``` r
names(snp_final)
```

    ## [1] "year"  "month" "close"

``` r
dim(unemployment_df)
```

    ## [1] 816   3

``` r
range(pull(unemployment_df, year))
```

    ## [1] "1948" "2015"

``` r
names(unemployment_df)
```

    ## [1] "year"         "month"        "unemployment"

``` r
dim(final_df)
```

    ## [1] 822  11

``` r
range(pull(final_df, year))
```

    ## [1] "1947" "2015"

``` r
names(final_df)
```

    ##  [1] "year"         "month"        "gov_gop"      "sen_gop"      "rep_gop"     
    ##  [6] "gov_dem"      "sen_dem"      "rep_dem"      "president"    "close"       
    ## [11] "unemployment"

**For pols\_month\_df, the dimension is (822 x 9), the range of year is
(1947, 2015), and key variables is “president”.**

**For snp\_final, the dimension is (787 x 3), the range of year is
(1950, 2015), and key variable is “close”.**

**For unemployment\_df, the dimension is (816 x 3), the range of year is
(1948, 2015), and key variable is “unemployment”.**

**For final\_df, the dimension is (822 x 11), the range of year is
(1947, 2015), and key variable is “president”, “close”,
“unemployment”.**

## Problem 3

``` r
popular_baby_names_df = 
  read.csv("./data/Popular_Baby_Names (1).csv") %>% 
  janitor::clean_names() %>% 
  mutate(ethnicity = recode(ethnicity, "ASIAN AND PACI" = "ASIAN AND PACIFIC ISLANDER",
                            "BLACK NON HISP" = "BLACK NON HISPANIC",
                            "WHITE NON HISP" = "WHITE NON HISPANIC"),
        child_s_first_name = str_to_sentence(child_s_first_name),
        gender = str_to_sentence(gender),
        ethnicity = str_to_sentence(ethnicity))

nrow(popular_baby_names_df)
```

    ## [1] 19418

``` r
popular_baby_names_df = 
  distinct(popular_baby_names_df, .keep_all= TRUE)

nrow(popular_baby_names_df)
```

    ## [1] 12181

``` r
olivia_rank_df = 
  popular_baby_names_df %>%
  filter(child_s_first_name == "Olivia" & gender == "Female") %>%
  select(-gender, -child_s_first_name, -count) %>% 
  pivot_wider(
    names_from = year_of_birth,
    values_from = rank
  )

popular_male_name_df = 
  popular_baby_names_df %>%
  filter(gender == "Male" & rank == 1) %>% 
  select(-gender, -rank, -count) %>% 
  pivot_wider(
    names_from = year_of_birth,
    values_from = child_s_first_name
  )

olivia_rank_df
```

    ## # A tibble: 4 × 7
    ##   ethnicity                  `2016` `2015` `2014` `2013` `2012` `2011`
    ##   <chr>                       <int>  <int>  <int>  <int>  <int>  <int>
    ## 1 Asian and pacific islander      1      1      1      3      3      4
    ## 2 Black non hispanic              8      4      8      6      8     10
    ## 3 Hispanic                       13     16     16     22     22     18
    ## 4 White non hispanic              1      1      1      1      4      2

``` r
popular_male_name_df
```

    ## # A tibble: 4 × 7
    ##   ethnicity                  `2016` `2015` `2014` `2013` `2012` `2011` 
    ##   <chr>                      <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  
    ## 1 Asian and pacific islander Ethan  Jayden Jayden Jayden Ryan   Ethan  
    ## 2 Black non hispanic         Noah   Noah   Ethan  Ethan  Jayden Jayden 
    ## 3 Hispanic                   Liam   Liam   Liam   Jayden Jayden Jayden 
    ## 4 White non hispanic         Joseph David  Joseph David  Joseph Michael

``` r
m_nonhis_2016 = 
  popular_baby_names_df %>% 
  filter(gender == "Male" & ethnicity == "White non hispanic" & year_of_birth == 2016)

ggplot(m_nonhis_2016, aes(x = rank, y = count)) +
  labs(title = "correlation between names of white non-hispanic male children and rank",
       x = "rank",
       y = "# of children") + geom_point()
```

![](DS_HW2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
