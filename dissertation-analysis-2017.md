dissertation analysis
================

``` r
library(qwraps2)
options(qwraps2_markup = "markdown")
library(readr)
library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(skimr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ tibble  3.1.2     ✓ stringr 1.4.0
    ## ✓ tidyr   1.1.3     ✓ forcats 0.5.1
    ## ✓ purrr   0.3.4

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
library(broom)
library(knitr)
library(SmartEDA)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
analysis17 <- read_csv("analysis17.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   X1 = col_double(),
    ##   location_name = col_character(),
    ##   GSNI_PERIOD = col_character(),
    ##   twobias = col_double(),
    ##   cvd17fem = col_double(),
    ##   cvd17both = col_double(),
    ##   LEbirth2019 = col_double(),
    ##   phy17edit = col_double(),
    ##   scl17 = col_double(),
    ##   GDP17 = col_double(),
    ##   MMR17 = col_double()
    ## )

``` r
setDT(analysis17)

analysis17[, GSNI_PERIOD := as.factor(GSNI_PERIOD)]
analysis17[, X1:= NULL]
```

\#2017

# CODE BOOK (analysis17)

| Variable       | Description                                                                                                      |
| -------------- | ---------------------------------------------------------------------------------------------------------------- |
| location\_name | country                                                                                                          |
| GSNI\_PERIOD   | year data collection took place for GSNI survey 2005-2009 or 2010-2017                                           |
| twobias        | share of people with at least two bias                                                                           |
| cvd17fem       | age standardised CVD mortality rates for women per | | | 100,000 population (from global burden of disease) 2017 |
| cvd17both      | as above for whole population                                                                                    |
| LEbirth2019    | life expectancy at birth for women in 2019, from WHO (years)                                                     |
| phy17edit      | physicians per 1000 population 2017 (world bank) data added from other years n = 19 (2017-16)                    |
| scl17          | mean years of school 2017 (ourworldindata)                                                                       |
| GDP17          | Gross domestic product per capita (current US | | | dollar) 2017 (world bank)                                    |
| MMR17          | Maternal Mortality Ratio 2017 (world bank)                                                                       |

\#Descriptive analysis

``` r
glimpse(analysis17)
```

    ## Rows: 75
    ## Columns: 10
    ## $ location_name <chr> "Algeria", "Andorra", "Argentina", "Armenia", "Australia…
    ## $ GSNI_PERIOD   <fct> 2010–2014, 2005–2009, 2010–2014, 2010–2014, 2010–2014, 2…
    ## $ twobias       <dbl> 87.00, 7.43, 42.49, 81.28, 23.00, 93.82, 71.70, 52.39, 4…
    ## $ cvd17fem      <dbl> 450.74425, 95.83302, 154.09804, 297.69330, 87.33218, 685…
    ## $ cvd17both     <dbl> 401.39469, 106.95304, 190.14065, 349.47783, 106.36798, 7…
    ## $ LEbirth2019   <dbl> 78.10, 84.95, 79.50, 79.20, 84.80, 74.10, 79.60, 79.40, …
    ## $ phy17edit     <dbl> 1.7879, 3.3333, 3.9901, 4.4023, 3.6778, 3.4460, 5.1905, …
    ## $ scl17         <dbl> 8.0, 10.2, 9.9, 11.7, 12.9, 10.7, 12.3, 7.8, 11.8, 1.5, …
    ## $ GDP17         <dbl> 4111.2941, 38962.8804, 14613.0418, 3914.5013, 54027.9668…
    ## $ MMR17         <dbl> 112, NA, 39, 26, 6, 26, 2, 60, 10, 320, 10, 13, 29, 83, …

``` r
summary(analysis17)
```

    ##  location_name         GSNI_PERIOD    twobias         cvd17fem     
    ##  Length:75          2005–2009:18   Min.   : 7.43   Min.   : 57.81  
    ##  Class :character   2010–2014:57   1st Qu.:41.94   1st Qu.:122.23  
    ##  Mode  :character                  Median :68.56   Median :226.82  
    ##                                    Mean   :62.83   Mean   :251.14  
    ##                                    3rd Qu.:86.33   3rd Qu.:345.94  
    ##                                    Max.   :98.07   Max.   :945.26  
    ##                                                                    
    ##    cvd17both        LEbirth2019      phy17edit          scl17       
    ##  Min.   :  77.09   Min.   :63.40   Min.   :0.0008   Min.   : 1.500  
    ##  1st Qu.: 153.66   1st Qu.:75.83   1st Qu.:1.0649   1st Qu.: 7.800  
    ##  Median : 253.26   Median :79.20   Median :2.3788   Median :10.200  
    ##  Mean   : 282.64   Mean   :78.13   Mean   :2.3466   Mean   : 9.557  
    ##  3rd Qu.: 373.43   3rd Qu.:82.99   3rd Qu.:3.4053   3rd Qu.:12.100  
    ##  Max.   :1026.67   Max.   :86.90   Max.   :6.1297   Max.   :14.100  
    ##                                    NA's   :4        NA's   :6       
    ##      GDP17           MMR17       
    ##  Min.   :  735   Min.   :  2.00  
    ##  1st Qu.: 3895   1st Qu.:  9.00  
    ##  Median : 9064   Median : 22.00  
    ##  Mean   :18417   Mean   : 86.16  
    ##  3rd Qu.:28567   3rd Qu.: 77.25  
    ##  Max.   :80450   Max.   :917.00  
    ##  NA's   :7       NA's   :5

``` r
analysis17noNA <- cbind(na.exclude(analysis17)) #exclude rows with NA as will prevent model comparison

#create data table without MMR
analysis17noMMR <- cbind(analysis17[, MMR17:=NULL])

analysis17noMMRnoNA <- cbind(na.exclude(analysis17noMMR))

skim(analysis17noNA) 
```

|                                                  |                |
| :----------------------------------------------- | :------------- |
| Name                                             | analysis17noNA |
| Number of rows                                   | 66             |
| Number of columns                                | 10             |
| Key                                              | NULL           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| character                                        | 1              |
| factor                                           | 1              |
| numeric                                          | 8              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| location\_name |          0 |              1 |   4 |  19 |     0 |        66 |          0 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts      |
| :------------- | ---------: | -------------: | :------ | --------: | :--------------- |
| GSNI\_PERIOD   |          0 |              1 | FALSE   |         2 | 201: 52, 200: 14 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |     mean |       sd |     p0 |     p25 |     p50 |      p75 |     p100 | hist  |
| :------------- | ---------: | -------------: | -------: | -------: | -----: | ------: | ------: | -------: | -------: | :---- |
| twobias        |          0 |              1 |    61.75 |    26.57 |  10.75 |   40.69 |   64.91 |    85.82 |    98.07 | ▃▃▃▃▇ |
| cvd17fem       |          0 |              1 |   245.51 |   158.66 |  57.81 |  121.59 |  223.47 |   317.70 |   945.26 | ▇▅▂▁▁ |
| cvd17both      |          0 |              1 |   274.93 |   168.45 |  77.09 |  153.34 |  248.34 |   348.65 |  1026.67 | ▇▅▁▁▁ |
| LEbirth2019    |          0 |              1 |    78.08 |     6.23 |  63.40 |   75.05 |   79.35 |    83.04 |    86.90 | ▂▂▃▇▆ |
| phy17edit      |          0 |              1 |     2.31 |     1.48 |   0.00 |    0.93 |    2.38 |     3.43 |     6.13 | ▇▇▇▃▁ |
| scl17          |          0 |              1 |     9.63 |     2.94 |   1.50 |    7.80 |   10.15 |    12.17 |    14.10 | ▁▂▆▆▇ |
| GDP17          |          0 |              1 | 18222.48 | 20731.50 | 734.99 | 3856.86 | 8606.76 | 27712.30 | 80449.99 | ▇▁▁▁▁ |
| MMR17          |          0 |              1 |    90.42 |   161.46 |   2.00 |    9.00 |   26.00 |    82.00 |   917.00 | ▇▁▁▁▁ |

\#\#\#\#\#Histograms

``` r
#ggplot(analysis17noNA, aes(x=twobias))+geom_histogram(binwidth=5, color = "Black", fill = "Pink")

analysis17noNA[,hist(twobias)]
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    ## $breaks
    ##  [1]  10  20  30  40  50  60  70  80  90 100
    ## 
    ## $counts
    ## [1]  3  9  4  9  6  3  5 16 11
    ## 
    ## $density
    ## [1] 0.004545455 0.013636364 0.006060606 0.013636364 0.009090909 0.004545455
    ## [7] 0.007575758 0.024242424 0.016666667
    ## 
    ## $mids
    ## [1] 15 25 35 45 55 65 75 85 95
    ## 
    ## $xname
    ## [1] "twobias"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
analysis17noNA[,hist(cvd17fem)]
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

    ## $breaks
    ##  [1]    0  100  200  300  400  500  600  700  800  900 1000
    ## 
    ## $counts
    ##  [1] 11 18 17 10  8  0  1  0  0  1
    ## 
    ## $density
    ##  [1] 0.0016666667 0.0027272727 0.0025757576 0.0015151515 0.0012121212
    ##  [6] 0.0000000000 0.0001515152 0.0000000000 0.0000000000 0.0001515152
    ## 
    ## $mids
    ##  [1]  50 150 250 350 450 550 650 750 850 950
    ## 
    ## $xname
    ## [1] "cvd17fem"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
analysis17noNA[,hist(cvd17both)]
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

    ## $breaks
    ##  [1]    0  100  200  300  400  500  600  700  800  900 1000 1100
    ## 
    ## $counts
    ##  [1]  4 23 15 12  8  2  0  1  0  0  1
    ## 
    ## $density
    ##  [1] 0.0006060606 0.0034848485 0.0022727273 0.0018181818 0.0012121212
    ##  [6] 0.0003030303 0.0000000000 0.0001515152 0.0000000000 0.0000000000
    ## [11] 0.0001515152
    ## 
    ## $mids
    ##  [1]   50  150  250  350  450  550  650  750  850  950 1050
    ## 
    ## $xname
    ## [1] "cvd17both"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
analysis17noNA[,hist(LEbirth2019)]
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

    ## $breaks
    ## [1] 60 65 70 75 80 85 90
    ## 
    ## $counts
    ## [1]  4  5  8 21 22  6
    ## 
    ## $density
    ## [1] 0.01212121 0.01515152 0.02424242 0.06363636 0.06666667 0.01818182
    ## 
    ## $mids
    ## [1] 62.5 67.5 72.5 77.5 82.5 87.5
    ## 
    ## $xname
    ## [1] "LEbirth2019"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
analysis17noNA[,hist(phy17edit)]
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

    ## $breaks
    ## [1] 0 1 2 3 4 5 6 7
    ## 
    ## $counts
    ## [1] 17  8 19 15  4  2  1
    ## 
    ## $density
    ## [1] 0.25757576 0.12121212 0.28787879 0.22727273 0.06060606 0.03030303 0.01515152
    ## 
    ## $mids
    ## [1] 0.5 1.5 2.5 3.5 4.5 5.5 6.5
    ## 
    ## $xname
    ## [1] "phy17edit"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
analysis17noNA[,hist(scl17)]
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->

    ## $breaks
    ## [1]  0  2  4  6  8 10 12 14 16
    ## 
    ## $counts
    ## [1]  1  2  4 14 11 16 17  1
    ## 
    ## $density
    ## [1] 0.007575758 0.015151515 0.030303030 0.106060606 0.083333333 0.121212121
    ## [7] 0.128787879 0.007575758
    ## 
    ## $mids
    ## [1]  1  3  5  7  9 11 13 15
    ## 
    ## $xname
    ## [1] "scl17"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
analysis17noNA[,hist(GDP17)]
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->

    ## $breaks
    ##  [1]     0 10000 20000 30000 40000 50000 60000 70000 80000 90000
    ## 
    ## $counts
    ## [1] 37  9  5  2  6  3  2  1  1
    ## 
    ## $density
    ## [1] 5.606061e-05 1.363636e-05 7.575758e-06 3.030303e-06 9.090909e-06
    ## [6] 4.545455e-06 3.030303e-06 1.515152e-06 1.515152e-06
    ## 
    ## $mids
    ## [1]  5000 15000 25000 35000 45000 55000 65000 75000 85000
    ## 
    ## $xname
    ## [1] "GDP17"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
analysis17noNA[,hist(MMR17)]
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->

    ## $breaks
    ##  [1]    0  100  200  300  400  500  600  700  800  900 1000
    ## 
    ## $counts
    ##  [1] 51  6  2  2  3  1  0  0  0  1
    ## 
    ## $density
    ##  [1] 0.0077272727 0.0009090909 0.0003030303 0.0003030303 0.0004545455
    ##  [6] 0.0001515152 0.0000000000 0.0000000000 0.0000000000 0.0001515152
    ## 
    ## $mids
    ##  [1]  50 150 250 350 450 550 650 750 850 950
    ## 
    ## $xname
    ## [1] "MMR17"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
#no continuous variables variables normally distributed

summary(analysis17noNA 
        )
```

    ##  location_name         GSNI_PERIOD    twobias         cvd17fem     
    ##  Length:66          2005–2009:14   Min.   :10.75   Min.   : 57.81  
    ##  Class :character   2010–2014:52   1st Qu.:40.69   1st Qu.:121.59  
    ##  Mode  :character                  Median :64.91   Median :223.47  
    ##                                    Mean   :61.75   Mean   :245.51  
    ##                                    3rd Qu.:85.82   3rd Qu.:317.70  
    ##                                    Max.   :98.07   Max.   :945.26  
    ##    cvd17both        LEbirth2019      phy17edit          scl17       
    ##  Min.   :  77.09   Min.   :63.40   Min.   :0.0008   Min.   : 1.500  
    ##  1st Qu.: 153.34   1st Qu.:75.05   1st Qu.:0.9292   1st Qu.: 7.800  
    ##  Median : 248.34   Median :79.35   Median :2.3765   Median :10.150  
    ##  Mean   : 274.93   Mean   :78.08   Mean   :2.3116   Mean   : 9.626  
    ##  3rd Qu.: 348.65   3rd Qu.:83.05   3rd Qu.:3.4257   3rd Qu.:12.175  
    ##  Max.   :1026.67   Max.   :86.90   Max.   :6.1297   Max.   :14.100  
    ##      GDP17           MMR17       
    ##  Min.   :  735   Min.   :  2.00  
    ##  1st Qu.: 3857   1st Qu.:  9.00  
    ##  Median : 8607   Median : 26.00  
    ##  Mean   :18222   Mean   : 90.42  
    ##  3rd Qu.:27712   3rd Qu.: 82.00  
    ##  Max.   :80450   Max.   :917.00

\#\#\#\#\#summary tables
<https://statsandr.com/blog/descriptive-statistics-in-r/>
<https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html>

``` r
our_summary2 <-
  list("GSNI index 2 or more bias" =
       list("min"       = ~ round(min(twobias),1),
            "max"       = ~ round(max(twobias),1),
            "median (IQR)" = ~ median_iqr(twobias)),
       "Female CVD age adjusted mortality rate (per 100 000 population)" =
       list("min"       = ~ round(min(cvd17fem),1),
            "max"       = ~ round(max(cvd17fem),1),
            "median (IQR)" = ~ median_iqr(cvd17fem)),
       "Population CVD age adjusted mortality rate (per 100 000 population)" =
       list("min"       = ~ round(min(cvd17both),1),
            "max"       = ~ round(max(cvd17both),1),
            "median (IQR)" = ~ median_iqr(cvd17both)),
       "Female life expectancy at birth" =
       list("min"       = ~ round(min(LEbirth2019),1),
            "max"       = ~ round(max(LEbirth2019),1),
            "median (IQR)" = ~ median_iqr(LEbirth2019)),
       "Physicians per 1000 population" =
       list("min"       = ~ round(min(phy17edit),1),
            "max"       = ~ round(max(phy17edit),1),
            "median (IQR)" = ~ median_iqr(phy17edit)),
       "GDP per capita" =
       list("min"       = ~ round(min(GDP17),1),
            "max"       = ~ round(max(GDP17),1),
            "median (IQR)" = ~ median_iqr(GDP17)),
        "Maternal Mortality Ratio" =
       list("min"       = ~ round(min(MMR17),1),
            "max"       = ~ round(max(MMR17),1),
            "median (IQR)" = ~ median_iqr(MMR17)),
       "Mean years of schooling" =
       list("min"       = ~ round(min(scl17),1),
            "max"       = ~ round(max(scl17),1),
            "median (IQR)" = ~ median_iqr(scl17)),
       "Year of Index Collection" =
       list("2005-2009" = ~ n_perc0(GSNI_PERIOD == levels(analysis17noNA$GSNI_PERIOD)[1]),
            "2010-2017"  = ~ n_perc0(GSNI_PERIOD == levels(analysis17noNA$GSNI_PERIOD)[2]))
)
```

``` r
whole17 <- summary_table(analysis17noNA, our_summary2)
whole17
```

    ## 
    ## 
    ## |                                                                        |analysis17noNA (N = 66)        |
    ## |:-----------------------------------------------------------------------|:------------------------------|
    ## |**GSNI index 2 or more bias**                                           |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |10.8                           |
    ## |&nbsp;&nbsp; max                                                        |98.1                           |
    ## |&nbsp;&nbsp; median (IQR)                                               |64.91 (40.69, 85.82)           |
    ## |**Female CVD age adjusted mortality rate (per 100 000 population)**     |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |57.8                           |
    ## |&nbsp;&nbsp; max                                                        |945.3                          |
    ## |&nbsp;&nbsp; median (IQR)                                               |223.47 (121.59, 317.70)        |
    ## |**Population CVD age adjusted mortality rate (per 100 000 population)** |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |77.1                           |
    ## |&nbsp;&nbsp; max                                                        |1026.7                         |
    ## |&nbsp;&nbsp; median (IQR)                                               |248.34 (153.34, 348.65)        |
    ## |**Female life expectancy at birth**                                     |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |63.4                           |
    ## |&nbsp;&nbsp; max                                                        |86.9                           |
    ## |&nbsp;&nbsp; median (IQR)                                               |79.35 (75.05, 83.04)           |
    ## |**Physicians per 1000 population**                                      |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |0                              |
    ## |&nbsp;&nbsp; max                                                        |6.1                            |
    ## |&nbsp;&nbsp; median (IQR)                                               |2.38 (0.93, 3.43)              |
    ## |**GDP per capita**                                                      |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |735                            |
    ## |&nbsp;&nbsp; max                                                        |80450                          |
    ## |&nbsp;&nbsp; median (IQR)                                               |8,606.76 (3,856.86, 27,712.30) |
    ## |**Maternal Mortality Ratio**                                            |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |2                              |
    ## |&nbsp;&nbsp; max                                                        |917                            |
    ## |&nbsp;&nbsp; median (IQR)                                               |26.00 (9.00, 82.00)            |
    ## |**Mean years of schooling**                                             |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |1.5                            |
    ## |&nbsp;&nbsp; max                                                        |14.1                           |
    ## |&nbsp;&nbsp; median (IQR)                                               |10.15 (7.80, 12.17)            |
    ## |**Year of Index Collection**                                            |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; 2005-2009                                                  |14 (21)                        |
    ## |&nbsp;&nbsp; 2010-2017                                                  |52 (79)                        |

\#OUTCOME 1 - CVD mortality female

\#\#Scatter plots

\#\#\#CVD mort (female) \~ GSNI

``` r
ggplot(analysis17noNA, aes(x = twobias, y = cvd17fem)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "percentage with 2 GSNI bias (%)",
                y = "female age standardised CVD mortaltiy rate (per 100k)",
                title = "Scatter plot of GSNI (2 bias) and female CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(twobias,cvd17fem,method = "spearman")]
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  twobias and cvd17fem
    ## S = 15538, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.6756497

\#\#\#CVD mort (female) \~ physicians per 1000

``` r
ggplot(analysis17noNA, aes(x = phy17edit, y = cvd17fem)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "physicians per 1000 population",
                y = "female age standardised CVD mortaltiy rate (per 100k)",
                title = "Scatter plot of phycisians per 1000 and female CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(cvd17fem,phy17edit,method = "spearman")]
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  cvd17fem and phy17edit
    ## S = 61512, p-value = 0.02113
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.2840413

\#\#\#CVD mort (female) \~ mean years of schooling

``` r
ggplot(analysis17noNA, aes(x = scl17, y = cvd17fem)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "mean years of schooling",
                y = "female age standardised CVD mortaltiy rate (per 100k)",
                title = "Mean years of schooling and female CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(scl17,cvd17fem,method = "spearman")]
```

    ## Warning in cor.test.default(scl17, cvd17fem, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  scl17 and cvd17fem
    ## S = 65877, p-value = 0.00191
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.3751618

\#\#\#CVD mort (female) \~ GDP per capita

``` r
ggplot(analysis17noNA, aes(x =GDP17, y = cvd17fem)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "GDP per capita",
                y = "female age standardised CVD mortaltiy rate (per 100k)",
                title = "GDP per capita and female CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(GDP17,cvd17fem,method = "spearman")]
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  GDP17 and cvd17fem
    ## S = 80540, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.6812441

\#\#\#CVD mort (female) \~ MMR

``` r
ggplot(analysis17noNA, aes(x = MMR17, y = cvd17fem)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "Maternal Mortality Ratio",
                y = "female age standardised CVD mortaltiy rate (per 100k)",
                title = "MMR and female CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(MMR17,cvd17fem,method = "spearman")]
```

    ## Warning in cor.test.default(MMR17, cvd17fem, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  MMR17 and cvd17fem
    ## S = 26378, p-value = 0.0001538
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.4493615

\#\#\#CVD mort (female) \~ year of index

``` r
outlier17.1<- analysis17noNA[cvd17fem>550,]
ggplot(analysis17noNA, aes(x = GSNI_PERIOD, y = cvd17fem)) +
  geom_boxplot(color = "violetred3", notch= TRUE)+
  
         labs(x = "GSNI period",
                y = "female age standardised CVD mortaltiy rate (per 100k) (2017)",
                title = "GSNI period of collection and female CVD mortality")+
ggrepel::geom_text_repel(data = outlier17.1, aes(label = location_name                                        ))  
```

    ## notch went outside hinges. Try setting notch=FALSE.

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

\#\#Univariable regression

\#\#\#CVD mort (female) \~ GSNI

``` r
m17.1 <- lm(cvd17fem ~ twobias, data = analysis17noNA)
summary(m17.1)
```

    ## 
    ## Call:
    ## lm(formula = cvd17fem ~ twobias, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -230.18  -65.84  -21.58   45.09  607.69 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  26.7318    40.3328   0.663     0.51    
    ## twobias       3.5431     0.6007   5.898 1.51e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 128.7 on 64 degrees of freedom
    ## Multiple R-squared:  0.3522, Adjusted R-squared:  0.342 
    ## F-statistic: 34.79 on 1 and 64 DF,  p-value: 1.511e-07

``` r
plot(m17.1)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

\#\#\#CVD mort (female) \~ physicians per 1000

``` r
m17.2 <- lm(cvd17fem ~ phy17edit, data = analysis17noNA)
summary(m17.2)
```

    ## 
    ## Call:
    ## lm(formula = cvd17fem ~ phy17edit, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -185.90 -110.40  -38.54   66.05  700.88 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   287.29      36.17   7.942 4.07e-11 ***
    ## phy17edit     -18.07      13.21  -1.368    0.176    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 157.6 on 64 degrees of freedom
    ## Multiple R-squared:  0.02842,    Adjusted R-squared:  0.01324 
    ## F-statistic: 1.872 on 1 and 64 DF,  p-value: 0.176

``` r
plot(m17.2)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

\#\#\#CVD mort (female) \~ mean years of schooling

``` r
m17.3 <- lm(cvd17fem ~ scl17, data = analysis17noNA)
summary(m17.3)
```

    ## 
    ## Call:
    ## lm(formula = cvd17fem ~ scl17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -171.34 -111.86  -37.88   56.46  718.87 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  343.702     66.725   5.151 2.68e-06 ***
    ## scl17        -10.201      6.635  -1.537    0.129    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 157 on 64 degrees of freedom
    ## Multiple R-squared:  0.03562,    Adjusted R-squared:  0.02055 
    ## F-statistic: 2.364 on 1 and 64 DF,  p-value: 0.1291

``` r
plot(m17.3)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

\#\#\#CVD mort (female) \~ GDP per capita

``` r
m17.4 <- lm(cvd17fem ~ GDP17, data = analysis17noNA)
summary(m17.4)
```

    ## 
    ## Call:
    ## lm(formula = cvd17fem ~ GDP17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -210.31  -72.14  -25.15   42.55  638.07 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.141e+02  2.290e+01  13.717  < 2e-16 ***
    ## GDP17       -3.762e-03  8.331e-04  -4.516 2.77e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 139.2 on 64 degrees of freedom
    ## Multiple R-squared:  0.2417, Adjusted R-squared:  0.2298 
    ## F-statistic: 20.39 on 1 and 64 DF,  p-value: 2.77e-05

``` r
plot(m17.4)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

\#\#\#CVD mort (female) \~ MMR

``` r
m17.5 <- lm(cvd17fem ~ MMR17, data = analysis17noNA)
summary(m17.5)
```

    ## 
    ## Call:
    ## lm(formula = cvd17fem ~ MMR17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -174.07 -114.90  -26.83   58.49  709.55 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 231.0832    22.2993  10.363 2.54e-15 ***
    ## MMR17         0.1596     0.1212   1.317    0.193    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 157.8 on 64 degrees of freedom
    ## Multiple R-squared:  0.02638,    Adjusted R-squared:  0.01116 
    ## F-statistic: 1.734 on 1 and 64 DF,  p-value: 0.1926

``` r
plot(m17.5)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->

\#\#\#CVD mort (female) \~ year of index

``` r
m17.6 <- lm(cvd17fem ~ GSNI_PERIOD, data = analysis17noNA)
summary(m17.6)
```

    ## 
    ## Call:
    ## lm(formula = cvd17fem ~ GSNI_PERIOD, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -192.92 -126.96  -25.42   78.26  694.53 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            226.14      42.65   5.303 1.51e-06 ***
    ## GSNI_PERIOD2010–2014    24.59      48.05   0.512    0.611    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 159.6 on 64 degrees of freedom
    ## Multiple R-squared:  0.004076,   Adjusted R-squared:  -0.01149 
    ## F-statistic: 0.2619 on 1 and 64 DF,  p-value: 0.6105

``` r
plot(m17.6)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->

\#\#Multivariable regression

``` r
m17.7 <- lm(cvd17fem ~ twobias + phy17edit + scl17+ MMR17+ GDP17, data = analysis17noNA)
summary(m17.7)
```

    ## 
    ## Call:
    ## lm(formula = cvd17fem ~ twobias + phy17edit + scl17 + MMR17 + 
    ##     GDP17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -171.85  -70.27  -21.48   35.03  534.97 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.391e+02  1.117e+02  -1.245   0.2180    
    ## twobias      3.675e+00  8.550e-01   4.298 6.43e-05 ***
    ## phy17edit    6.341e+00  1.689e+01   0.375   0.7087    
    ## scl17        1.866e+01  9.630e+00   1.938   0.0573 .  
    ## MMR17        4.994e-02  1.285e-01   0.389   0.6989    
    ## GDP17       -2.257e-03  1.091e-03  -2.069   0.0429 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 123.2 on 60 degrees of freedom
    ## Multiple R-squared:  0.4433, Adjusted R-squared:  0.3969 
    ## F-statistic: 9.555 on 5 and 60 DF,  p-value: 9.627e-07

``` r
plot(m17.7)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-19-4.png)<!-- -->

``` r
#added in GSNI PERIOD
m17.8 <- lm(cvd17fem ~ twobias + phy17edit + scl17+ MMR17+ GDP17+GSNI_PERIOD, data = analysis17noNA)
summary(m17.8)
```

    ## 
    ## Call:
    ## lm(formula = cvd17fem ~ twobias + phy17edit + scl17 + MMR17 + 
    ##     GDP17 + GSNI_PERIOD, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -172.08  -71.63  -17.47   41.30  526.98 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.316e+02  1.083e+02  -1.215   0.2293    
    ## twobias               4.317e+00  8.778e-01   4.919 7.32e-06 ***
    ## phy17edit             8.503e+00  1.639e+01   0.519   0.6059    
    ## scl17                 2.116e+01  9.399e+00   2.252   0.0281 *  
    ## MMR17                 1.348e-02  1.256e-01   0.107   0.9149    
    ## GDP17                -2.439e-03  1.060e-03  -2.300   0.0250 *  
    ## GSNI_PERIOD2010–2014 -8.844e+01  3.991e+01  -2.216   0.0306 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 119.4 on 59 degrees of freedom
    ## Multiple R-squared:  0.4861, Adjusted R-squared:  0.4338 
    ## F-statistic:   9.3 on 6 and 59 DF,  p-value: 3.603e-07

``` r
plot(m17.8)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-19-5.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-19-6.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-19-7.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-19-8.png)<!-- -->

``` r
#r2 higher in m8

car::outlierTest(m17.8)
```

    ##    rstudent unadjusted p-value Bonferroni p
    ## 64 5.699147         4.2468e-07   2.8029e-05

``` r
m17.8tidy <- tidy(m17.8)
kable(m17.8tidy, digits = 2)
```

| term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           | \-131.55 |    108.30 |    \-1.21 |    0.23 |
| twobias               |     4.32 |      0.88 |      4.92 |    0.00 |
| phy17edit             |     8.50 |     16.39 |      0.52 |    0.61 |
| scl17                 |    21.16 |      9.40 |      2.25 |    0.03 |
| MMR17                 |     0.01 |      0.13 |      0.11 |    0.91 |
| GDP17                 |     0.00 |      0.00 |    \-2.30 |    0.02 |
| GSNI\_PERIOD2010–2014 |  \-88.44 |     39.91 |    \-2.22 |    0.03 |

assess for colinearity

``` r
car::vif(m17.7)
```

    ##   twobias phy17edit     scl17     MMR17     GDP17 
    ##  2.210213  2.675700  3.421685  1.843317  2.189554

``` r
car::vif(m17.8)
```

    ##     twobias   phy17edit       scl17       MMR17       GDP17 GSNI_PERIOD 
    ##    2.481296    2.685211    3.471669    1.875509    2.202765    1.232549

``` r
#all <4
```

assess for normality of residuals

``` r
shapiro.test(m17.7$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m17.7$residuals
    ## W = 0.87897, p-value = 1.073e-05

``` r
shapiro.test(m17.8$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m17.8$residuals
    ## W = 0.86645, p-value = 4.052e-06

``` r
#evidence of non-normality in the residuals
```

``` r
#check for independence
plot(m17.7$residuals, type = "o")
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
plot(m17.8$residuals, type = "o")
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

\#Assumptions Linearity - residual vs fitted appears within limits
Normality of residuals - QQplots show some deviance, shapiro-wilk are
significant suggesting lack of normality Independence - looks fine
multicolinearity - all within limits homoscedacity - scale-location
plots look ok

Comparing AIC (using corrected as number of covariates is 10% of data
<https://stats.stackexchange.com/questions/86768/aicc-for-small-sample-sizes>)

``` r
AICcmodavg::AICc(m17.7)
```

    ## [1] 832.3785

``` r
AICcmodavg::AICc(m17.8) #m17.8 has lower AICc -> preferable
```

    ## [1] 829.697

\#OUTCOME 2 - CVD mortality

\#\#Scatter plots

\#\#\#CVD mort \~ GSNI

``` r
ggplot(analysis17noNA, aes(x = twobias, y = cvd17both)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "percentage with 2 GSNI bias (%)",
                y = "age standardised CVD mortaltiy rate (per 100k)",
                title = "Scatter plot of GSNI (2 bias) and CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(twobias,cvd17both,method = "spearman")]
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  twobias and cvd17both
    ## S = 17686, p-value = 3.127e-08
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##      rho 
    ## 0.630811

\#\#\#CVD mort \~ physicians per 1000

``` r
ggplot(analysis17noNA, aes(x = phy17edit, y = cvd17both)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "physicians per 1000 population",
                y = "age standardised CVD mortaltiy rate (per 100k)",
                title = "Scatter plot of phycisians per 1000 and CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(cvd17both,phy17edit,method = "spearman")]
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  cvd17both and phy17edit
    ## S = 57402, p-value = 0.1105
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.1982465

\#\#\#CVD mort \~ mean years of schooling

``` r
ggplot(analysis17noNA, aes(x = scl17, y = cvd17both)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "mean years of schooling",
                y = "age standardised CVD mortaltiy rate (per 100k)",
                title = "Mean years of schooling and CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(scl17,cvd17both,method = "spearman")]
```

    ## Warning in cor.test.default(scl17, cvd17both, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  scl17 and cvd17both
    ## S = 63482, p-value = 0.007724
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.3251556

\#\#\#CVD mort \~ GDP per capita

``` r
ggplot(analysis17noNA, aes(x =GDP17, y = cvd17both)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "GDP per capita",
                y = "age standardised CVD mortaltiy rate (per 100k)",
                title = "GDP per capita and CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(GDP17,cvd17both,method = "spearman")]
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  GDP17 and cvd17both
    ## S = 79204, p-value = 4.913e-09
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.6533556

\#\#\#CVD mort \~ MMR

``` r
ggplot(analysis17noNA, aes(x = MMR17, y = cvd17both)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "Maternal Mortality Ratio",
                y = "age standardised CVD mortaltiy rate (per 100k)",
                title = "MMR and CVD mortality")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(MMR17,cvd17both,method = "spearman")]
```

    ## Warning in cor.test.default(MMR17, cvd17both, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  MMR17 and cvd17both
    ## S = 29460, p-value = 0.001411
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.3850387

\#\#\#CVD mort \~ year of index

``` r
outlier17.2 <- analysis17noNA[cvd17both>650,]
ggplot(analysis17noNA, aes(x = GSNI_PERIOD, y = cvd17both)) +
  geom_boxplot(color = "violetred3", notch= TRUE)+
  
         labs(x = "GSNI period",
                y = "age standardised CVD mortaltiy rate (per 100k) (2017)",
                title = "GSNI period of collection and CVD mortality")+
ggrepel::geom_text_repel(data = outlier17.2, aes(label = location_name                                        ))  
```

    ## notch went outside hinges. Try setting notch=FALSE.

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

\#\#Univariable regression

\#\#\#CVD mort \~ GSNI

``` r
m17.9 <- lm(cvd17both ~ twobias, data = analysis17noNA)
summary(m17.9)
```

    ## 
    ## Call:
    ## lm(formula = cvd17both ~ twobias, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -218.58  -78.56  -21.59   39.15  662.10 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  61.8783    44.6302   1.386     0.17    
    ## twobias       3.4502     0.6647   5.191 2.31e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 142.4 on 64 degrees of freedom
    ## Multiple R-squared:  0.2963, Adjusted R-squared:  0.2853 
    ## F-statistic: 26.94 on 1 and 64 DF,  p-value: 2.311e-06

``` r
plot(m17.9)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-30-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-30-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-30-4.png)<!-- -->
\#\#\#CVD mort \~ physicians per 1000

``` r
m17.10 <- lm(cvd17both ~ phy17edit, data = analysis17noNA)
summary(m17.10)
```

    ## 
    ## Call:
    ## lm(formula = cvd17both ~ phy17edit, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -197.26 -118.51  -32.98   73.59  752.11 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  288.339     38.910   7.410 3.53e-10 ***
    ## phy17edit     -5.802     14.207  -0.408    0.684    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 169.5 on 64 degrees of freedom
    ## Multiple R-squared:  0.0026, Adjusted R-squared:  -0.01298 
    ## F-statistic: 0.1668 on 1 and 64 DF,  p-value: 0.6843

``` r
plot(m17.10)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-31-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-31-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-31-4.png)<!-- -->
\#\#\#CVD mort \~ mean years of schooling

``` r
m17.11 <- lm(cvd17both ~ scl17, data = analysis17noNA)
summary(m17.11)
```

    ## 
    ## Call:
    ## lm(formula = cvd17both ~ scl17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -187.87 -117.91  -39.59   59.61  764.06 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  338.172     71.663   4.719 1.33e-05 ***
    ## scl17         -6.570      7.126  -0.922     0.36    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 168.6 on 64 degrees of freedom
    ## Multiple R-squared:  0.01311,    Adjusted R-squared:  -0.00231 
    ## F-statistic: 0.8502 on 1 and 64 DF,  p-value: 0.36

``` r
plot(m17.11)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-32-4.png)<!-- -->
\#\#\#CVD mort \~ GDP per capita

``` r
m17.12 <- lm(cvd17both ~ GDP17, data = analysis17noNA)
summary(m17.12)
```

    ## 
    ## Call:
    ## lm(formula = cvd17both ~ GDP17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -232.77  -86.84  -26.89   54.37  683.81 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.504e+02  2.401e+01  14.594  < 2e-16 ***
    ## GDP17       -4.143e-03  8.737e-04  -4.743 1.22e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 146 on 64 degrees of freedom
    ## Multiple R-squared:   0.26,  Adjusted R-squared:  0.2485 
    ## F-statistic: 22.49 on 1 and 64 DF,  p-value: 1.222e-05

``` r
plot(m17.12)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-33-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-33-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-33-4.png)<!-- -->
\#\#\#CVD mort \~ MMR

``` r
m17.13 <- lm(cvd17both ~ MMR17, data = analysis17noNA)
summary(m17.13)
```

    ## 
    ## Call:
    ## lm(formula = cvd17both ~ MMR17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -190.63 -114.81  -32.25   75.84  756.92 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 267.30105   23.91499  11.177   <2e-16 ***
    ## MMR17         0.08432    0.12998   0.649    0.519    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 169.2 on 64 degrees of freedom
    ## Multiple R-squared:  0.006533,   Adjusted R-squared:  -0.00899 
    ## F-statistic: 0.4209 on 1 and 64 DF,  p-value: 0.5188

``` r
plot(m17.13)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-34-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-34-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-34-4.png)<!-- -->
\#\#\#CVD mort \~ year of index

``` r
m17.14 <- lm(cvd17both ~ GSNI_PERIOD, data = analysis17noNA)
summary(m17.14)
```

    ## 
    ## Call:
    ## lm(formula = cvd17both ~ GSNI_PERIOD, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -203.56 -125.89  -29.80   80.85  746.02 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            253.69      45.27   5.604 4.76e-07 ***
    ## GSNI_PERIOD2010–2014    26.96      51.00   0.529    0.599    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 169.4 on 64 degrees of freedom
    ## Multiple R-squared:  0.004346,   Adjusted R-squared:  -0.01121 
    ## F-statistic: 0.2794 on 1 and 64 DF,  p-value: 0.5989

``` r
plot(m17.14)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-35-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-35-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-35-4.png)<!-- -->
\#\#Multivariable regression

``` r
m17.15<- lm(cvd17both ~ twobias + phy17edit + scl17+ MMR17+ GDP17, data = analysis17noNA)
summary(m17.15)
```

    ## 
    ## Call:
    ## lm(formula = cvd17both ~ twobias + phy17edit + scl17 + MMR17 + 
    ##     GDP17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -213.80  -62.48  -25.68   48.04  564.24 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.357e+02  1.157e+02  -1.173 0.245278    
    ## twobias      3.661e+00  8.852e-01   4.135 0.000112 ***
    ## phy17edit    1.864e+01  1.749e+01   1.066 0.290736    
    ## scl17        2.071e+01  9.971e+00   2.078 0.042039 *  
    ## MMR17        1.720e-02  1.330e-01   0.129 0.897581    
    ## GDP17       -3.261e-03  1.129e-03  -2.887 0.005397 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 127.6 on 60 degrees of freedom
    ## Multiple R-squared:  0.4706, Adjusted R-squared:  0.4265 
    ## F-statistic: 10.67 on 5 and 60 DF,  p-value: 2.313e-07

``` r
plot(m17.15)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-36-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-36-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-36-4.png)<!-- -->

``` r
#added in GSNI PERIOD
m17.16 <- lm(cvd17both ~ twobias + phy17edit + scl17+ MMR17+ GDP17+GSNI_PERIOD, data = analysis17noNA)
summary(m17.16)
```

    ## 
    ## Call:
    ## lm(formula = cvd17both ~ twobias + phy17edit + scl17 + MMR17 + 
    ##     GDP17 + GSNI_PERIOD, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -224.34  -73.03  -20.14   39.11  555.07 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.271e+02  1.110e+02  -1.144  0.25713    
    ## twobias               4.399e+00  9.000e-01   4.888 8.18e-06 ***
    ## phy17edit             2.112e+01  1.681e+01   1.257  0.21386    
    ## scl17                 2.358e+01  9.637e+00   2.447  0.01739 *  
    ## MMR17                -2.468e-02  1.288e-01  -0.192  0.84866    
    ## GDP17                -3.470e-03  1.087e-03  -3.192  0.00227 ** 
    ## GSNI_PERIOD2010–2014 -1.016e+02  4.092e+01  -2.482  0.01592 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 122.4 on 59 degrees of freedom
    ## Multiple R-squared:  0.5207, Adjusted R-squared:  0.4719 
    ## F-statistic: 10.68 on 6 and 59 DF,  p-value: 5.241e-08

``` r
plot(m17.16)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-36-5.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-36-6.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-36-7.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-36-8.png)<!-- -->

``` r
car::outlierTest(m17.16)
```

    ##    rstudent unadjusted p-value Bonferroni p
    ## 64 5.947448         1.6637e-07    1.098e-05

``` r
m17.16tidy <- tidy(m17.16)
kable(m17.16tidy, digits = 2)
```

| term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           | \-127.06 |    111.04 |    \-1.14 |    0.26 |
| twobias               |     4.40 |      0.90 |      4.89 |    0.00 |
| phy17edit             |    21.12 |     16.81 |      1.26 |    0.21 |
| scl17                 |    23.58 |      9.64 |      2.45 |    0.02 |
| MMR17                 |   \-0.02 |      0.13 |    \-0.19 |    0.85 |
| GDP17                 |     0.00 |      0.00 |    \-3.19 |    0.00 |
| GSNI\_PERIOD2010–2014 | \-101.57 |     40.92 |    \-2.48 |    0.02 |

assess for colinearity

``` r
car::vif(m17.15)
```

    ##   twobias phy17edit     scl17     MMR17     GDP17 
    ##  2.210213  2.675700  3.421685  1.843317  2.189554

``` r
car::vif(m17.16)
```

    ##     twobias   phy17edit       scl17       MMR17       GDP17 GSNI_PERIOD 
    ##    2.481296    2.685211    3.471669    1.875509    2.202765    1.232549

``` r
#all <4
```

assess for normality of residuals

``` r
shapiro.test(m17.15$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m17.15$residuals
    ## W = 0.88658, p-value = 1.992e-05

``` r
shapiro.test(m17.16$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m17.16$residuals
    ## W = 0.87141, p-value = 5.923e-06

``` r
#evidence of non-normality in the residuals
```

``` r
#check for independence
plot(m17.15$residuals, type = "o")
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
plot(m17.16$residuals, type = "o")
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-39-2.png)<!-- -->

Comparing AIC (using corrected as number of covariates is 10% of data
<https://stats.stackexchange.com/questions/86768/aicc-for-small-sample-sizes>)

``` r
AICcmodavg::AICc(m17.15)
```

    ## [1] 836.9593

``` r
AICcmodavg::AICc(m17.16) #m16 has lower AICc therefore better
```

    ## [1] 832.9982

\#Assumptions Linearity - residual vs fitted appears within limits
Normality of residuals - QQplots show some deviance, shapiro-wilk are
significant suggesting lack of normality Independence - looks fine
multicolinearity - all within limits homoscedacity - scale-location plot
looks ok

\#OUTCOME 3 Life expectancy for women

\#\#Scatter plots

\#\#\#Life Expectancy \~ GSNI

``` r
ggplot(analysis17noNA, aes(x = twobias, y = LEbirth2019)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "percentage with 2 GSNI bias (%)",
                y = "Female life expectancy",
                title = "Scatter plot of GSNI (2 bias) and female life expectancy")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(twobias,LEbirth2019,method = "spearman")]
```

    ## Warning in cor.test.default(twobias, LEbirth2019, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  twobias and LEbirth2019
    ## S = 83845, p-value = 4.14e-13
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.7502375

\#\#\#Life Expectancy \~ physicians per 1000

``` r
ggplot(analysis17noNA, aes(x = phy17edit, y = LEbirth2019)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "physicians per 1000",
                y = "Female life expectancy",
                title = "Scatter plot of physicians per 1000 and female life expectancy")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(phy17edit,LEbirth2019,method = "spearman")]
```

    ## Warning in cor.test.default(phy17edit, LEbirth2019, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  phy17edit and LEbirth2019
    ## S = 18426, p-value = 3.838e-08
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.6153605

\#\#\#Life Expectancy \~ mean years of schooling

``` r
ggplot(analysis17noNA, aes(x = scl17, y = LEbirth2019)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "mean years of schooling",
                y = "Female life expectancy",
                title = "Scatter plot of mean years of schooling and female life expectancy")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(scl17,LEbirth2019,method = "spearman")]
```

    ## Warning in cor.test.default(scl17, LEbirth2019, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  scl17 and LEbirth2019
    ## S = 15137, p-value = 2.438e-10
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.6840209

\#\#\#Life Expectancy \~ GDP per capita

``` r
ggplot(analysis17noNA, aes(x = GDP17, y = LEbirth2019)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "GDP per capita",
                y = "Female life expectancy",
                title = "GDP per capita and female life expectancy")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(GDP17,LEbirth2019,method = "spearman")]
```

    ## Warning in cor.test.default(GDP17, LEbirth2019, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  GDP17 and LEbirth2019
    ## S = 6535.8, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.8635676

\#\#\#Life Expectancy \~ MMR

``` r
ggplot(analysis17noNA, aes(x = MMR17, y = LEbirth2019)) +
  geom_point(color = "violetred3")+
  geom_smooth(method = "lm")+
         labs(x = "MMR",
                y = "Female life expectancy",
                title = "Scatter plot of MMR and female life expectancy")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
analysis17noNA[,cor.test(MMR17,LEbirth2019,method = "spearman")]
```

    ## Warning in cor.test.default(MMR17, LEbirth2019, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  MMR17 and LEbirth2019
    ## S = 87285, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.8220538

\#\#\#Life Expectancy \~ year of index

``` r
outlier17.3<-analysis17noNA[LEbirth2019<67]
ggplot(analysis17noNA, aes(x = GSNI_PERIOD, y = LEbirth2019)) +
  geom_boxplot(color = "violetred3", notch= TRUE)+
  
         labs(x = "GSNI period",
                y = "Life expectancy at birth (2019)",
                title = "GSNI period of collection and life expectancy")+
ggrepel::geom_text_repel(data = outlier17.3, aes(label = location_name                                        ))  
```

    ## notch went outside hinges. Try setting notch=FALSE.

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

\#\#Univariable regression

\#\#\#Life Expectancy \~ GSNI

``` r
m17.17 <- lm(LEbirth2019 ~ twobias, data = analysis17noNA)
summary(m17.17)
```

    ## 
    ## Call:
    ## lm(formula = LEbirth2019 ~ twobias, data = analysis17noNA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.8095  -1.8778   0.6291   2.8409  10.5522 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 87.89115    1.44905   60.65  < 2e-16 ***
    ## twobias     -0.15884    0.02158   -7.36 4.33e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.624 on 64 degrees of freedom
    ## Multiple R-squared:  0.4584, Adjusted R-squared:   0.45 
    ## F-statistic: 54.17 on 1 and 64 DF,  p-value: 4.329e-10

``` r
plot(m17.17)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-47-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-47-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-47-4.png)<!-- -->

\#\#\#Life Expectancy \~ physicians per 1000

``` r
m17.18 <- lm(LEbirth2019 ~ phy17edit, data = analysis17noNA)
summary(m17.18)
```

    ## 
    ## Call:
    ## lm(formula = LEbirth2019 ~ phy17edit, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.058  -2.980   1.137   3.431   8.535 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  71.5592     1.0704  66.855  < 2e-16 ***
    ## phy17edit     2.8222     0.3908   7.221 7.59e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.664 on 64 degrees of freedom
    ## Multiple R-squared:  0.449,  Adjusted R-squared:  0.4404 
    ## F-statistic: 52.15 on 1 and 64 DF,  p-value: 7.594e-10

``` r
plot(m17.18)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-48-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-48-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-48-4.png)<!-- -->

\#\#\#Life Expectancy \~ mean years of schooling

``` r
m17.19 <- lm(LEbirth2019 ~ scl17, data = analysis17noNA)
summary(m17.19)
```

    ## 
    ## Call:
    ## lm(formula = LEbirth2019 ~ scl17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.2837  -2.5249   0.6663   3.0264   9.4046 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  63.2348     1.8351  34.458  < 2e-16 ***
    ## scl17         1.5426     0.1825   8.454 5.13e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.319 on 64 degrees of freedom
    ## Multiple R-squared:  0.5275, Adjusted R-squared:  0.5202 
    ## F-statistic: 71.46 on 1 and 64 DF,  p-value: 5.127e-12

``` r
plot(m17.19)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-49-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-49-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-49-4.png)<!-- -->

\#\#\#Life Expectancy \~ GDP per capita

``` r
m17.20 <- lm(LEbirth2019 ~ GDP17, data = analysis17noNA)
summary(m17.20)
```

    ## 
    ## Call:
    ## lm(formula = LEbirth2019 ~ GDP17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.403  -1.962   1.660   3.570   6.051 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 7.465e+01  8.047e-01  92.761  < 2e-16 ***
    ## GDP17       1.886e-04  2.928e-05   6.441 1.76e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.894 on 64 degrees of freedom
    ## Multiple R-squared:  0.3933, Adjusted R-squared:  0.3838 
    ## F-statistic: 41.48 on 1 and 64 DF,  p-value: 1.763e-08

``` r
plot(m17.20)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-50-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-50-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-50-4.png)<!-- -->
\#\#\#Life Expectancy \~ MMR

``` r
m17.21 <- lm(LEbirth2019 ~ MMR17, data = analysis17noNA)
summary(m17.21)
```

    ## 
    ## Call:
    ## lm(formula = LEbirth2019 ~ MMR17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.8658 -2.2139  0.0565  3.0883 11.3145 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 80.850484   0.541401   149.3  < 2e-16 ***
    ## MMR17       -0.030605   0.002943   -10.4 2.19e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.831 on 64 degrees of freedom
    ## Multiple R-squared:  0.6283, Adjusted R-squared:  0.6225 
    ## F-statistic: 108.2 on 1 and 64 DF,  p-value: 2.192e-15

``` r
plot(m17.21)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-51-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-51-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-51-4.png)<!-- -->
\#\#\#Life Expectancy \~ year of index

``` r
m17.22 <- lm(LEbirth2019 ~ GSNI_PERIOD, data = analysis17noNA)
summary(m17.22)
```

    ## 
    ## Call:
    ## lm(formula = LEbirth2019 ~ GSNI_PERIOD, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.744  -3.294   1.171   4.831   8.556 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            77.113      1.674  46.076   <2e-16 ***
    ## GSNI_PERIOD2010–2014    1.231      1.885   0.653    0.516    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.262 on 64 degrees of freedom
    ## Multiple R-squared:  0.00662,    Adjusted R-squared:  -0.008901 
    ## F-statistic: 0.4265 on 1 and 64 DF,  p-value: 0.516

``` r
plot(m17.22)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-52-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-52-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-52-4.png)<!-- -->

\#\#Multivariable regression

``` r
m17.23 <- lm(LEbirth2019 ~ twobias + phy17edit + scl17+ MMR17+ GDP17, data = analysis17noNA)
summary(m17.23)
```

    ## 
    ## Call:
    ## lm(formula = LEbirth2019 ~ twobias + phy17edit + scl17 + MMR17 + 
    ##     GDP17, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.9022 -1.6350 -0.2641  1.8379  6.6195 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.049e+01  2.602e+00  30.937  < 2e-16 ***
    ## twobias     -5.794e-02  1.991e-02  -2.910  0.00507 ** 
    ## phy17edit    4.967e-01  3.933e-01   1.263  0.21153    
    ## scl17        8.233e-02  2.243e-01   0.367  0.71483    
    ## MMR17       -2.001e-02  2.993e-03  -6.687 8.54e-09 ***
    ## GDP17        5.722e-05  2.540e-05   2.252  0.02796 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.869 on 60 degrees of freedom
    ## Multiple R-squared:  0.8045, Adjusted R-squared:  0.7882 
    ## F-statistic: 49.37 on 5 and 60 DF,  p-value: < 2.2e-16

``` r
plot(m17.23)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-2.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-3.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-4.png)<!-- -->

``` r
#added in GSNI PERIOD
m17.24 <- lm(LEbirth2019 ~ twobias + phy17edit + scl17+ MMR17+ GDP17+GSNI_PERIOD, data = analysis17noNA)
summary(m17.24)
```

    ## 
    ## Call:
    ## lm(formula = LEbirth2019 ~ twobias + phy17edit + scl17 + MMR17 + 
    ##     GDP17 + GSNI_PERIOD, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.9473 -1.6185 -0.0382  1.7533  5.5960 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           8.028e+01  2.480e+00  32.374  < 2e-16 ***
    ## twobias              -7.565e-02  2.010e-02  -3.764 0.000387 ***
    ## phy17edit             4.371e-01  3.754e-01   1.165 0.248884    
    ## scl17                 1.349e-02  2.152e-01   0.063 0.950229    
    ## MMR17                -1.901e-02  2.876e-03  -6.609 1.24e-08 ***
    ## GDP17                 6.223e-05  2.427e-05   2.564 0.012924 *  
    ## GSNI_PERIOD2010–2014  2.436e+00  9.138e-01   2.666 0.009893 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.734 on 59 degrees of freedom
    ## Multiple R-squared:  0.8255, Adjusted R-squared:  0.8078 
    ## F-statistic: 46.52 on 6 and 59 DF,  p-value: < 2.2e-16

``` r
plot(m17.24)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-5.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-6.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-7.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-8.png)<!-- -->

``` r
#take log of GDP and phy based on scatter plots
m17.242 <- lm(LEbirth2019 ~ twobias + log(phy17edit) + scl17+ MMR17+ log(GDP17)+GSNI_PERIOD, data = analysis17noNA)
summary(m17.242)
```

    ## 
    ## Call:
    ## lm(formula = LEbirth2019 ~ twobias + log(phy17edit) + scl17 + 
    ##     MMR17 + log(GDP17) + GSNI_PERIOD, data = analysis17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.4682 -1.3153  0.1846  1.3831  3.3983 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          60.747221   4.231255  14.357  < 2e-16 ***
    ## twobias              -0.038593   0.017982  -2.146  0.03598 *  
    ## log(phy17edit)        0.923045   0.275065   3.356  0.00139 ** 
    ## scl17                -0.285641   0.174914  -1.633  0.10779    
    ## MMR17                -0.013822   0.002558  -5.404 1.23e-06 ***
    ## log(GDP17)            2.459351   0.432121   5.691 4.18e-07 ***
    ## GSNI_PERIOD2010–2014  1.315833   0.780069   1.687  0.09692 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.284 on 59 degrees of freedom
    ## Multiple R-squared:  0.8782, Adjusted R-squared:  0.8658 
    ## F-statistic: 70.88 on 6 and 59 DF,  p-value: < 2.2e-16

``` r
plot(m17.242)
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-9.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-10.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-11.png)<!-- -->![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-53-12.png)<!-- -->

``` r
car::outlierTest(m17.24)
```

    ## No Studentized residuals with Bonferroni p < 0.05
    ## Largest |rstudent|:
    ##    rstudent unadjusted p-value Bonferroni p
    ## 40 3.443315          0.0010732     0.070833

assess for colinearity

``` r
car::vif(m17.23)
```

    ##   twobias phy17edit     scl17     MMR17     GDP17 
    ##  2.210213  2.675700  3.421685  1.843317  2.189554

``` r
car::vif(m17.24)
```

    ##     twobias   phy17edit       scl17       MMR17       GDP17 GSNI_PERIOD 
    ##    2.481296    2.685211    3.471669    1.875509    2.202765    1.232549

``` r
#all <4
```

assess for normality of residuals

``` r
shapiro.test(m17.23$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m17.23$residuals
    ## W = 0.97989, p-value = 0.3597

``` r
shapiro.test(m17.24$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m17.24$residuals
    ## W = 0.98605, p-value = 0.6678

``` r
#no evidence of non-normality in the residuals
```

``` r
#check for independence
plot(m17.23$residuals, type = "o")
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

``` r
plot(m17.24$residuals, type = "o")
```

![](dissertation-analysis-2017_files/figure-gfm/unnamed-chunk-56-2.png)<!-- -->

Assumptions Linearity - residual vs fitted appears within limits (but
bit wavy?) Normality of residuals - QQplots look ok and shapiro wilk not
significant Independence - looks fine multicolinearity - all within
limits homoscedacity - scale-location plot looks ok around 1

outlier 45

Comparing AIC (using corrected as number of covariates is 10% of data
<https://stats.stackexchange.com/questions/86768/aicc-for-small-sample-sizes>)

``` r
AICcmodavg::AICc(m17.23)
```

    ## [1] 336.0754

``` r
AICcmodavg::AICc(m17.24) #m24 has lower AICc -> preferable and higher r2
```

    ## [1] 331.1644

\#—
