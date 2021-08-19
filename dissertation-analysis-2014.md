dissertation analysis 2014
================

\#\#\#load packages

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

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## Warning: Missing column names filled in: 'X1' [1]

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   X1 = col_double(),
    ##   location_name = col_character(),
    ##   GSNI_PERIOD = col_character(),
    ##   twobias = col_double(),
    ##   cvd14fem = col_double(),
    ##   cvd14both = col_double(),
    ##   LEbirth2015 = col_double(),
    ##   phy14edit = col_double(),
    ##   scl14 = col_double(),
    ##   GDP14 = col_double(),
    ##   MMR14 = col_double()
    ## )

\#2014

# CODE BOOK (analysis14)

| Variable       | Description                                                                                                      |
| -------------- | ---------------------------------------------------------------------------------------------------------------- |
| location\_name | country                                                                                                          |
| GSNI\_PERIOD   | year data collection took place for GSNI survey 2005-2009 or 2010-2014                                           |
| twobias        | share of people with at least two bias                                                                           |
| cvd14fem       | age standardised CVD mortality rates for women per | | | 100,000 population (from global burden of disease) 2014 |
| cvd14both      | as above for whole population                                                                                    |
| LEbirth2015    | life expectancy at birth for women in 2015, from WHO (years)                                                     |
| phy14edit      | physicians per 1000 population 2014 (world bank) data added from other years n = 19 (2013-17)                    |
| scl14          | mean years of school 2014 (ourworldindata)                                                                       |
| GDP14          | Gross domestic product per capita (current US | | | dollar) 2014 (world bank)                                    |
| MMR14          | Maternal Mortality Ratio 2014 (world bank)                                                                       |

\#Descriptive analysis

    ## Rows: 75
    ## Columns: 10
    ## $ location_name <chr> "Algeria", "Andorra", "Argentina", "Armenia", "Australia…
    ## $ GSNI_PERIOD   <fct> 2010–2014, 2005–2009, 2010–2014, 2010–2014, 2010–2014, 2…
    ## $ twobias       <dbl> 87.00, 7.43, 42.49, 81.28, 23.00, 93.82, 71.70, 52.39, 4…
    ## $ cvd14fem      <dbl> 471.02644, 95.79920, 146.19661, 305.51958, 91.08401, 688…
    ## $ cvd14both     <dbl> 415.16724, 108.34739, 181.88855, 360.00518, 110.03050, 7…
    ## $ LEbirth2015   <dbl> 77.5, 84.8, 79.3, 77.8, 84.2, 73.4, 78.8, 78.6, 78.0, 63…
    ## $ phy14edit     <dbl> 1.8325, 3.3333, 3.9385, 2.8928, 3.4314, 3.4460, 5.0120, …
    ## $ scl14         <dbl> 7.9, 10.2, 9.8, 11.5, 12.7, 10.7, 12.1, 7.4, 10.9, 1.4, …
    ## $ GDP14         <dbl> 5494.3523, 41303.9294, 12334.7982, 3986.2316, 62510.7912…
    ## $ MMR14         <dbl> 114, NA, 42, 27, 6, 28, 3, 62, 11, 353, 11, 15, 31, 85, …

    ##  location_name         GSNI_PERIOD    twobias         cvd14fem      
    ##  Length:75          2005–2009:18   Min.   : 7.43   Min.   :  62.03  
    ##  Class :character   2010–2014:57   1st Qu.:41.94   1st Qu.: 128.06  
    ##  Mode  :character                  Median :68.56   Median : 241.30  
    ##                                    Mean   :62.83   Mean   : 258.01  
    ##                                    3rd Qu.:86.33   3rd Qu.: 345.30  
    ##                                    Max.   :98.07   Max.   :1024.86  
    ##                                                                     
    ##    cvd14both        LEbirth2015      phy14edit          scl14       
    ##  Min.   :  82.57   Min.   :61.00   Min.   :0.0464   Min.   : 1.400  
    ##  1st Qu.: 155.95   1st Qu.:74.45   1st Qu.:1.2909   1st Qu.: 7.600  
    ##  Median : 262.98   Median :78.60   Median :2.3020   Median :10.100  
    ##  Mean   : 289.87   Mean   :77.26   Mean   :2.2106   Mean   : 9.377  
    ##  3rd Qu.: 372.59   3rd Qu.:82.60   3rd Qu.:3.2245   3rd Qu.:11.800  
    ##  Max.   :1107.01   Max.   :86.40   Max.   :5.0120   Max.   :14.000  
    ##                                    NA's   :4        NA's   :6       
    ##      GDP14             MMR14       
    ##  Min.   :  566.9   Min.   :  2.00  
    ##  1st Qu.: 4094.7   1st Qu.: 10.25  
    ##  Median : 9181.1   Median : 25.50  
    ##  Mean   :20390.4   Mean   : 93.53  
    ##  3rd Qu.:31623.5   3rd Qu.: 82.75  
    ##  Max.   :97019.2   Max.   :943.00  
    ##  NA's   :7         NA's   :5

|                                                  |                |
| :----------------------------------------------- | :------------- |
| Name                                             | analysis14noNA |
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
| cvd14fem       |          0 |              1 |   252.63 |   165.80 |  62.03 |  127.85 |  228.78 |   326.31 |  1024.86 | ▇▃▂▁▁ |
| cvd14both      |          0 |              1 |   281.87 |   175.19 |  82.57 |  155.61 |  248.61 |   350.55 |  1107.01 | ▇▅▁▁▁ |
| LEbirth2015    |          0 |              1 |    77.21 |     6.69 |  61.00 |   73.75 |   78.75 |    82.65 |    86.40 | ▂▁▂▇▆ |
| phy14edit      |          0 |              1 |     2.17 |     1.32 |   0.05 |    1.10 |    2.25 |     3.20 |     5.01 | ▇▅▇▅▂ |
| scl14          |          0 |              1 |     9.44 |     2.93 |   1.40 |    7.60 |   10.05 |    11.88 |    14.00 | ▁▂▇▇▇ |
| GDP14          |          0 |              1 | 20168.88 | 23654.21 | 566.93 | 4022.39 | 8216.43 | 28878.57 | 97019.18 | ▇▁▂▁▁ |
| MMR14          |          0 |              1 |    98.12 |   173.40 |   2.00 |   10.00 |   28.00 |    90.25 |   943.00 | ▇▁▁▁▁ |

list of countries included

    ##  [1] "Algeria"             "Argentina"           "Armenia"            
    ##  [4] "Australia"           "Azerbaijan"          "Belarus"            
    ##  [7] "Brazil"              "Bulgaria"            "Burkina Faso"       
    ## [10] "Canada"              "Chile"               "China"              
    ## [13] "Colombia"            "Cyprus"              "Ecuador"            
    ## [16] "Estonia"             "Ethiopia"            "Finland"            
    ## [19] "France"              "Georgia"             "Germany"            
    ## [22] "Ghana"               "Haiti"               "Hungary"            
    ## [25] "India"               "Indonesia"           "Iraq"               
    ## [28] "Japan"               "Jordan"              "Kazakhstan"         
    ## [31] "Kuwait"              "Lebanon"             "Libya"              
    ## [34] "Malaysia"            "Mali"                "Mexico"             
    ## [37] "Morocco"             "Netherlands"         "New Zealand"        
    ## [40] "Nigeria"             "Norway"              "Pakistan"           
    ## [43] "Peru"                "Philippines"         "Poland"             
    ## [46] "Qatar"               "Romania"             "Rwanda"             
    ## [49] "Serbia"              "Singapore"           "Slovenia"           
    ## [52] "South Africa"        "Spain"               "Sweden"             
    ## [55] "Switzerland"         "Thailand"            "Trinidad and Tobago"
    ## [58] "Tunisia"             "Turkey"              "Ukraine"            
    ## [61] "United Kingdom"      "United States"       "Uruguay"            
    ## [64] "Uzbekistan"          "Zambia"              "Zimbabwe"

\#\#\#\#\#Histograms
![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

    ## $breaks
    ##  [1]    0  100  200  300  400  500  600  700  800  900 1000 1100
    ## 
    ## $counts
    ##  [1] 10 19 16 11  8  0  1  0  0  0  1
    ## 
    ## $density
    ##  [1] 0.0015151515 0.0028787879 0.0024242424 0.0016666667 0.0012121212
    ##  [6] 0.0000000000 0.0001515152 0.0000000000 0.0000000000 0.0000000000
    ## [11] 0.0001515152
    ## 
    ## $mids
    ##  [1]   50  150  250  350  450  550  650  750  850  950 1050
    ## 
    ## $xname
    ## [1] "cvd14fem"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

    ## $breaks
    ##  [1]    0  100  200  300  400  500  600  700  800  900 1000 1100 1200
    ## 
    ## $counts
    ##  [1]  3 23 15 12  8  3  0  1  0  0  0  1
    ## 
    ## $density
    ##  [1] 0.0004545455 0.0034848485 0.0022727273 0.0018181818 0.0012121212
    ##  [6] 0.0004545455 0.0000000000 0.0001515152 0.0000000000 0.0000000000
    ## [11] 0.0000000000 0.0001515152
    ## 
    ## $mids
    ##  [1]   50  150  250  350  450  550  650  750  850  950 1050 1150
    ## 
    ## $xname
    ## [1] "cvd14both"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

    ## $breaks
    ## [1] 60 65 70 75 80 85 90
    ## 
    ## $counts
    ## [1]  6  5  7 22 25  1
    ## 
    ## $density
    ## [1] 0.018181818 0.015151515 0.021212121 0.066666667 0.075757576 0.003030303
    ## 
    ## $mids
    ## [1] 62.5 67.5 72.5 77.5 82.5 87.5
    ## 
    ## $xname
    ## [1] "LEbirth2015"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

    ## $breaks
    ##  [1] 0.0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5
    ## 
    ## $counts
    ##  [1] 11  5  3  7 13  9  9  4  3  1  1
    ## 
    ## $density
    ##  [1] 0.33333333 0.15151515 0.09090909 0.21212121 0.39393939 0.27272727
    ##  [7] 0.27272727 0.12121212 0.09090909 0.03030303 0.03030303
    ## 
    ## $mids
    ##  [1] 0.25 0.75 1.25 1.75 2.25 2.75 3.25 3.75 4.25 4.75 5.25
    ## 
    ## $xname
    ## [1] "phy14edit"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->

    ## $breaks
    ## [1]  0  2  4  6  8 10 12 14
    ## 
    ## $counts
    ## [1]  1  3  4 14 11 18 15
    ## 
    ## $density
    ## [1] 0.007575758 0.022727273 0.030303030 0.106060606 0.083333333 0.136363636
    ## [7] 0.113636364
    ## 
    ## $mids
    ## [1]  1  3  5  7  9 11 13
    ## 
    ## $xname
    ## [1] "scl14"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->

    ## $breaks
    ##  [1] 0e+00 1e+04 2e+04 3e+04 4e+04 5e+04 6e+04 7e+04 8e+04 9e+04 1e+05
    ## 
    ## $counts
    ##  [1] 34 11  5  1  5  5  2  0  2  1
    ## 
    ## $density
    ##  [1] 5.151515e-05 1.666667e-05 7.575758e-06 1.515152e-06 7.575758e-06
    ##  [6] 7.575758e-06 3.030303e-06 0.000000e+00 3.030303e-06 1.515152e-06
    ## 
    ## $mids
    ##  [1]  5000 15000 25000 35000 45000 55000 65000 75000 85000 95000
    ## 
    ## $xname
    ## [1] "GDP14"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->

    ## $breaks
    ##  [1]    0  100  200  300  400  500  600  700  800  900 1000
    ## 
    ## $counts
    ##  [1] 51  6  2  2  3  0  1  0  0  1
    ## 
    ## $density
    ##  [1] 0.0077272727 0.0009090909 0.0003030303 0.0003030303 0.0004545455
    ##  [6] 0.0000000000 0.0001515152 0.0000000000 0.0000000000 0.0001515152
    ## 
    ## $mids
    ##  [1]  50 150 250 350 450 550 650 750 850 950
    ## 
    ## $xname
    ## [1] "MMR14"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

    ##  location_name         GSNI_PERIOD    twobias         cvd14fem      
    ##  Length:66          2005–2009:14   Min.   :10.75   Min.   :  62.03  
    ##  Class :character   2010–2014:52   1st Qu.:40.69   1st Qu.: 127.85  
    ##  Mode  :character                  Median :64.91   Median : 228.78  
    ##                                    Mean   :61.75   Mean   : 252.63  
    ##                                    3rd Qu.:85.82   3rd Qu.: 326.31  
    ##                                    Max.   :98.07   Max.   :1024.86  
    ##    cvd14both        LEbirth2015      phy14edit          scl14       
    ##  Min.   :  82.57   Min.   :61.00   Min.   :0.0464   Min.   : 1.400  
    ##  1st Qu.: 155.61   1st Qu.:73.75   1st Qu.:1.0965   1st Qu.: 7.600  
    ##  Median : 248.61   Median :78.75   Median :2.2545   Median :10.050  
    ##  Mean   : 281.87   Mean   :77.21   Mean   :2.1730   Mean   : 9.439  
    ##  3rd Qu.: 350.55   3rd Qu.:82.65   3rd Qu.:3.2001   3rd Qu.:11.875  
    ##  Max.   :1107.01   Max.   :86.40   Max.   :5.0120   Max.   :14.000  
    ##      GDP14             MMR14       
    ##  Min.   :  566.9   Min.   :  2.00  
    ##  1st Qu.: 4022.4   1st Qu.: 10.00  
    ##  Median : 8216.4   Median : 28.00  
    ##  Mean   :20168.9   Mean   : 98.12  
    ##  3rd Qu.:28878.6   3rd Qu.: 90.25  
    ##  Max.   :97019.2   Max.   :943.00

\#\#\#\#\#summary tables
<https://statsandr.com/blog/descriptive-statistics-in-r/>
<https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html>

    ## 
    ## 
    ## |Summary Statistics                                                      |n = 66                         |
    ## |:-----------------------------------------------------------------------|:------------------------------|
    ## |**GSNI index 2 or more bias**                                           |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |10.8                           |
    ## |&nbsp;&nbsp; max                                                        |98.1                           |
    ## |&nbsp;&nbsp; median (IQR)                                               |64.91 (40.69, 85.82)           |
    ## |**Female CVD age adjusted mortality rate (per 100 000 population)**     |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |62                             |
    ## |&nbsp;&nbsp; max                                                        |1024.9                         |
    ## |&nbsp;&nbsp; median (IQR)                                               |228.78 (127.85, 326.31)        |
    ## |**Population CVD age adjusted mortality rate (per 100 000 population)** |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |82.6                           |
    ## |&nbsp;&nbsp; max                                                        |1107                           |
    ## |&nbsp;&nbsp; median (IQR)                                               |248.61 (155.61, 350.55)        |
    ## |**Female life expectancy at birth**                                     |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |61                             |
    ## |&nbsp;&nbsp; max                                                        |86.4                           |
    ## |&nbsp;&nbsp; median (IQR)                                               |78.75 (73.75, 82.65)           |
    ## |**Physicians per 1000 population**                                      |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |0                              |
    ## |&nbsp;&nbsp; max                                                        |5                              |
    ## |&nbsp;&nbsp; median (IQR)                                               |2.25 (1.10, 3.20)              |
    ## |**GDP per capita**                                                      |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |566.9                          |
    ## |&nbsp;&nbsp; max                                                        |97019.2                        |
    ## |&nbsp;&nbsp; median (IQR)                                               |8,216.43 (4,022.39, 28,878.57) |
    ## |**Maternal Mortality Ratio**                                            |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |2                              |
    ## |&nbsp;&nbsp; max                                                        |943                            |
    ## |&nbsp;&nbsp; median (IQR)                                               |28.00 (10.00, 90.25)           |
    ## |**Mean years of schooling**                                             |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; min                                                        |1.4                            |
    ## |&nbsp;&nbsp; max                                                        |14                             |
    ## |&nbsp;&nbsp; median (IQR)                                               |10.05 (7.60, 11.88)            |
    ## |**Year of Index Collection**                                            |&nbsp;&nbsp;                   |
    ## |&nbsp;&nbsp; 2005-2009                                                  |14 (21)                        |
    ## |&nbsp;&nbsp; 2010-2014                                                  |52 (79)                        |

\#OUTCOME 1 - CVD mortality female

\#\#Scatter plots

\#\#\#CVD mort (female) \~ GSNI

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  twobias and cvd14fem
    ## S = 15462, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.6772362

\#\#\#CVD mort (female) \~ physicians per 1000

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  cvd14fem and phy14edit
    ## S = 59580, p-value = 0.04885
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.2437115

\#\#\#CVD mort (female) \~ mean years of schooling

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

    ## Warning in cor.test.default(scl14, cvd14fem, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  scl14 and cvd14fem
    ## S = 66349, p-value = 0.001412
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.3850194

\#\#\#CVD mort (female) \~ GDP per capita

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  GDP14 and cvd14fem
    ## S = 78220, p-value = 2.734e-08
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.6328149

\#\#\#CVD mort (female) \~ MMR

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

    ## Warning in cor.test.default(MMR14, cvd14fem, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  MMR14 and cvd14fem
    ## S = 25793, p-value = 9.58e-05
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.4615851

\#\#\#CVD mort (female) \~ year of index

    ## notch went outside hinges. Try setting notch=FALSE.

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

\#\#Univariable regression

\#\#\#CVD mort (female) \~ GSNI

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ twobias, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -233.29  -69.07  -22.50   40.53  677.85 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  28.3134    42.5752   0.665    0.508    
    ## twobias       3.6327     0.6341   5.729 2.93e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 135.9 on 64 degrees of freedom
    ## Multiple R-squared:  0.339,  Adjusted R-squared:  0.3287 
    ## F-statistic: 32.82 on 1 and 64 DF,  p-value: 2.928e-07

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |   28.313 |    42.575 |     0.665 |   0.508 |
| twobias     |    3.633 |     0.634 |     5.729 |   0.000 |

\#\#\#CVD mort (female) \~ physicians per 1000

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ phy14edit, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -188.06 -117.08  -35.73   58.63  775.28 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   285.51      39.66   7.199 8.32e-10 ***
    ## phy14edit     -15.13      15.65  -0.967    0.337    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 165.9 on 64 degrees of freedom
    ## Multiple R-squared:  0.0144, Adjusted R-squared:  -0.0009971 
    ## F-statistic: 0.9353 on 1 and 64 DF,  p-value: 0.3371

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

\#\#\#CVD mort (female) \~ mean years of schooling

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ scl14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -170.45 -115.86  -42.71   59.90  791.04 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  348.045     68.701   5.066 3.69e-06 ***
    ## scl14        -10.108      6.955  -1.453    0.151    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 164.4 on 64 degrees of freedom
    ## Multiple R-squared:  0.03195,    Adjusted R-squared:  0.01682 
    ## F-statistic: 2.112 on 1 and 64 DF,  p-value: 0.151

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

\#\#\#CVD mort (female) \~ GDP per capita

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ GDP14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -209.97  -70.21  -33.72   42.79  718.38 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.141e+02  2.442e+01  12.860  < 2e-16 ***
    ## GDP14       -3.046e-03  7.891e-04  -3.861 0.000266 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 150.5 on 64 degrees of freedom
    ## Multiple R-squared:  0.1889, Adjusted R-squared:  0.1762 
    ## F-statistic:  14.9 on 1 and 64 DF,  p-value: 0.000266

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->

\#\#\#CVD mort (female) \~ MMR

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ MMR14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -177.31 -117.38  -31.59   59.20  781.96 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 238.6246    23.4115  10.193 4.95e-15 ***
    ## MMR14         0.1427     0.1182   1.208    0.232    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 165.2 on 64 degrees of freedom
    ## Multiple R-squared:  0.02228,    Adjusted R-squared:  0.007006 
    ## F-statistic: 1.459 on 1 and 64 DF,  p-value: 0.2316

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->

\#\#\#CVD mort (female) \~ year of index

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -196.28 -129.79  -22.93   75.64  766.56 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            231.54      44.56   5.196 2.26e-06 ***
    ## GSNI_PERIOD2010–2014    26.76      50.20   0.533    0.596    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 166.7 on 64 degrees of freedom
    ## Multiple R-squared:  0.004421,   Adjusted R-squared:  -0.01113 
    ## F-statistic: 0.2842 on 1 and 64 DF,  p-value: 0.5958

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-19-4.png)<!-- -->

\#\#Multivariable regression

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ twobias + phy14edit + scl14 + MMR14 + 
    ##     GDP14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -189.89  -66.68  -15.83   34.86  611.53 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.704e+02  1.165e+02  -1.462   0.1489    
    ## twobias      4.226e+00  8.430e-01   5.013 5.04e-06 ***
    ## phy14edit    3.184e+01  2.116e+01   1.505   0.1376    
    ## scl14        1.226e+01  1.020e+01   1.202   0.2340    
    ## MMR14        8.876e-02  1.302e-01   0.682   0.4979    
    ## GDP14       -1.568e-03  9.179e-04  -1.709   0.0927 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 129.6 on 60 degrees of freedom
    ## Multiple R-squared:  0.4364, Adjusted R-squared:  0.3895 
    ## F-statistic: 9.293 on 5 and 60 DF,  p-value: 1.361e-06

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-4.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ twobias + phy14edit + scl14 + MMR14 + 
    ##     GDP14 + GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -181.53  -68.46  -25.12   45.14  601.72 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.566e+02  1.129e+02  -1.387   0.1708    
    ## twobias               4.883e+00  8.672e-01   5.630 5.27e-07 ***
    ## phy14edit             3.409e+01  2.051e+01   1.662   0.1018    
    ## scl14                 1.489e+01  9.945e+00   1.497   0.1397    
    ## MMR14                 4.500e-02  1.275e-01   0.353   0.7255    
    ## GDP14                -1.823e-03  8.959e-04  -2.035   0.0464 *  
    ## GSNI_PERIOD2010–2014 -9.461e+01  4.225e+01  -2.239   0.0289 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 125.4 on 59 degrees of freedom
    ## Multiple R-squared:  0.4806, Adjusted R-squared:  0.4277 
    ## F-statistic: 9.098 on 6 and 59 DF,  p-value: 4.83e-07

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-5.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-6.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-7.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-8.png)<!-- -->

| term                  |  estimate | std.error | statistic | p.value |
| :-------------------- | --------: | --------: | --------: | ------: |
| (Intercept)           | \-156.608 |   112.947 |   \-1.387 |   0.171 |
| twobias               |     4.883 |     0.867 |     5.630 |   0.000 |
| phy14edit             |    34.086 |    20.507 |     1.662 |   0.102 |
| scl14                 |    14.890 |     9.945 |     1.497 |   0.140 |
| MMR14                 |     0.045 |     0.128 |     0.353 |   0.725 |
| GDP14                 |   \-0.002 |     0.001 |   \-2.035 |   0.046 |
| GSNI\_PERIOD2010–2014 |  \-94.608 |    42.252 |   \-2.239 |   0.029 |

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ twobias + phy14edit + scl14 + GDP14 + 
    ##     GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -186.20  -68.70  -24.42   47.13  600.91 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.372e+02  9.790e+01  -1.401   0.1663    
    ## twobias               4.889e+00  8.607e-01   5.680 4.18e-07 ***
    ## phy14edit             3.200e+01  1.949e+01   1.642   0.1059    
    ## scl14                 1.396e+01  9.518e+00   1.467   0.1477    
    ## GDP14                -1.836e-03  8.886e-04  -2.066   0.0431 *  
    ## GSNI_PERIOD2010–2014 -9.689e+01  4.145e+01  -2.338   0.0228 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 124.5 on 60 degrees of freedom
    ## Multiple R-squared:  0.4795, Adjusted R-squared:  0.4361 
    ## F-statistic: 11.05 on 5 and 60 DF,  p-value: 1.431e-07

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-9.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-10.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-11.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-20-12.png)<!-- -->

<http://r-statistics.co/Outlier-Treatment-With-R.html>
![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

    ##    location_name GSNI_PERIOD twobias  cvd14fem cvd14both LEbirth2015 phy14edit
    ## 1:      Ethiopia   2005–2009   35.14  224.2859  229.4954        68.6    0.0464
    ## 2:       Nigeria   2010–2014   94.99  249.7113  246.8299        62.8    0.3828
    ## 3:         Qatar   2010–2014   94.90  487.7226  351.4539        75.9    1.7351
    ## 4:    Uzbekistan   2010–2014   87.73 1024.8648 1107.0130        73.9    2.3742
    ##    scl14      GDP14 MMR14
    ## 1:   2.5   566.9265   472
    ## 2:   5.9  3098.9863   943
    ## 3:   9.8 83858.4769    10
    ## 4:  11.3  2492.3366    30

    ##    rstudent unadjusted p-value Bonferroni p
    ## 64 6.512177         1.9239e-08   1.2698e-06

assess for colinearity

    ##   twobias phy14edit     scl14     MMR14     GDP14 
    ##  1.943714  2.997944  3.463677  1.973197  1.825508

    ##     twobias   phy14edit       scl14       MMR14       GDP14 GSNI_PERIOD 
    ##    2.194520    3.005140    3.512574    2.020651    1.855386    1.251682

assess for normality of residuals

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m7$residuals
    ## W = 0.84874, p-value = 1.109e-06

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m8$residuals
    ## W = 0.84125, p-value = 6.59e-07

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

\#\#\#\#Assumptions Linearity - residual vs fitted appears within limits
Normality of residuals - QQplots show some deviance, shapiro-wilk are
significant suggesting lack of normality Independence - looks fine
multicolinearity - all within limits homoscedacity - scale-location plot
for m8 looks better than m7, (flatter and closer to 1)

\#\#\#\#Try removing Uzbekistan

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ twobias + phy14edit + scl14 + MMR14 + 
    ##     GDP14 + GSNI_PERIOD, data = analysis14noNAnoUzbek)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -183.26  -67.86  -26.51   47.24  602.61 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.519e+02  1.161e+02  -1.309   0.1958    
    ## twobias               4.853e+00  8.861e-01   5.477 9.75e-07 ***
    ## phy14edit             3.530e+01  2.147e+01   1.644   0.1056    
    ## scl14                 1.428e+01  1.044e+01   1.368   0.1765    
    ## MMR14                 4.493e-02  1.286e-01   0.349   0.7281    
    ## GDP14                -1.829e-03  9.038e-04  -2.024   0.0475 *  
    ## GSNI_PERIOD2010–2014 -9.357e+01  4.289e+01  -2.182   0.0332 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 126.5 on 58 degrees of freedom
    ## Multiple R-squared:  0.4769, Adjusted R-squared:  0.4228 
    ## F-statistic: 8.815 on 6 and 58 DF,  p-value: 7.832e-07

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-25-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-25-4.png)<!-- -->

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m8noUzbek$residuals
    ## W = 0.84215, p-value = 8.18e-07

\#\#\#\#Comparing AIC (using corrected as number of covariates is 10% of
data
<https://stats.stackexchange.com/questions/86768/aicc-for-small-sample-sizes>)

    ## [1] 839.0019

    ## [1] 836.2142

\#OUTCOME 2 - CVD mortality

\#\#Scatter plots

\#\#\#CVD mort \~ GSNI

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  twobias and cvd14both
    ## S = 18386, p-value = 7.78e-08
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.6161987

\#\#\#CVD mort \~ physicians per 1000

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  cvd14both and phy14edit
    ## S = 55422, p-value = 0.2078
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.1569147

\#\#\#CVD mort \~ mean years of schooling

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

    ## Warning in cor.test.default(scl14, cvd14both, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  scl14 and cvd14both
    ## S = 63212, p-value = 0.008916
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.3195339

\#\#\#CVD mort \~ GDP per capita

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  GDP14 and cvd14both
    ## S = 76662, p-value = 1.935e-07
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.6002922

\#\#\#CVD mort \~ MMR

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

    ## Warning in cor.test.default(MMR14, cvd14both, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  MMR14 and cvd14both
    ## S = 29336, p-value = 0.001302
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.3876162

\#\#\#CVD mort \~ year of index

    ## notch went outside hinges. Try setting notch=FALSE.

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

\#\#Univariable regression

\#\#\#CVD mort \~ GSNI

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ twobias, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -215.59  -83.37  -24.77   41.71  734.28 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  65.8931    46.9019   1.405    0.165    
    ## twobias       3.4976     0.6985   5.007  4.6e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 149.7 on 64 degrees of freedom
    ## Multiple R-squared:  0.2815, Adjusted R-squared:  0.2702 
    ## F-statistic: 25.07 on 1 and 64 DF,  p-value: 4.604e-06

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-33-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-33-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-33-4.png)<!-- -->

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |   65.893 |    46.902 |     1.405 |   0.165 |
| twobias     |    3.498 |     0.699 |     5.007 |   0.000 |

\#\#\#CVD mort \~ physicians per 1000

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ phy14edit, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -198.84 -124.81  -33.54   67.74  825.68 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  287.665     42.201   6.817  3.9e-09 ***
    ## phy14edit     -2.669     16.648  -0.160    0.873    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 176.5 on 64 degrees of freedom
    ## Multiple R-squared:  0.0004014,  Adjusted R-squared:  -0.01522 
    ## F-statistic: 0.0257 on 1 and 64 DF,  p-value: 0.8731

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-34-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-34-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-34-4.png)<!-- -->
\#\#\#CVD mort \~ mean years of schooling

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ scl14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -187.03 -121.16  -44.62   64.92  837.01 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  342.069     73.356   4.663 1.63e-05 ***
    ## scl14         -6.378      7.427  -0.859    0.394    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 175.5 on 64 degrees of freedom
    ## Multiple R-squared:  0.01139,    Adjusted R-squared:  -0.004054 
    ## F-statistic: 0.7375 on 1 and 64 DF,  p-value: 0.3937

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-35-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-35-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-35-4.png)<!-- -->
\#\#\#CVD mort \~ GDP per capita

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ GDP14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -233.02  -84.24  -27.40   49.03  764.59 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.510e+02  2.540e+01  13.816  < 2e-16 ***
    ## GDP14       -3.426e-03  8.208e-04  -4.174 9.22e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 156.5 on 64 degrees of freedom
    ## Multiple R-squared:  0.214,  Adjusted R-squared:  0.2017 
    ## F-statistic: 17.42 on 1 and 64 DF,  p-value: 9.22e-05

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-36-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-36-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-36-4.png)<!-- -->
\#\#\#CVD mort \~ MMR

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ MMR14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -192.65 -119.91  -30.70   74.65  830.01 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 274.86092   24.95453  11.014   <2e-16 ***
    ## MMR14         0.07138    0.12598   0.567    0.573    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 176.1 on 64 degrees of freedom
    ## Multiple R-squared:  0.004992,   Adjusted R-squared:  -0.01056 
    ## F-statistic: 0.3211 on 1 and 64 DF,  p-value: 0.5729

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-37-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-37-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-37-4.png)<!-- -->
\#\#\#CVD mort \~ year of index

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -205.18 -130.46  -34.69   70.12  819.27 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            260.02      47.09   5.522 6.52e-07 ***
    ## GSNI_PERIOD2010–2014    27.73      53.05   0.523    0.603    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 176.2 on 64 degrees of freedom
    ## Multiple R-squared:  0.004252,   Adjusted R-squared:  -0.01131 
    ## F-statistic: 0.2733 on 1 and 64 DF,  p-value: 0.603

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-38-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-38-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-38-4.png)<!-- -->
\#\#Multivariable regression

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ twobias + phy14edit + scl14 + MMR14 + 
    ##     GDP14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -221.93  -83.53  -19.17   49.25  640.57 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.606e+02  1.199e+02  -1.340  0.18537    
    ## twobias      4.123e+00  8.677e-01   4.751  1.3e-05 ***
    ## phy14edit    4.532e+01  2.178e+01   2.081  0.04167 *  
    ## scl14        1.437e+01  1.050e+01   1.369  0.17610    
    ## MMR14        6.106e-02  1.340e-01   0.456  0.65023    
    ## GDP14       -2.589e-03  9.447e-04  -2.741  0.00807 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 133.3 on 60 degrees of freedom
    ## Multiple R-squared:  0.4652, Adjusted R-squared:  0.4207 
    ## F-statistic: 10.44 on 5 and 60 DF,  p-value: 3.08e-07

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-4.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ twobias + phy14edit + scl14 + MMR14 + 
    ##     GDP14 + GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -233.88  -79.83  -21.16   53.17  629.13 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.446e+02  1.148e+02  -1.259  0.21292    
    ## twobias               4.889e+00  8.817e-01   5.544 7.27e-07 ***
    ## phy14edit             4.794e+01  2.085e+01   2.300  0.02503 *  
    ## scl14                 1.744e+01  1.011e+01   1.725  0.08981 .  
    ## MMR14                 9.997e-03  1.297e-01   0.077  0.93881    
    ## GDP14                -2.886e-03  9.108e-04  -3.169  0.00243 ** 
    ## GSNI_PERIOD2010–2014 -1.104e+02  4.296e+01  -2.570  0.01272 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 127.5 on 59 degrees of freedom
    ## Multiple R-squared:  0.5191, Adjusted R-squared:  0.4702 
    ## F-statistic: 10.61 on 6 and 59 DF,  p-value: 5.749e-08

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-5.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-6.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-7.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-8.png)<!-- -->

| term                  |  estimate | std.error | statistic | p.value |
| :-------------------- | --------: | --------: | --------: | ------: |
| (Intercept)           | \-144.594 |   114.831 |   \-1.259 |   0.213 |
| twobias               |     4.889 |     0.882 |     5.544 |   0.000 |
| phy14edit             |    47.945 |    20.849 |     2.300 |   0.025 |
| scl14                 |    17.438 |    10.111 |     1.725 |   0.090 |
| MMR14                 |     0.010 |     0.130 |     0.077 |   0.939 |
| GDP14                 |   \-0.003 |     0.001 |   \-3.169 |   0.002 |
| GSNI\_PERIOD2010–2014 | \-110.388 |    42.957 |   \-2.570 |   0.013 |

    ##    rstudent unadjusted p-value Bonferroni p
    ## 64 6.842594         5.3852e-09   3.5542e-07

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ twobias + phy14edit + scl14 + GDP14 + 
    ##     GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -234.20  -75.78  -21.53   52.37  628.94 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.403e+02  9.943e+01  -1.411  0.16346    
    ## twobias               4.890e+00  8.742e-01   5.594 5.79e-07 ***
    ## phy14edit             4.748e+01  1.980e+01   2.398  0.01959 *  
    ## scl14                 1.723e+01  9.667e+00   1.783  0.07973 .  
    ## GDP14                -2.889e-03  9.025e-04  -3.201  0.00219 ** 
    ## GSNI_PERIOD2010–2014 -1.109e+02  4.210e+01  -2.634  0.01071 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 126.5 on 60 degrees of freedom
    ## Multiple R-squared:  0.519,  Adjusted R-squared:  0.4789 
    ## F-statistic: 12.95 on 5 and 60 DF,  p-value: 1.494e-08

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-9.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-10.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-11.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-39-12.png)<!-- -->

assess for colinearity

    ##   twobias phy14edit     scl14     MMR14     GDP14 
    ##  1.943714  2.997944  3.463677  1.973197  1.825508

    ##     twobias   phy14edit       scl14       MMR14       GDP14 GSNI_PERIOD 
    ##    2.194520    3.005140    3.512574    2.020651    1.855386    1.251682

assess for normality of residuals

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m15$residuals
    ## W = 0.85323, p-value = 1.528e-06

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m16$residuals
    ## W = 0.84278, p-value = 7.322e-07

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-42-2.png)<!-- -->

Comparing AIC (using corrected as number of covariates is 10% of data
<https://stats.stackexchange.com/questions/86768/aicc-for-small-sample-sizes>)

\#\#\#\#noUzbek

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ twobias + phy14edit + scl14 + MMR14 + 
    ##     GDP14 + GSNI_PERIOD, data = analysis14noNAnoUzbek)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -232.62  -83.65  -19.59   51.24  630.94 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.350e+02  1.179e+02  -1.145  0.25672    
    ## twobias               4.828e+00  8.998e-01   5.365 1.48e-06 ***
    ## phy14edit             5.040e+01  2.180e+01   2.312  0.02437 *  
    ## scl14                 1.621e+01  1.060e+01   1.529  0.13178    
    ## MMR14                 9.846e-03  1.306e-01   0.075  0.94016    
    ## GDP14                -2.899e-03  9.178e-04  -3.159  0.00251 ** 
    ## GSNI_PERIOD2010–2014 -1.083e+02  4.355e+01  -2.486  0.01582 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 128.4 on 58 degrees of freedom
    ## Multiple R-squared:  0.5172, Adjusted R-squared:  0.4673 
    ## F-statistic: 10.36 on 6 and 58 DF,  p-value: 8.922e-08

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-43-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-43-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-43-4.png)<!-- -->

    ## [1] 842.8055

    ## [1] 838.3986

Assumptions Linearity - residual vs fitted appears within limits
Normality of residuals - QQplots show some deviance, shapiro-wilk are
significant suggesting lack of normality Independence - looks fine
multicolinearity - all within limits homoscedacity - scale-location plot
for m8 looks better than m7, (flatter and closer to 1)

\#OUTCOME 3 Life expectancy for women

## Scatter plots

### Life Expectancy \~ GSNI

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

    ## Warning in cor.test.default(twobias, LEbirth2015, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  twobias and LEbirth2015
    ## S = 82853, p-value = 3.718e-12
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.7295238

\#\#\#Life Expectancy \~ physicians per 1000
![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

    ## Warning in cor.test.default(phy14edit, LEbirth2015, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  phy14edit and LEbirth2015
    ## S = 18823, p-value = 6.522e-08
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.6070735

\#\#\#Life Expectancy \~ mean years of schooling

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

    ## Warning in cor.test.default(scl14, LEbirth2015, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  scl14 and LEbirth2015
    ## S = 16359, p-value = 1.859e-09
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.6585144

\#\#\#Life Expectancy \~ GDP per capita
![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-48-2.png)<!-- -->

    ## Warning in cor.test.default(GDP14, LEbirth2015, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  GDP14 and LEbirth2015
    ## S = 7703.3, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.8391966

\#\#\#Life Expectancy \~ MMR

    ## `geom_smooth()` using formula 'y ~ x'

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

    ## Warning in cor.test.default(MMR14, LEbirth2015, method = "spearman"): Cannot
    ## compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  MMR14 and LEbirth2015
    ## S = 87526, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.8270673

\#\#\#Life Expectancy \~ year of index

    ## notch went outside hinges. Try setting notch=FALSE.

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

\#\#Univariable regression

\#\#\#Life Expectancy \~ GSNI

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ twobias, data = analysis14noNA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.0511  -1.6738   0.5984   2.9578  11.5699 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 87.51883    1.57945  55.411  < 2e-16 ***
    ## twobias     -0.16698    0.02352  -7.099 1.25e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.04 on 64 degrees of freedom
    ## Multiple R-squared:  0.4405, Adjusted R-squared:  0.4318 
    ## F-statistic: 50.39 on 1 and 64 DF,  p-value: 1.249e-09

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-51-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-51-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-51-4.png)<!-- -->

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |   87.519 |     1.579 |    55.411 |       0 |
| twobias     |  \-0.167 |     0.024 |   \-7.099 |       0 |

\#\#\#Life Expectancy \~ physicians per 1000

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ phy14edit, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.5680 -2.8826  0.6241  3.6286  9.5692 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  69.4758     1.1505   60.39  < 2e-16 ***
    ## phy14edit     3.5583     0.4539    7.84 6.18e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.812 on 64 degrees of freedom
    ## Multiple R-squared:  0.4899, Adjusted R-squared:  0.4819 
    ## F-statistic: 61.46 on 1 and 64 DF,  p-value: 6.178e-11

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-52-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-52-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-52-4.png)<!-- -->

\#\#\#Life Expectancy \~ mean years of schooling

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ scl14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2052  -2.5658   0.8328   3.2410  10.6957 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  61.9538     1.9865  31.187  < 2e-16 ***
    ## scl14         1.6160     0.2011   8.035 2.79e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.754 on 64 degrees of freedom
    ## Multiple R-squared:  0.5022, Adjusted R-squared:  0.4944 
    ## F-statistic: 64.57 on 1 and 64 DF,  p-value: 2.792e-11

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-53-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-53-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-53-4.png)<!-- -->

\#\#\#Life Expectancy \~ GDP per capita

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ GDP14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.079  -1.881   1.642   3.677   6.210 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 7.384e+01  8.821e-01  83.713  < 2e-16 ***
    ## GDP14       1.670e-04  2.850e-05   5.861 1.75e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.435 on 64 degrees of freedom
    ## Multiple R-squared:  0.3493, Adjusted R-squared:  0.3391 
    ## F-statistic: 34.35 on 1 and 64 DF,  p-value: 1.749e-07

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-54-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-54-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-54-4.png)<!-- -->
\#\#\#Life Expectancy \~ MMR

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ MMR14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.9826  -2.1824   0.3134   2.9699  11.7609 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 80.24720    0.56859  141.13  < 2e-16 ***
    ## MMR14       -0.03097    0.00287  -10.79 4.83e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.013 on 64 degrees of freedom
    ## Multiple R-squared:  0.6453, Adjusted R-squared:  0.6398 
    ## F-statistic: 116.4 on 1 and 64 DF,  p-value: 4.831e-16

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-55-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-55-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-55-4.png)<!-- -->
\#\#\#Life Expectancy \~ year of index

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.483  -3.662   1.317   5.167   8.917 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            76.188      1.795   42.44   <2e-16 ***
    ## GSNI_PERIOD2010–2014    1.295      2.022    0.64    0.524    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.716 on 64 degrees of freedom
    ## Multiple R-squared:  0.006365,   Adjusted R-squared:  -0.00916 
    ## F-statistic:  0.41 on 1 and 64 DF,  p-value: 0.5243

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-56-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-56-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-56-4.png)<!-- -->

\#\#Multivariable regression

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ twobias + phy14edit + scl14 + MMR14 + 
    ##     GDP14, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.5600 -1.6300 -0.2972  1.9758  7.8241 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.164e+01  2.809e+00  29.067  < 2e-16 ***
    ## twobias     -7.019e-02  2.033e-02  -3.453  0.00102 ** 
    ## phy14edit    5.307e-01  5.101e-01   1.040  0.30236    
    ## scl14       -2.712e-03  2.459e-01  -0.011  0.99124    
    ## MMR14       -2.155e-02  3.139e-03  -6.865 4.24e-09 ***
    ## GDP14        4.408e-05  2.213e-05   1.992  0.05097 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.124 on 60 degrees of freedom
    ## Multiple R-squared:  0.7985, Adjusted R-squared:  0.7817 
    ## F-statistic: 47.55 on 5 and 60 DF,  p-value: < 2.2e-16

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-4.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ twobias + phy14edit + scl14 + MMR14 + 
    ##     GDP14 + GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.7389 -1.6750  0.0731  1.7729  6.6983 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           8.129e+01  2.710e+00  29.999  < 2e-16 ***
    ## twobias              -8.691e-02  2.081e-02  -4.177 9.88e-05 ***
    ## phy14edit             4.734e-01  4.920e-01   0.962   0.3398    
    ## scl14                -6.964e-02  2.386e-01  -0.292   0.7714    
    ## MMR14                -2.043e-02  3.060e-03  -6.678 9.46e-09 ***
    ## GDP14                 5.056e-05  2.149e-05   2.352   0.0220 *  
    ## GSNI_PERIOD2010–2014  2.410e+00  1.014e+00   2.377   0.0207 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.009 on 59 degrees of freedom
    ## Multiple R-squared:  0.8161, Adjusted R-squared:  0.7974 
    ## F-statistic: 43.64 on 6 and 59 DF,  p-value: < 2.2e-16

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-5.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-6.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-7.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-8.png)<!-- -->

| term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           |   81.290 |     2.710 |    29.999 |   0.000 |
| twobias               |  \-0.087 |     0.021 |   \-4.177 |   0.000 |
| phy14edit             |    0.473 |     0.492 |     0.962 |   0.340 |
| scl14                 |  \-0.070 |     0.239 |   \-0.292 |   0.771 |
| MMR14                 |  \-0.020 |     0.003 |   \-6.678 |   0.000 |
| GDP14                 |    0.000 |     0.000 |     2.352 |   0.022 |
| GSNI\_PERIOD2010–2014 |    2.410 |     1.014 |     2.377 |   0.021 |

    ##    rstudent unadjusted p-value Bonferroni p
    ## 40 3.620126         0.00061965     0.040897

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ twobias + phy14edit + scl14 + GDP14 + 
    ##     GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.4651  -2.5521   0.2207   2.9981   7.7849 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           7.247e+01  3.109e+00  23.310  < 2e-16 ***
    ## twobias              -8.971e-02  2.733e-02  -3.282  0.00172 ** 
    ## phy14edit             1.422e+00  6.190e-01   2.297  0.02513 *  
    ## scl14                 3.533e-01  3.023e-01   1.169  0.24703    
    ## GDP14                 5.642e-05  2.822e-05   1.999  0.05011 .  
    ## GSNI_PERIOD2010–2014  3.447e+00  1.316e+00   2.619  0.01115 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.954 on 60 degrees of freedom
    ## Multiple R-squared:  0.6771, Adjusted R-squared:  0.6502 
    ## F-statistic: 25.17 on 5 and 60 DF,  p-value: 1.399e-13

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-9.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-10.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-11.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-12.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ twobias + log(phy14edit) + scl14 + 
    ##     log(GDP14) + GSNI_PERIOD, data = analysis14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.9577 -1.6872  0.3242  1.9324  7.5722 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          64.89547    5.53832  11.718  < 2e-16 ***
    ## twobias              -0.07226    0.02165  -3.337 0.001457 ** 
    ## log(phy14edit)        2.78040    0.60884   4.567 2.51e-05 ***
    ## scl14                -0.37770    0.24896  -1.517 0.134482    
    ## log(GDP14)            1.99548    0.53642   3.720 0.000441 ***
    ## GSNI_PERIOD2010–2014  1.27122    1.06032   1.199 0.235280    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.071 on 60 degrees of freedom
    ## Multiple R-squared:  0.8052, Adjusted R-squared:  0.789 
    ## F-statistic:  49.6 on 5 and 60 DF,  p-value: < 2.2e-16

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-13.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-14.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-15.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-57-16.png)<!-- -->

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

assess for colinearity

    ##   twobias phy14edit     scl14     MMR14     GDP14 
    ##  1.943714  2.997944  3.463677  1.973197  1.825508

    ##     twobias   phy14edit       scl14       MMR14       GDP14 GSNI_PERIOD 
    ##    2.194520    3.005140    3.512574    2.020651    1.855386    1.251682

assess for normality of residuals

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m23$residuals
    ## W = 0.97679, p-value = 0.2516

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m24$residuals
    ## W = 0.98552, p-value = 0.6377

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-61-2.png)<!-- -->

Assumptions Linearity - residual vs fitted appears non linear, will
investigate log transformation of variables Normality of residuals -
QQplots look ok and shapiro wilk not significant Independence - looks
fine multicolinearity - all within limits homoscedacity - scale-location
plot looks ok around 1

\#\#\#\#no Nigeria

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ twobias + phy14edit + scl14 + MMR14 + 
    ##     GDP14 + GSNI_PERIOD, data = analysis14noNAnoNig)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.4075 -1.4343  0.3266  1.3697  4.7081 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           8.559e+01  2.739e+00  31.245  < 2e-16 ***
    ## twobias              -9.070e-02  1.898e-02  -4.778 1.25e-05 ***
    ## phy14edit             1.484e-01  4.571e-01   0.325   0.7466    
    ## scl14                -2.613e-01  2.237e-01  -1.168   0.2475    
    ## MMR14                -3.092e-02  4.020e-03  -7.692 2.01e-10 ***
    ## GDP14                 4.410e-05  1.966e-05   2.243   0.0287 *  
    ## GSNI_PERIOD2010–2014  1.631e+00  9.481e-01   1.720   0.0908 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.741 on 58 degrees of freedom
    ## Multiple R-squared:  0.8383, Adjusted R-squared:  0.8215 
    ## F-statistic: 50.11 on 6 and 58 DF,  p-value: < 2.2e-16

![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-62-2.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-62-3.png)<!-- -->![](dissertation-analysis-2014_files/figure-gfm/unnamed-chunk-62-4.png)<!-- -->

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  m24noNig$residuals
    ## W = 0.96159, p-value = 0.04152

Comparing AIC (using corrected as number of covariates is 10% of data
<https://stats.stackexchange.com/questions/86768/aicc-for-small-sample-sizes>)

    ## [1] 347.2905

    ## [1] 343.8477

    ## [1] 326.7144

\#—
