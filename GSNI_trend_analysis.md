GSNI change over time
================

How fast to GSNI change over time?

``` r
library(readxl)
library(data.table)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
library(skimr)
library(ggplot2)
library(readr)
```

``` r
results14 <- read_csv("analysis14.csv")
```

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

``` r
setDT(results14)

results14[, GSNI_PERIOD := as.factor(GSNI_PERIOD)]
results14[, X1:= NULL]

trend <- read_excel("gsni_tables.xlsx", 3)
```

    ## New names:
    ## * `` -> ...2
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * ...

``` r
trend <- as.data.table(trend)
```

``` r
trend <- trend[, c(1,4,5)]
glimpse(trend)
```

    ## Rows: 47
    ## Columns: 3
    ## $ `Table A3a: Gender Social Norms Index (GSNI), trends` <chr> NA, NA, NA, NA, …
    ## $ ...4                                                  <chr> NA, NA, NA, "GSN…
    ## $ ...5                                                  <chr> NA, NA, NA, NA, …

``` r
names(trend) <- c("country", "time1", "time2")

#time 1 = 2005-09 collection 
#time 2 = 2010-14 collection

trend <- trend[8:38,] 

glimpse(trend)
```

    ## Rows: 31
    ## Columns: 3
    ## $ country <chr> "Argentina", "Australia", "Brazil", "Chile", "China", "Cyprus"…
    ## $ time1   <chr> "42.22", "27.2", "53.63", "60.94", "65.2", "51.13", "78.930000…
    ## $ time2   <chr> "42.49", "23", "52.39", "42.2", "64.42", "49.44", "77.12", "33…

``` r
trend[, time1 := as.numeric(time1)]
trend[, time2 := as.numeric(time2)]

glimpse(trend)
```

    ## Rows: 31
    ## Columns: 3
    ## $ country <chr> "Argentina", "Australia", "Brazil", "Chile", "China", "Cyprus"…
    ## $ time1   <dbl> 42.22, 27.20, 53.63, 60.94, 65.20, 51.13, 78.93, 31.13, 87.21,…
    ## $ time2   <dbl> 42.49, 23.00, 52.39, 42.20, 64.42, 49.44, 77.12, 33.07, 92.69,…

inspect means and boxplot

``` r
trend[, boxplot(time1, time2)]
```

    ## Warning in as.data.table.list(jval, .named = NULL): Item 2 has 2 rows but
    ## longest item has 5; recycled with remainder.

    ## Warning in as.data.table.list(jval, .named = NULL): Item 3 has 2 rows but
    ## longest item has 5; recycled with remainder.

    ## Warning in as.data.table.list(jval, .named = NULL): Item 4 has 0 rows but
    ## longest item has 5; filled with NA

    ## Warning in as.data.table.list(jval, .named = NULL): Item 5 has 0 rows but
    ## longest item has 5; filled with NA

    ## Warning in as.data.table.list(jval, .named = NULL): Item 6 has 2 rows but
    ## longest item has 5; recycled with remainder.

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    ##    stats.V1 stats.V2  n  conf.V1  conf.V2 out group names
    ## 1:    6.380   10.750 31 50.21682 41.13401  NA    NA     1
    ## 2:   40.710   39.185 31 70.62318 63.64599  NA    NA     2
    ## 3:   60.420   52.390 31 50.21682 41.13401  NA    NA     1
    ## 4:   76.665   78.850 31 70.62318 63.64599  NA    NA     2
    ## 5:   96.980   95.670 31 50.21682 41.13401  NA    NA     1

``` r
trend[ , mean(time1, na.rm=TRUE)]
```

    ## [1] 56.58677

``` r
trend[ , mean(time2, na.rm=TRUE)]
```

    ## [1] 56.48194

``` r
trend %>%
  summarize(mean(time1),  sd(time1), mean(time2), sd(time2))
```

    ##   mean(time1) sd(time1) mean(time2) sd(time2)
    ## 1    56.58677  22.35656    56.48194  24.42457

check normality

``` r
trend[, hist(time1)]
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## $breaks
    ## [1]   0  20  40  60  80 100
    ## 
    ## $counts
    ## [1]  1  7  7 12  4
    ## 
    ## $density
    ## [1] 0.001612903 0.011290323 0.011290323 0.019354839 0.006451613
    ## 
    ## $mids
    ## [1] 10 30 50 70 90
    ## 
    ## $xname
    ## [1] "time1"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
shapiro.test(trend$time1)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  trend$time1
    ## W = 0.97723, p-value = 0.7322

``` r
#time1 normally distributed


trend[, hist(time2)]
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

    ## $breaks
    ## [1]   0  20  40  60  80 100
    ## 
    ## $counts
    ## [1] 2 6 8 7 8
    ## 
    ## $density
    ## [1] 0.003225806 0.009677419 0.012903226 0.011290323 0.012903226
    ## 
    ## $mids
    ## [1] 10 30 50 70 90
    ## 
    ## $xname
    ## [1] "time2"
    ## 
    ## $equidist
    ## [1] TRUE
    ## 
    ## attr(,"class")
    ## [1] "histogram"

``` r
shapiro.test(trend$time2)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  trend$time2
    ## W = 0.96195, p-value = 0.3282

``` r
#time1 and 2 normally distributed therefore can use a t test
```

``` r
var.test(trend$time1, trend$time2, alternative= "two.sided")
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  trend$time1 and trend$time2
    ## F = 0.83783, num df = 30, denom df = 30, p-value = 0.6311
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.4039794 1.7376136
    ## sample estimates:
    ## ratio of variances 
    ##          0.8378307

``` r
#ftest pvalue >0.05 therefore variances equal
```

\#paired t.test (variables both normally distributed and equal
variances)

``` r
#paired t-test as same countries

trend[, t.test(time1, time2, paired= TRUE, var.equal=TRUE)]
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  time1 and time2
    ## t = 0.10402, df = 30, p-value = 0.9178
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.953541  2.163218
    ## sample estimates:
    ## mean of the differences 
    ##               0.1048387

``` r
glimpse(trend)
```

    ## Rows: 31
    ## Columns: 3
    ## $ country <chr> "Argentina", "Australia", "Brazil", "Chile", "China", "Cyprus"…
    ## $ time1   <dbl> 42.22, 27.20, 53.63, 60.94, 65.20, 51.13, 78.93, 31.13, 87.21,…
    ## $ time2   <dbl> 42.49, 23.00, 52.39, 42.20, 64.42, 49.44, 77.12, 33.07, 92.69,…

``` r
#no sig difference
```

\#boxplot expermimentation

``` r
trendbox <- trend
names(trendbox) <- c("country", "2005-2009", "2010-2014")

library(reshape) #for ggplot need to melt the data so all numberic in one column
```

    ## 
    ## Attaching package: 'reshape'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     rename

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, smiths

    ## The following object is masked from 'package:data.table':
    ## 
    ##     melt

``` r
melt(trendbox)
```

    ## Using country as id variables

    ##                country  variable value
    ## 1            Argentina 2005-2009 42.22
    ## 2            Australia 2005-2009 27.20
    ## 3               Brazil 2005-2009 53.63
    ## 4                Chile 2005-2009 60.94
    ## 5                China 2005-2009 65.20
    ## 6               Cyprus 2005-2009 51.13
    ## 7              Georgia 2005-2009 78.93
    ## 8              Germany 2005-2009 31.13
    ## 9                Ghana 2005-2009 87.21
    ## 10               India 2005-2009 75.91
    ## 11               Japan 2005-2009 45.80
    ## 12              Jordan 2005-2009 96.98
    ## 13 Korea (Republic of) 2005-2009 65.54
    ## 14            Malaysia 2005-2009 87.42
    ## 15              Mexico 2005-2009 49.81
    ## 16             Morocco 2005-2009 77.42
    ## 17         Netherlands 2005-2009 22.47
    ## 18              Poland 2005-2009 50.36
    ## 19             Romania 2005-2009 60.42
    ## 20  Russian Federation 2005-2009 63.30
    ## 21              Rwanda 2005-2009 79.76
    ## 22            Slovenia 2005-2009 33.17
    ## 23        South Africa 2005-2009 71.69
    ## 24               Spain 2005-2009 25.39
    ## 25              Sweden 2005-2009  6.38
    ## 26            Thailand 2005-2009 81.29
    ## 27 Trinidad and Tobago 2005-2009 48.40
    ## 28              Turkey 2005-2009 77.57
    ## 29             Ukraine 2005-2009 64.58
    ## 30       United States 2005-2009 33.74
    ## 31             Uruguay 2005-2009 39.20
    ## 32           Argentina 2010-2014 42.49
    ## 33           Australia 2010-2014 23.00
    ## 34              Brazil 2010-2014 52.39
    ## 35               Chile 2010-2014 42.20
    ## 36               China 2010-2014 64.42
    ## 37              Cyprus 2010-2014 49.44
    ## 38             Georgia 2010-2014 77.12
    ## 39             Germany 2010-2014 33.07
    ## 40               Ghana 2010-2014 92.69
    ## 41               India 2010-2014 83.25
    ## 42               Japan 2010-2014 41.67
    ## 43              Jordan 2010-2014 95.67
    ## 44 Korea (Republic of) 2010-2014 62.91
    ## 45            Malaysia 2010-2014 88.38
    ## 46              Mexico 2010-2014 51.00
    ## 47             Morocco 2010-2014 80.58
    ## 48         Netherlands 2010-2014 15.88
    ## 49              Poland 2010-2014 47.31
    ## 50             Romania 2010-2014 60.84
    ## 51  Russian Federation 2010-2014 68.56
    ## 52              Rwanda 2010-2014 89.39
    ## 53            Slovenia 2010-2014 28.25
    ## 54        South Africa 2010-2014 80.90
    ## 55               Spain 2010-2014 25.16
    ## 56              Sweden 2010-2014 10.75
    ## 57            Thailand 2010-2014 74.50
    ## 58 Trinidad and Tobago 2010-2014 51.25
    ## 59              Turkey 2010-2014 85.70
    ## 60             Ukraine 2010-2014 65.40
    ## 61       United States 2010-2014 30.07
    ## 62             Uruguay 2010-2014 36.70

``` r
trendboxmelt <- reshape::melt(trendbox) #code in one line
```

    ## Using country as id variables

``` r
glimpse(trendboxmelt)
```

    ## Rows: 62
    ## Columns: 3
    ## $ country  <chr> "Argentina", "Australia", "Brazil", "Chile", "China", "Cyprus…
    ## $ variable <fct> 2005-2009, 2005-2009, 2005-2009, 2005-2009, 2005-2009, 2005-2…
    ## $ value    <dbl> 42.22, 27.20, 53.63, 60.94, 65.20, 51.13, 78.93, 31.13, 87.21…

``` r
b <- ggplot(trendboxmelt, aes(x=variable, y = value, color = variable)) +
         geom_boxplot(notch=FALSE) + #if notch=TRUE and notches don't overlap good chance medians differ (notches display the confidence intervals around the medians)
stat_summary(fun= mean, geom = "point", shape = 23, size = 15) + #displayes the means as a diamond
geom_jitter(shape=16, size = 1, position = position_jitter(0.1)) +#adds in points, the jitter randomly varies the position along x axis
labs(title = "GSNI values by time period", x = "GSNI collection time period", y= "GSNI value (2 bias)")+
theme(legend.position = "none") #remove legend
b
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#n=31
```

``` r
#customise colours

b+scale_color_manual(values= c('Purple4', 'DarkSalmon'))
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#to fill box with colour need to change code at the beginning
ggplot(trendboxmelt, aes(x=variable, y = value, fill = variable)) +
         geom_boxplot(notch=FALSE) +
stat_summary(fun= mean, geom = "point", shape = 23, size = 15) + 
geom_jitter(shape=16, size = 1, position = position_jitter(0.1))+
  scale_fill_brewer(palette="Dark2")+#personalise colours, can also use scale_fill_manual as above
theme(legend.position = "none")
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
ggplot(trendboxmelt, aes(x=variable, y = value, fill = variable)) +
         geom_boxplot(notch=FALSE) +
stat_summary(fun= mean, geom = "point", shape = 23, size = 15) + 
geom_jitter(shape=16, size = 1, position = position_jitter(0.1))+
  scale_fill_grey() + theme_classic()#grey scale
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
ggplot(trendboxmelt, aes(x=variable, y = value, fill = country)) +
         geom_boxplot() #doesnt work with this data but say had two groups of countries could use the fill = to separate them in the box plot
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
trendboxmelt %>%
  group_by(variable) %>%
summarize(median(value))
```

    ## # A tibble: 2 x 2
    ##   variable  `median(value)`
    ##   <fct>               <dbl>
    ## 1 2005-2009            60.4
    ## 2 2010-2014            52.4

## Compare all countries that were collected at each time period n = 75

``` r
ggplot(results14, aes(x= GSNI_PERIOD, y = twobias, color = GSNI_PERIOD)) +
         geom_boxplot(notch=FALSE) + #if notch=TRUE and notches don't overlap good chance medians differ (notches display the confidence intervals around the medians)
stat_summary(fun= mean, geom = "point", shape = 23, size = 15) + #displayes the means as a diamond
geom_jitter(shape=16, size = 1, position = position_jitter(0.1)) +#adds in points, the jitter randomly varies the position along x axis
labs(title = "GSNI values by time period", x = "GSNI collection time period", y= "GSNI2 (%)")+
theme(legend.position = "none") 
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#n=75
```

check assumptions

``` r
p1 <- results14[GSNI_PERIOD == "2005–2009", twobias]
hist(p1)
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
shapiro.test(p1) #evidence that normally distributed
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  p1
    ## W = 0.90869, p-value = 0.08158

``` r
p2 <- results14[GSNI_PERIOD == "2010–2014", twobias]
hist(p2)
```

![](GSNI_trend_analysis_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
shapiro.test(p2) #evidence that not normally distributed 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  p2
    ## W = 0.90727, p-value = 0.000352

``` r
wilcox.test(p1,p2,paired=FALSE
            )
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  p1 and p2
    ## W = 320, p-value = 0.01694
    ## alternative hypothesis: true location shift is not equal to 0

``` r
#pvalue <0.05 therefore the groups differ significantly, collection in 2010-20014 has a stat sig higher GSNI value (different countries)
```
