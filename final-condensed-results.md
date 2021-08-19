final-condensed-results
================

## Equations

\[
Y_{femcvd} = \beta_0 + \beta_1X_{gsni} + \beta_2X_{physper1000} + \beta_3X_{meanyearsschooling} + \beta_4X_{gdp} + \beta_5X_{datacollectionperiod} + \epsilon
\]

\[
Y_{cvd} = \beta_0 + \beta_1X_{gsni} + \beta_2X_{physper1000} + \beta_3X_{meanyearsschooling} + \beta_4X_{gdp} + \beta_5X_{datacollectionperiod} + \epsilon
\]

\[
Y_{LE} = \beta_0 + \beta_1X_{gsni} + log(\beta_2X_{physper1000}) + \beta_3X_{meanyearsschooling} + log(\beta_4X_{gdp}) + \beta_5X_{datacollectionperiod} + \beta_6X_{mmr} + \epsilon
\]

**H\_0** \[
\beta_1= 0
\]

**H\_1** \[
\beta_1\ne 0
\]

## Results

# SUMMARY STATISTICS

| Summary Statistics                                                      | n = 75                             |
| :---------------------------------------------------------------------- | :--------------------------------- |
| **Female CVD age adjusted mortality rate (per 100 000 population)**     |                                    |
| min                                                                     | 62.03                              |
| max                                                                     | 1024.86                            |
| median (IQR)                                                            | 241.30 (128.06, 345.30)            |
| **Population CVD age adjusted mortality rate (per 100 000 population)** |                                    |
| min                                                                     | 82.57                              |
| max                                                                     | 1107.01                            |
| median (IQR)                                                            | 262.98 (155.95, 372.59)            |
| **Female life expectancy at birth**                                     |                                    |
| min                                                                     | 61                                 |
| max                                                                     | 86.4                               |
| median (IQR)                                                            | 78.60 (74.45, 82.60)               |
| **GSNI value (2 or more biases)**                                       |                                    |
| min                                                                     | 7.43                               |
| max                                                                     | 98.07                              |
| median (IQR)                                                            | 68.56 (41.94, 86.33)               |
| **Year of Index Collection**                                            |                                    |
| 2005-2009                                                               | 18 (24)                            |
| 2010-2014                                                               | 57 (76)                            |
| **Physicians per 1000 population**                                      |                                    |
| min                                                                     | 0.05                               |
| max                                                                     | 5.01                               |
| median (IQR)                                                            | 71; 2.30 (1.29, 3.22)              |
| **GDP per capita**                                                      |                                    |
| min                                                                     | 566.93                             |
| max                                                                     | 97019.18                           |
| median (IQR)                                                            | 68; 9,181.10 (4,094.72, 31,623.52) |
| **Mean years of schooling**                                             |                                    |
| min                                                                     | 1.4                                |
| max                                                                     | 14                                 |
| median (IQR)                                                            | 69; 10.10 (7.60, 11.80)            |
| **Maternal Mortality Rate**                                             |                                    |
| min                                                                     | 2                                  |
| max                                                                     | 943                                |
| median (IQR)                                                            | 70; 25.50 (10.25, 82.75)           |

\#box plots

boxplot and t-test to compare trend of GSNI over collection periods

Compared the GSNI values per time period for countries with both values
(boxplot below), paired t-test no stat sig difference between time
periods - indicating that the norms do not change quickly over time

![](final-condensed-results_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

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

Compare values from the 2 collection points Mann whitney U test shows
sig difference in value of the 2 collection periods, this may be due to
countries taken at each time point, but may be due to the survey itself,
therefore controlling for the time period of collection seems
reasonable. eg there might be something which has pushed up the GSNI in
the second collection and these countries also happen to have higher CVD
rates which would change the relationship

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  p1 and p2
    ## W = 320, p-value = 0.01694
    ## alternative hypothesis: true location shift is not equal to 0

``` r
boxes <- plot_grid(box1, box2, labels="AUTO")
boxes
```

![](final-condensed-results_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave("boxresults.png", boxes)
```

    ## Saving 7 x 5 in image

\#scatter plots

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  twobias and cvd14fem
    ## S = 23488, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##      rho 
    ## 0.665889

positive correlation between GSNI and female CVD mortality rates (rho
0.67 p\<0.05)

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  twobias and cvd14both
    ## S = 28216, p-value = 2.801e-08
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.5986344

positive correlation between GSNI and female CVD mortality rates (rho
0.60 p\<0.05)

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  twobias and LEbirth2015
    ## S = 121044, p-value = 2.734e-13
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.7218176

See a negative correlation with female life expectancy and GSNI (rho
-0.72, p-value \<0.05)

``` r
scatterresults <- plot_grid(scatter1, scatter2, scatter3, labels="AUTO")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

``` r
ggsave("scatterresults.png", scatterresults, width = 28, height = 25, units = "cm")
```

\#regression models

<table style="text-align:center">

<caption>

<strong>Results table 1</strong>

</caption>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="4">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

female CVD mortality

</td>

<td>

population CVD mortality

</td>

<td>

female CVD mortality

</td>

<td>

population CVD mortality

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

<td>

(4)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

constant

</td>

<td>

30.04 (39.25)

</td>

<td>

65.58 (43.71)

</td>

<td>

\-133.15 (94.46)

</td>

<td>

\-140.69 (95.92)

</td>

</tr>

<tr>

<td style="text-align:left">

GSNI2

</td>

<td>

3.63<sup>\*\*\*</sup> (0.58)

</td>

<td>

3.57<sup>\*\*\*</sup> (0.64)

</td>

<td>

4.86<sup>\*\*\*</sup> (0.84)

</td>

<td>

4.89<sup>\*\*\*</sup> (0.85)

</td>

</tr>

<tr>

<td style="text-align:left">

physicians per 1000

</td>

<td>

</td>

<td>

</td>

<td>

32.31 (19.26)

</td>

<td>

47.45<sup>\*</sup> (19.55)

</td>

</tr>

<tr>

<td style="text-align:left">

mean years of schooling

</td>

<td>

</td>

<td>

</td>

<td>

13.75 (9.37)

</td>

<td>

17.25 (9.52)

</td>

</tr>

<tr>

<td style="text-align:left">

GDP per capita

</td>

<td>

</td>

<td>

</td>

<td>

\-0.002<sup>\*</sup> (0.001)

</td>

<td>

\-0.003<sup>\*\*</sup> (0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

GSNI data collection period 2010-2014

</td>

<td>

</td>

<td>

</td>

<td>

\-97.89<sup>\*</sup> (40.74)

</td>

<td>

\-110.79<sup>\*\*</sup> (41.36)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.35

</td>

<td>

0.30

</td>

<td>

0.49

</td>

<td>

0.53

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.34

</td>

<td>

0.29

</td>

<td>

0.44

</td>

<td>

0.49

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

39.68<sup>\*\*\*</sup> (df = 1; 73)

</td>

<td>

30.97<sup>\*\*\*</sup> (df = 1; 73)

</td>

<td>

11.54<sup>\*\*\*</sup> (df = 5; 61)

</td>

<td>

13.54<sup>\*\*\*</sup> (df = 5; 61)

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="4" style="text-align:right">

  - p\<0.05; \*\* p\<0.01; \*\*\* p\<0.001
    </td>
    </tr>
    </table>

for every 1% increase in percentage of population with 2 or more gender
biases, age adjusted female CVD mortality rate increases by 4.86, and
population CVD mortality rate increases by 4.89. That is controlling for
physicians per 1000, education, GDP and GSNI collection period.

<table style="text-align:center">

<caption>

<strong>Results table 2</strong>

</caption>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="2">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="2">

female life expectancy

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

constant

</td>

<td>

87.31<sup>\*\*\*</sup> (1.46)

</td>

<td>

68.74<sup>\*\*\*</sup> (4.82)

</td>

</tr>

<tr>

<td style="text-align:left">

GSNI

</td>

<td>

\-0.16<sup>\*\*\*</sup> (0.02)

</td>

<td>

\-0.07<sup>\*\*\*</sup> (0.02)

</td>

</tr>

<tr>

<td style="text-align:left">

log (physicians per 1000)

</td>

<td>

</td>

<td>

1.27<sup>\*</sup> (0.61)

</td>

</tr>

<tr>

<td style="text-align:left">

mean years of schooling

</td>

<td>

</td>

<td>

\-0.41 (0.21)

</td>

</tr>

<tr>

<td style="text-align:left">

log (GDP per capita)

</td>

<td>

</td>

<td>

1.82<sup>\*\*\*</sup> (0.46)

</td>

</tr>

<tr>

<td style="text-align:left">

GSNI data collection period 2010-2014

</td>

<td>

</td>

<td>

1.30 (0.91)

</td>

</tr>

<tr>

<td style="text-align:left">

MMR

</td>

<td>

</td>

<td>

\-0.01<sup>\*\*\*</sup> (0.003)

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.43

</td>

<td>

0.86

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.42

</td>

<td>

0.84

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

55.39<sup>\*\*\*</sup> (df = 1; 73)

</td>

<td>

60.02<sup>\*\*\*</sup> (df = 6; 59)

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="2" style="text-align:right">

  - p\<0.05; \*\* p\<0.01; \*\*\* p\<0.001
    </td>
    </tr>
    </table>

For every 1% increase in percentage of population with 2 or more gender
biases, female life expectancy decreases by 0.072 years (26 days) when
controlling for physicians per 1000, GDP, education, GSNI collection
period and MMR

#### Sensitivity analysis

Analysis repeated with most recently available data (2017/19)

Outcome 1&2

<table style="text-align:center">

<caption>

<strong>Results table 3</strong>

</caption>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="2">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

female CVD mortality

</td>

<td>

population CVD mortality

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

constant

</td>

<td>

\-118.15 (91.70)

</td>

<td>

\-132.22 (93.98)

</td>

</tr>

<tr>

<td style="text-align:left">

GSNI

</td>

<td>

4.27<sup>\*\*\*</sup> (0.85)

</td>

<td>

4.36<sup>\*\*\*</sup> (0.87)

</td>

</tr>

<tr>

<td style="text-align:left">

physicians per 1000

</td>

<td>

8.39 (15.71)

</td>

<td>

22.02 (16.10)

</td>

</tr>

<tr>

<td style="text-align:left">

mean years of schooling

</td>

<td>

20.53<sup>\*</sup> (8.80)

</td>

<td>

23.92<sup>\*</sup> (9.01)

</td>

</tr>

<tr>

<td style="text-align:left">

GDP per capita

</td>

<td>

\-0.002<sup>\*</sup> (0.001)

</td>

<td>

\-0.003<sup>\*\*</sup> (0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

GSNI data collection period 2010-2014

</td>

<td>

\-90.81<sup>\*</sup> (38.64)

</td>

<td>

\-101.63<sup>\*</sup> (39.59)

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.49

</td>

<td>

0.53

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.45

</td>

<td>

0.49

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic (df = 5; 61)

</td>

<td>

11.80<sup>\*\*\*</sup>

</td>

<td>

13.60<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="2" style="text-align:right">

  - p\<0.05; \*\* p\<0.01; \*\*\* p\<0.001
    </td>
    </tr>
    </table>

Outcome 3

<table style="text-align:center">

<caption>

<strong>Results table 4</strong>

</caption>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

female life expectancy

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

constant

</td>

<td>

60.75<sup>\*\*\*</sup> (4.23)

</td>

</tr>

<tr>

<td style="text-align:left">

GSNI

</td>

<td>

\-0.04<sup>\*</sup> (0.02)

</td>

</tr>

<tr>

<td style="text-align:left">

log (physicians per 1000)

</td>

<td>

0.92<sup>\*\*</sup> (0.28)

</td>

</tr>

<tr>

<td style="text-align:left">

mean years of schooling

</td>

<td>

\-0.29 (0.17)

</td>

</tr>

<tr>

<td style="text-align:left">

log (GDP per capita)

</td>

<td>

2.46<sup>\*\*\*</sup> (0.43)

</td>

</tr>

<tr>

<td style="text-align:left">

GSNI data collection period 2010-2014

</td>

<td>

1.32 (0.78)

</td>

</tr>

<tr>

<td style="text-align:left">

MMR

</td>

<td>

\-0.01<sup>\*\*\*</sup> (0.003)

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.88

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.87

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

70.88<sup>\*\*\*</sup> (df = 6; 59)

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td style="text-align:right">

  - p\<0.05; \*\* p\<0.01; \*\*\* p\<0.001
    </td>
    </tr>
    </table>
