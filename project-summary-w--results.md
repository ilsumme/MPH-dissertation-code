presentation
================

# An investigation into the association between gender social norms, cardiovascular disease and life expectancy

## Background

The population health benefits of gender equity are well recognised.
Clark and Horton are quoted in the Lancet series on the subject saying
There will be no sustainable development without gender equity and that
Gender needs to come of age in global health. However, the scope of
research in this area has had a blinkered focus on subjects that are
generally considered woman’s health issues, like reproductive health and
intimate partner violence.

![](images/lancet.png)

It is cardiovascular disease (CVD) however, that is the leading cause of
death for women and men alike worldwide, and this is my proposed topic
of research.

![GBD 2019 <https://vizhub.healthdata.org/gbd-compare/>](images/GBD.png)

The societal construct of gender is recognised as a social determinant
of health, leading to inequity in CVD outcomes between genders. It is
likely that that harmful gender social norms (the “unspoken rules”
influencing how individuals enact their gender) may be resulting in
increased CVD burden for the entire population.

The question of how gender and social norms might impact on CVD risk has
been explored by O’Neil et al., describing how CVD risk factors such as
physical activity, smoking, drinking and psychological stress are
impacted by societal norms. However, this theoretical model has not been
tested in practice, and an analysis of how these norms might play out at
population level to influence CVD mortality and life expectancy is
lacking. This is the research gap which my study wishes to address.

![Framework of how gender impacts cardiovascular disease. From: O’Neil,
A., Scovelle, A. J., Milner, A. J. & Kavanagh, A. 2018. Gender/Sex as a
Social Determinant of Cardiovascular Risk. Circulation, 137,
854-864.](images/Oneil.png)

The United Nations propose that GSN may be the key to understanding why
the world is not on track to achieving gender equality in line with the
sustainable development goals. They have created a GSN index, measuring
population beliefs on women’s political, educational, economic and
physical place in society. This index does not include measures of
health and therefore allows for a correlation analysis with health
outcomes

![How the gender social norms index is constructed. From: Niessen, L.
W., Mohan, D., Akuoku, J. K., Mirelman, A. J., Ahmed, S., Koehlmoos, T.
P., Trujillo, A., Khan, J. & Peters, D. H. 2018. Tackling socioeconomic
inequalities and non-communicable diseases in low-income and
middle-income countries under the Sustainable Development agenda. The
Lancet, 391, 2036-2046.](images/GSNI.png) \#\# Research Question

What is the relationship between gender social norms and 1) CVD
mortality 2) life expectancy for women, at a country level?

### Aim

Examine the relationship between GSNs (as measured by the UN Gender
Social Norms Index) and 1) CVD mortality and 2) female life expectancy
with use of an ecological study design. Objectives

• Perform a systematic literature review looking at the association
between GSNs and population health.  
• Identify potential confounding factors • Create a conceptual model to
explain a pathway through which GSNs may influence CVD mortality and
life expectancy • Create a regression model to explore the relationship
between the GSNI and 1) female CVD morality 2) population CVD mortality
3) female life expectancy with the country as the unit of analysis •
Make recommendations for future research. Hypothesis

#### H<sub>0</sub> Gender social norms as measured by the Gender Social Norms Index has no association with female/population CVD mortality or female life expectancy at the country level

#### H<sub>1</sub> Gender social norms as measured by the Gender Social Norms Index has an association with female/population CVD mortality and female life expectancy at the country level

## Literature review

The literature review asked the question, what is the impact of unequal
gender social norms on health and gendered health inequalities?

Only 1 study was identified that looked at CVD outcome (stroke) out of
the 97 included. There were some that looked at RFs for CVD like PA,
obestiy, smoking. Majority demonstrated an association with gender
inequality, however many had no or poor use of confounding variables
when using an ecoloical or cross sectional design.

In order to logically hypothesise, visualise and communicate potential
pathways linking gender social norms to CVD and LE and the way these may
interact with potential confounding variables, a conceptual model was
constructed (figure x). The process of creating the model began with
linking the gender social norms index domains to outcomes identified in
the literature review. Confounding variables were identified with help
from the literature review and from wider knowledge about determinants
of population health outcomes.

![](images/Conceptualmodel.png) \#\# Methods

An ecological study design has been chosen with the country as the unit
of analysis. This type of design is appropriate due to the nature of
gender social norms which are a pervasive and societal phenomenon, and
valid to study at the population rather than at an individual level. For
example, it would be feasible to postulate that GSNs held at an
individual level may impact on health, however the GSNs that are held
more widely within the milieu of society may well also have an impact on
that individuals health regardless of their own individual beliefs.
Therefore, measuring GSNs at the country level can provide a more
societal overview of beliefs and their relationship with the health
outcomes of interest. Additionally, the data required for the analysis
is readily available at country level.

#### Statistical plan

  - comparing GSNI values between collection periods to understand how
    quickly GSNs might change and the impact of collection period
  - scatter plots between outcome variables and covariates
  - univariable linear regression
  - multivariable linear regression

#### Outcome variables

1)  Female CVD mortality rate – Female age standardised CVD mortality
    rate per 100 000 population at country level. From Global Burden of
    Disease Data (2019)

2)  Population CVD mortality rate – Age standardised CVD mortality rate
    per 100 000 population at country level. From Global Burden of
    Disease Data (2019)

3)  Female life expectancy – Life expectancy at birth in years. From
    World Health Observatory data.

#### Primary explanatory variable

GSNI (value for percentage of population with 2 or more gender biases)

#### Covariates

  - Economic measure: GDP per capita
  - Healthcare measure: physicians per 1000 population
  - Eduction measure: mean years of schooling
  - period of GSNI data collection
  - Maternal mortality ratio (outcome 3 only) Measures of the economic,
    healthcare and educational situation of each country were included
    as covariates based on the results of the literature review and
    wider evidence of the impact of these variables on population health

## Results

# SUMMARY STATISTICS

n = 75 (limited by GSNI), however for the regression models n = 66-67
due to missing variables

    ## 
    ## 
    ## |Summary Statistics                                                      |n =  75                            |
    ## |:-----------------------------------------------------------------------|:----------------------------------|
    ## |**GSNI index 2 or more bias**                                           |&nbsp;&nbsp;                       |
    ## |&nbsp;&nbsp; min                                                        |7.43                               |
    ## |&nbsp;&nbsp; max                                                        |98.07                              |
    ## |&nbsp;&nbsp; median (IQR)                                               |68.56 (41.94, 86.33)               |
    ## |**Female CVD age adjusted mortality rate (per 100 000 population)**     |&nbsp;&nbsp;                       |
    ## |&nbsp;&nbsp; min                                                        |62.03                              |
    ## |&nbsp;&nbsp; max                                                        |1024.86                            |
    ## |&nbsp;&nbsp; median (IQR)                                               |241.30 (128.06, 345.30)            |
    ## |**Population CVD age adjusted mortality rate (per 100 000 population)** |&nbsp;&nbsp;                       |
    ## |&nbsp;&nbsp; min                                                        |82.57                              |
    ## |&nbsp;&nbsp; max                                                        |1107.01                            |
    ## |&nbsp;&nbsp; median (IQR)                                               |262.98 (155.95, 372.59)            |
    ## |**Female life expectancy at birth**                                     |&nbsp;&nbsp;                       |
    ## |&nbsp;&nbsp; min                                                        |61                                 |
    ## |&nbsp;&nbsp; max                                                        |86.4                               |
    ## |&nbsp;&nbsp; median (IQR)                                               |78.60 (74.45, 82.60)               |
    ## |**Physicians per 1000 population**                                      |&nbsp;&nbsp;                       |
    ## |&nbsp;&nbsp; min                                                        |0.05                               |
    ## |&nbsp;&nbsp; max                                                        |5.01                               |
    ## |&nbsp;&nbsp; median (IQR)                                               |71; 2.30 (1.29, 3.22)              |
    ## |**GDP per capita**                                                      |&nbsp;&nbsp;                       |
    ## |&nbsp;&nbsp; min                                                        |566.93                             |
    ## |&nbsp;&nbsp; max                                                        |97019.18                           |
    ## |&nbsp;&nbsp; median (IQR)                                               |68; 9,181.10 (4,094.72, 31,623.52) |
    ## |**Mean years of schooling**                                             |&nbsp;&nbsp;                       |
    ## |&nbsp;&nbsp; min                                                        |1.4                                |
    ## |&nbsp;&nbsp; max                                                        |14                                 |
    ## |&nbsp;&nbsp; median (IQR)                                               |69; 10.10 (7.60, 11.80)            |
    ## |**Maternal Mortality Rate**                                             |&nbsp;&nbsp;                       |
    ## |&nbsp;&nbsp; min                                                        |2                                  |
    ## |&nbsp;&nbsp; max                                                        |943                                |
    ## |&nbsp;&nbsp; median (IQR)                                               |70; 25.50 (10.25, 82.75)           |
    ## |**Year of Index Collection**                                            |&nbsp;&nbsp;                       |
    ## |&nbsp;&nbsp; 2005-2009                                                  |18 (24)                            |
    ## |&nbsp;&nbsp; 2010-2014                                                  |57 (76)                            |

# COMPARING GSNI VALUES OVER TIME/ OVER COLLECTION PERIOD

Compared the GSNI values per time period for countries with both values
(boxplot below), paired t-test no stat sig difference between time
periods - indicating that the norms do not change quickly over time

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

\#\#\#\#compare values from the 2 collection points Mann whitney U test
shows sig difference in value of the 2 collection periods, this may be
due to countries taken at each time point, but may be due to the survey
itself, therefore controlling for the time period of collection seems
reasonable. eg there might be something which has pushed up the GSNI in
the second collection and these countries also happen to have higher CVD
rates which would change the relationship

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# OUTCOME 1 - FEMALE CVD MORTALITY

## Scatter plot female CVD mortality (2014) vs GSNI

positive correlation between GSNI and female CVD mortality rates (rho
0.69 p\<0.05)
![](project-summary-w--results_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Univariable linear regression female CVD mortalty (2014) \~ GNSI

    ## 
    ## Call:
    ## lm(formula = cvd14fem ~ twobias, data = results14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -232.53  -69.48  -23.07   40.46  678.44 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  31.6231    40.6923   0.777     0.44    
    ## twobias       3.5883     0.6106   5.877 1.57e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 134.9 on 65 degrees of freedom
    ## Multiple R-squared:  0.347,  Adjusted R-squared:  0.337 
    ## F-statistic: 34.54 on 1 and 65 DF,  p-value: 1.573e-07

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |   31.623 |    40.692 |     0.777 |    0.44 |
| twobias     |    3.588 |     0.611 |     5.877 |    0.00 |

## Multivariable linear regression female CVD mortalty (2014) as outcome

for every 1% increase in percentage of population with 2 or more gender
biases, age adjusted female CVD mortality rate increases by 4.86, that
is controlling for physicians per 1000, education, GDP and GSNI
collection period.

| term                  |  estimate | std.error | statistic | p.value |
| :-------------------- | --------: | --------: | --------: | ------: |
| (Intercept)           | \-133.151 |    94.460 |   \-1.410 |   0.164 |
| twobias               |     4.863 |     0.841 |     5.780 |   0.000 |
| phy14edit             |    32.308 |    19.257 |     1.678 |   0.099 |
| scl14                 |    13.753 |     9.372 |     1.468 |   0.147 |
| GDP14                 |   \-0.002 |     0.001 |   \-2.086 |   0.041 |
| GSNI\_PERIOD2010–2014 |  \-97.889 |    40.737 |   \-2.403 |   0.019 |

# OUTCOME 2 - POPULATION CVD MORTALITY

## Scatter plot CVD mortality (2014) against GSNI

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Univariable linear regression CVD mortality (2014) \~ GSNI

    ## 
    ## Call:
    ## lm(formula = cvd14both ~ twobias, data = results14noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -215.62  -82.15  -23.44   40.44  734.51 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  67.2389    44.8028   1.501    0.138    
    ## twobias       3.4795     0.6722   5.176 2.37e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 148.5 on 65 degrees of freedom
    ## Multiple R-squared:  0.2919, Adjusted R-squared:  0.281 
    ## F-statistic: 26.79 on 1 and 65 DF,  p-value: 2.372e-06

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |   67.239 |    44.803 |     1.501 |   0.138 |
| twobias     |    3.480 |     0.672 |     5.176 |   0.000 |

## Multivariable regression with CVD mortality (2014) as outcome

Similar result is seen with population CVD mortality, which supports the
hypothesis that gender biases can impact negatively on the entire
population, not only women.

| term                  |  estimate | std.error | statistic | p.value |
| :-------------------- | --------: | --------: | --------: | ------: |
| (Intercept)           | \-140.694 |    95.915 |   \-1.467 |   0.148 |
| twobias               |     4.893 |     0.854 |     5.728 |   0.000 |
| phy14edit             |    47.449 |    19.554 |     2.427 |   0.018 |
| scl14                 |    17.253 |     9.516 |     1.813 |   0.075 |
| GDP14                 |   \-0.003 |     0.001 |   \-3.228 |   0.002 |
| GSNI\_PERIOD2010–2014 | \-110.793 |    41.364 |   \-2.678 |   0.009 |

# OUTCOME 3 - Female life expectancy at birth

## Scatter plot female life expectancy (2014) against GSNI

See a negative correlation with female life expectancy and GSNI

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Univariable linear regression Female LE (2014) \~ GSNI

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ twobias, data = results14noNA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.9873  -1.6177   0.6053   2.9145  11.5423 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 87.39802    1.50957  57.896  < 2e-16 ***
    ## twobias     -0.16536    0.02265  -7.301 5.09e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.004 on 65 degrees of freedom
    ## Multiple R-squared:  0.4505, Adjusted R-squared:  0.4421 
    ## F-statistic:  53.3 on 1 and 65 DF,  p-value: 5.094e-10

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |   87.398 |     1.510 |    57.896 |       0 |
| twobias     |  \-0.165 |     0.023 |   \-7.301 |       0 |

## Multivariable linear regression with Female life expectancy (2014) as outcome

For every 1 increase in percentage of population with 2 or more gender
biases, female life expectancy decreases by 0.072 years (26 days) when
controlling for physicians per 1000, GDP, education, GSNI collection
period and MMR

without MMR: r2 = 0.78 cAIC: 345

with MMR: r2 = 0.84 cAIC: 326

| term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           |   64.895 |     5.538 |    11.718 |   0.000 |
| twobias               |  \-0.072 |     0.022 |   \-3.337 |   0.001 |
| log(phy14edit)        |    2.780 |     0.609 |     4.567 |   0.000 |
| scl14                 |  \-0.378 |     0.249 |   \-1.517 |   0.134 |
| log(GDP14)            |    1.995 |     0.536 |     3.720 |   0.000 |
| GSNI\_PERIOD2010–2014 |    1.271 |     1.060 |     1.199 |   0.235 |

| term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           |   68.742 |     4.816 |    14.274 |   0.000 |
| twobias               |  \-0.072 |     0.019 |   \-3.867 |   0.000 |
| log(phy14edit)        |    1.274 |     0.610 |     2.086 |   0.041 |
| scl14                 |  \-0.407 |     0.213 |   \-1.905 |   0.062 |
| log(GDP14)            |    1.820 |     0.461 |     3.945 |   0.000 |
| GSNI\_PERIOD2010–2014 |    1.300 |     0.909 |     1.431 |   0.158 |
| MMR14                 |  \-0.015 |     0.003 |   \-4.760 |   0.000 |

## Some notes about the models

#### log transforming variables for the LE outcome

For the LE outcome, reiduals vs fitted did not appear linear

    ## 
    ## Call:
    ## lm(formula = LEbirth2015 ~ twobias + phy14edit + scl14 + GDP14 + 
    ##     GSNI_PERIOD + MMR14, data = results14MMRnoNA)
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
    ## GDP14                 5.056e-05  2.149e-05   2.352   0.0220 *  
    ## GSNI_PERIOD2010–2014  2.410e+00  1.014e+00   2.377   0.0207 *  
    ## MMR14                -2.043e-02  3.060e-03  -6.678 9.46e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.009 on 59 degrees of freedom
    ## Multiple R-squared:  0.8161, Adjusted R-squared:  0.7974 
    ## F-statistic: 43.64 on 6 and 59 DF,  p-value: < 2.2e-16

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Plotted regression terms of the model, GDP and phys per 1000 look like
they might fit better as a log (can also be seen in the scatter plots)

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->

With these variables log transformed the diagnostic looks a bit better

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

Additionally, there is an outlier in this model, but removing it does
not change the resutls
![](project-summary-w--results_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

    ##    location_name GSNI_PERIOD twobias cvd14fem cvd14both LEbirth2015 phy14edit
    ## 1:       Nigeria   2010–2014   94.99 249.7113  246.8299        62.8    0.3828
    ## 2:          Mali   2005–2009   93.36 307.2377  279.7339        61.4    0.1395
    ##    scl14     GDP14 MMR14
    ## 1:   5.9 3098.9863   943
    ## 2:   2.3  848.2741   642

| term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           |   71.890 |     5.038 |    14.271 |   0.000 |
| twobias               |  \-0.076 |     0.018 |   \-4.154 |   0.000 |
| log(phy14edit)        |    0.552 |     0.720 |     0.766 |   0.447 |
| scl14                 |  \-0.395 |     0.210 |   \-1.886 |   0.064 |
| log(GDP14)            |    1.611 |     0.467 |     3.449 |   0.001 |
| GSNI\_PERIOD2010–2014 |    1.167 |     0.895 |     1.304 |   0.198 |
| MMR14                 |  \-0.023 |     0.006 |   \-4.187 |   0.000 |

| term                  | estimate | std.error | statistic | p.value | term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: | :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           |   68.742 |     4.816 |    14.274 |   0.000 | (Intercept)           |   71.890 |     5.038 |    14.271 |   0.000 |
| twobias               |  \-0.072 |     0.019 |   \-3.867 |   0.000 | twobias               |  \-0.076 |     0.018 |   \-4.154 |   0.000 |
| log(phy14edit)        |    1.274 |     0.610 |     2.086 |   0.041 | log(phy14edit)        |    0.552 |     0.720 |     0.766 |   0.447 |
| scl14                 |  \-0.407 |     0.213 |   \-1.905 |   0.062 | scl14                 |  \-0.395 |     0.210 |   \-1.886 |   0.064 |
| log(GDP14)            |    1.820 |     0.461 |     3.945 |   0.000 | log(GDP14)            |    1.611 |     0.467 |     3.449 |   0.001 |
| GSNI\_PERIOD2010–2014 |    1.300 |     0.909 |     1.431 |   0.158 | GSNI\_PERIOD2010–2014 |    1.167 |     0.895 |     1.304 |   0.198 |
| MMR14                 |  \-0.015 |     0.003 |   \-4.760 |   0.000 | MMR14                 |  \-0.023 |     0.006 |   \-4.187 |   0.000 |

Results are similar Diagnostic plots are similar anything else I should
use to compare?

#### Sensitivity analysis Comparing results from 2014 to data from 2017

Outcome 1 (female cvd mort)

    ## 
    ## Call:
    ## lm(formula = cvd17fem ~ twobias + phy17edit + scl17 + GDP17 + 
    ##     GSNI_PERIOD, data = results17noNA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -170.64  -72.36  -19.15   41.65  528.48 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -1.182e+02  9.170e+01  -1.288   0.2025    
    ## twobias               4.266e+00  8.515e-01   5.010 4.95e-06 ***
    ## phy17edit             8.391e+00  1.571e+01   0.534   0.5951    
    ## scl17                 2.053e+01  8.796e+00   2.334   0.0229 *  
    ## GDP17                -2.445e-03  1.042e-03  -2.346   0.0222 *  
    ## GSNI_PERIOD2010–2014 -9.081e+01  3.864e+01  -2.350   0.0220 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 117.5 on 61 degrees of freedom
    ## Multiple R-squared:  0.4917, Adjusted R-squared:  0.4501 
    ## F-statistic:  11.8 on 5 and 61 DF,  p-value: 5.296e-08

| term                  | estimate | std.error | statistic | p.value | term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: | :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           | \-133.15 |     94.46 |    \-1.41 |    0.16 | (Intercept)           | \-118.15 |     91.70 |    \-1.29 |    0.20 |
| twobias               |     4.86 |      0.84 |      5.78 |    0.00 | twobias               |     4.27 |      0.85 |      5.01 |    0.00 |
| phy14edit             |    32.31 |     19.26 |      1.68 |    0.10 | phy17edit             |     8.39 |     15.71 |      0.53 |    0.60 |
| scl14                 |    13.75 |      9.37 |      1.47 |    0.15 | scl17                 |    20.53 |      8.80 |      2.33 |    0.02 |
| GDP14                 |     0.00 |      0.00 |    \-2.09 |    0.04 | GDP17                 |     0.00 |      0.00 |    \-2.35 |    0.02 |
| GSNI\_PERIOD2010–2014 |  \-97.89 |     40.74 |    \-2.40 |    0.02 | GSNI\_PERIOD2010–2014 |  \-90.81 |     38.64 |    \-2.35 |    0.02 |

Outcome 2 (cvd mort)

| term                  | estimate | std.error | statistic | p.value | term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: | :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           | \-140.69 |     95.92 |    \-1.47 |    0.15 | (Intercept)           | \-132.22 |     93.98 |    \-1.41 |    0.16 |
| twobias               |     4.89 |      0.85 |      5.73 |    0.00 | twobias               |     4.36 |      0.87 |      5.00 |    0.00 |
| phy14edit             |    47.45 |     19.55 |      2.43 |    0.02 | phy17edit             |    22.02 |     16.10 |      1.37 |    0.18 |
| scl14                 |    17.25 |      9.52 |      1.81 |    0.07 | scl17                 |    23.92 |      9.01 |      2.65 |    0.01 |
| GDP14                 |     0.00 |      0.00 |    \-3.23 |    0.00 | GDP17                 |     0.00 |      0.00 |    \-3.24 |    0.00 |
| GSNI\_PERIOD2010–2014 | \-110.79 |     41.36 |    \-2.68 |    0.01 | GSNI\_PERIOD2010–2014 | \-101.63 |     39.59 |    \-2.57 |    0.01 |

Outcome 3 (LE)

``` r
mf9log <- lm(LEbirth2019 ~ twobias + log(phy17edit) + scl17+log(GDP17)+GSNI_PERIOD+MMR17, data = results17MMRnoNA)
mf9logtidy<- tidy(mf9log)
kable(cbind(mf8logtidy, mf9logtidy), digits = 3)
```

| term                  | estimate | std.error | statistic | p.value | term                  | estimate | std.error | statistic | p.value |
| :-------------------- | -------: | --------: | --------: | ------: | :-------------------- | -------: | --------: | --------: | ------: |
| (Intercept)           |   68.742 |     4.816 |    14.274 |   0.000 | (Intercept)           |   60.747 |     4.231 |    14.357 |   0.000 |
| twobias               |  \-0.072 |     0.019 |   \-3.867 |   0.000 | twobias               |  \-0.039 |     0.018 |   \-2.146 |   0.036 |
| log(phy14edit)        |    1.274 |     0.610 |     2.086 |   0.041 | log(phy17edit)        |    0.923 |     0.275 |     3.356 |   0.001 |
| scl14                 |  \-0.407 |     0.213 |   \-1.905 |   0.062 | scl17                 |  \-0.286 |     0.175 |   \-1.633 |   0.108 |
| log(GDP14)            |    1.820 |     0.461 |     3.945 |   0.000 | log(GDP17)            |    2.459 |     0.432 |     5.691 |   0.000 |
| GSNI\_PERIOD2010–2014 |    1.300 |     0.909 |     1.431 |   0.158 | GSNI\_PERIOD2010–2014 |    1.316 |     0.780 |     1.687 |   0.097 |
| MMR14                 |  \-0.015 |     0.003 |   \-4.760 |   0.000 | MMR17                 |  \-0.014 |     0.003 |   \-5.404 |   0.000 |

## MMR

Was going to include MMR in the CVD outcomes original, with the logic
that if someone died in childbirth they would not die from CVD, however
was struggling to justify this when there are other causes of death eg
neonatal, ID that thought wouldn’t include, and doesn’t change the
results of the coefficients

Additinally, MMR may be on a causal pathway from GSNIs eg with women
having less access to healthcare.

Also, including MMR in the CVD outcomes reduces the r2, for the LE it
increases is

### Model diagnostics

Outcome 1 lm(formula = cvd14fem \~ twobias + phy14edit + scl14 + GDP14 +
GSNI\_PERIOD, data = results14noNA)

    ## Loading required namespace: qqplotr

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->
Outcome 2 lm(formula = cvd14both \~ twobias + phy14edit + scl14 + GDP14
+ GSNI\_PERIOD, data = results14noNA)
![](project-summary-w--results_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
Outcome 3 lm(formula = LEbirth2015 \~ twobias + log(phy14edit) + scl14 +
log(GDP14) + GSNI\_PERIOD + MMR14, data = results14MMRnoNA) note: when
remove outlier (40 = Nigeria) doesn’t change results but pushes
mutlicolinearity \>5

``` r
check_model(mf8log)
```

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
plot(mf2)
```

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-2.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-3.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-4.png)<!-- -->

``` r
plot(mf3)
```

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-5.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-6.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-7.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-8.png)<!-- -->

``` r
plot(mf5)
```

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-9.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-10.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-11.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-12.png)<!-- -->

``` r
plot(mf6)
```

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-13.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-14.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-15.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-16.png)<!-- -->

``` r
plot(mf8log)
```

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-17.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-18.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-19.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-20.png)<!-- -->

``` r
plot(mf9log)
```

![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-21.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-22.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-23.png)<!-- -->![](project-summary-w--results_files/figure-gfm/unnamed-chunk-28-24.png)<!-- -->
