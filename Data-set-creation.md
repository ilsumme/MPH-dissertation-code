Data set creation
================
Iona Lyell
19/08/2021

### load packages

``` r
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
```

## GENDER SOCIAL NORMS INDEX

from: <http://hdr.undp.org/en/gsni> Gender Social Norms Index data
tables (Excel)

``` r
gsni_tables <- read_excel("gsni_tables.xlsx")
```

    ## New names:
    ## * `` -> ...2
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * ...

``` r
gsni_tables <- as.data.table(gsni_tables)

#rename columns
setnames(gsni_tables, "Table A1: Gender Social Norms Index (GSNI), last available period", "location_name")
setnames(gsni_tables, "...2", "GSNI_PERIOD")
setnames(gsni_tables, "...3", "onebias") #share of people with at least 1 bias (%)
setnames(gsni_tables, "...4", "twobias") #share of people with at least 2 biases (%)
setnames(gsni_tables, "...5", "nobias") #share of people with no (%)
setnames(gsni_tables, "...6", "political") #share of people with political bias (%)
setnames(gsni_tables, "...7", "economic") #share of people with economic bias (%)
setnames(gsni_tables, "...8", "educational") #share of people with educational bias (%)
setnames(gsni_tables, "...9", "physical") #share of people with physical bias (%)


gsni_tables <- gsni_tables[8:82]
 #n = 75
```

## CVD MORTALITY RATES PER COUNTRY FOR WOMEN AND OVERALL

from: <http://ghdx.healthdata.org/gbd-results-tool>

Measure: Deaths Age: Age standardized  
Year: 2005-2019 (data collections for GINI 2005-2009 or 2010-2014) Need
to use all the same year probably?2014 Cause: B.2 cardiovascular disease
Context: Cause Location: Select only countries and territories Sex: Male
+ Female + Both Metric: Rate

Deaths per 100, 000 population (confimred by GBD CODEBOOK
IHME\_GBD\_2019\_MEASURE\_METRIC\_DEFINITIONS\_Y2020M10D15) from
<http://ghdx.healthdata.org/gbd-results-tool>

``` r
gbd_cvd_mort <- read_csv("IHME-GBD_2019_DATA-703c6c0a-1/IHME-GBD_2019_DATA-703c6c0a-1.csv")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   measure_id = col_double(),
    ##   measure_name = col_character(),
    ##   location_id = col_double(),
    ##   location_name = col_character(),
    ##   sex_id = col_double(),
    ##   sex_name = col_character(),
    ##   age_id = col_double(),
    ##   age_name = col_character(),
    ##   cause_id = col_double(),
    ##   cause_name = col_character(),
    ##   metric_id = col_double(),
    ##   metric_name = col_character(),
    ##   year = col_double(),
    ##   val = col_double(),
    ##   upper = col_double(),
    ##   lower = col_double()
    ## )

``` r
gbd_cvd_mort <- as.data.table(gbd_cvd_mort)
```

Create columns of each sex catergory and year…

``` r
gbd_cvd_mort <- gbd_cvd_mort[, .(location_name, sex_name, year, val)] #just columns we want

gbd_cvd_mort_split <- split(gbd_cvd_mort, gbd_cvd_mort$year)

cvd_2005 <- gbd_cvd_mort_split$`2005`
cvd_2006 <- gbd_cvd_mort_split$`2006`
cvd_2007 <- gbd_cvd_mort_split$`2007`
cvd_2008 <- gbd_cvd_mort_split$`2008`
cvd_2009 <- gbd_cvd_mort_split$`2009`
cvd_2010 <- gbd_cvd_mort_split$`2010`
cvd_2011 <- gbd_cvd_mort_split$`2011`
cvd_2012 <- gbd_cvd_mort_split$`2012`
cvd_2013 <- gbd_cvd_mort_split$`2013`
cvd_2014 <- gbd_cvd_mort_split$`2014`
cvd_2015 <- gbd_cvd_mort_split$`2015`
cvd_2016 <- gbd_cvd_mort_split$`2016`
cvd_2017 <- gbd_cvd_mort_split$`2017`
cvd_2018 <- gbd_cvd_mort_split$`2018`
cvd_2019 <- gbd_cvd_mort_split$`2019`
setnames(cvd_2005, "val", "cvd2005")
setnames(cvd_2006, "val", "cvd2006")
setnames(cvd_2007, "val", "cvd2007")
setnames(cvd_2008, "val", "cvd2008")
setnames(cvd_2009, "val", "cvd2009")
setnames(cvd_2010, "val", "cvd2010")
setnames(cvd_2011, "val", "cvd2011")
setnames(cvd_2012, "val", "cvd2012")
setnames(cvd_2013, "val", "cvd2013")
setnames(cvd_2014, "val", "cvd2014")
setnames(cvd_2015, "val", "cvd2015")
setnames(cvd_2016, "val", "cvd2016")
setnames(cvd_2017, "val", "cvd2017")
setnames(cvd_2018, "val", "cvd2018")
setnames(cvd_2019, "val", "cvd2019")
cvd_2005[, year:= NULL]
cvd_2006[, year:= NULL]
cvd_2007[, year:= NULL]
cvd_2008[, year:= NULL]
cvd_2009[, year:= NULL]
cvd_2010[, year:= NULL]
cvd_2011[, year:= NULL]
cvd_2012[, year:= NULL]
cvd_2013[, year:= NULL]
cvd_2014[, year:= NULL]
cvd_2015[, year:= NULL]
cvd_2016[, year:= NULL]
cvd_2017[, year:= NULL]
cvd_2018[, year:= NULL]
cvd_2019[, year:= NULL]


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("location_name", "sex_name"), all.x= TRUE, all.y= TRUE)
  return(df)
}
cvdallyears <- Reduce(MyMerge, list(cvd_2005, cvd_2006, cvd_2007, cvd_2008, cvd_2009, cvd_2010, cvd_2011, cvd_2012, cvd_2013, cvd_2014, cvd_2015, cvd_2016, cvd_2017, cvd_2018,cvd_2019))


cvdallyears_split <- split(cvdallyears, cvdallyears$sex_name)
cvdallyearsboth <- cvdallyears_split$`Both`
cvdallyearsfemale <- cvdallyears_split$`Female`
cvdallyearsmale <- cvdallyears_split$`Male`

names(cvdallyearsboth)[3:17] <- c("cvd05both", "cvd06both", "cvd07both", "cvd08both", "cvd09both", "cvd10both", "cvd11both", "cvd12both", "cvd13both", "cvd14both", "cvd15both", "cvd16both", "cvd17both", "cvd18both", "cvd19both")
cvdallyearsboth[, sex_name := NULL]

names(cvdallyearsfemale)[3:17] <- c("cvd05fem", "cvd06fem", "cvd07fem", "cvd08fem", "cvd09fem", "cvd10fem", "cvd11fem", "cvd12fem", "cvd13fem", "cvd14fem", "cvd15fem", "cvd16fem", "cvd17fem", "cvd18fem", "cvd19fem")
cvdallyearsfemale[, sex_name := NULL]

names(cvdallyearsmale)[3:17] <- c("cvd05male", "cvd06male", "cvd07male", "cvd08male", "cvd09male", "cvd10male", "cvd11male", "cvd12male", "cvd13male", "cvd14male", "cvd15male", "cvd16male", "cvd17male", "cvd18male", "cvd19male")
cvdallyearsmale[, sex_name := NULL]

MyMerge <- function(x, y){
  df <- merge(x, y, by= c("location_name"), all.x= TRUE, all.y= TRUE)
  return(df)
}
cvdfinal<- Reduce(MyMerge, list(cvdallyearsfemale, cvdallyearsmale, cvdallyearsboth))
```

``` r
#rename countries

cvdfinal$location_name[cvdfinal$location_name == "Iran (Islamic Republic of)"] <- "Iran, Islamic Republic of"

cvdfinal$location_name[cvdfinal$location_name == "Republic of Korea"] <- "Korea (Republic of)"
#Democratic People's Republic of Korea = north korea, Republic of Korea = south korea Korea (republic of) is in gsni tables

cvdfinal$location_name[cvdfinal$location_name == "Republic of Moldova"] <- "Moldova, Republic of"

cvdfinal$location_name[cvdfinal$location_name == "Palestine"] <- "Palestine, State of"

cvdfinal$location_name[cvdfinal$location_name == "United States of America"] <- "United States"

gsni_cvd <- merge(gsni_tables, cvdfinal, by = "location_name", all = TRUE) #combine gsni and cvd mort for both sexes

gsni_cvd <- na.omit(gsni_cvd, cols = "GSNI_PERIOD") #omit countries without GSNI and cvd mort


#n=75
```

## LIFE EXPECTANCY FOR WOMEN

Per country for women at birth and age 60 (?use age 60 to remove
confounding of MMR) 2000, 2010, 2015, 2019

from: <https://apps.who.int/gho/data/view.main.WOMENLEXv>

``` r
fle <- read_csv("female_life_expectancy.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1], 'X2' [2]

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   X1 = col_character(),
    ##   X2 = col_character(),
    ##   `Life expectancy at birth (years)` = col_character(),
    ##   `Life expectancy at age 60 (years)` = col_character()
    ## )

``` r
fle <- as.data.table(fle)
fle <- fle[ -1,]

names(fle) <- c("location_name", "year", "LEbirth", "LE60")



female_life_expectancy_split <- split(fle, fle$year)


FLE2000 <- female_life_expectancy_split$`2000`
setnames(FLE2000, old = c("LEbirth", "LE60"), new = c("LEbirth2000", "LE602000"))
FLE2000[, year:= NULL]

FLE2010 <- female_life_expectancy_split$`2010`
setnames(FLE2010, old = c("LEbirth", "LE60"), new = c("LEbirth2010", "LE602010"))
FLE2010[, year:= NULL]

FLE2015 <- female_life_expectancy_split$`2015`
setnames(FLE2015, old = c("LEbirth", "LE60"), new = c("LEbirth2015", "LE602015"))
FLE2015[, year:= NULL]

FLE2019 <- female_life_expectancy_split$`2019`
setnames(FLE2019, old = c("LEbirth", "LE60"), new = c("LEbirth2019", "LE602019"))
FLE2019[, year:= NULL]


MyMerge <- function(x, y){
  df <- merge(x, y, by= c("location_name"), all.x= TRUE, all.y= TRUE)
  return(df)
}
female_life_expectancy<- Reduce(MyMerge, list(FLE2000, FLE2010, FLE2015, FLE2019))
```

``` r
#rename countries for merge
female_life_expectancy$location_name[female_life_expectancy$location_name == "Iran (Islamic Republic of)"] <- "Iran, Islamic Republic of"

female_life_expectancy$location_name[female_life_expectancy$location_name == "Republic of Korea"] <- "Korea (Republic of)"
#Democratic People's Republic of Korea = north korea, Republic of Korea = south korea Korea (republic of) is in gsni tables 

female_life_expectancy$location_name[female_life_expectancy$location_name == "Republic of Moldova"] <- "Moldova, Republic of"

female_life_expectancy$location_name[female_life_expectancy$location_name == "United States of America"] <- "United States"


gsni_cvd_lifexp <- merge(gsni_cvd, female_life_expectancy, by = "location_name", all = TRUE) 
gsni_cvd_lifexp <- na.omit(gsni_cvd_lifexp, cols = "GSNI_PERIOD") #omit countries without GSNI
```

## PHYSICIANS PER 1000

from: <https://data.worldbank.org/indicator/SH.MED.PHYS.ZS>

Didn’t work when downloaded as csv but worked when downloaded as xlsx
row 4 is year, data from 1960-2020 last updated 17/02/2021

want 2005-2017

``` r
phys_per_1000 <- read_excel("physiciansper1000/API_SH.MED.PHYS.ZS_DS2_en_csv_v2_2063402.xlsx")
```

    ## New names:
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * `` -> ...7
    ## * ...

``` r
phys_per_1000 <- as.data.table(phys_per_1000)

#2005-2017 (very little data for 18/19) columns...50-...62

phys_05_17 <- phys_per_1000 %>% select(1,50:62)
phys_05_17 <- phys_05_17[-c(1:4),]

names(phys_05_17) <- c("location_name", "phy05", "phy06", "phy07", "phy08", "phy09", "phy10", "phy11", "phy12", "phy13", "phy14", "phy15", "phy16", "phy17")
```

``` r
#rename countries for merge
phys_05_17$location_name[phys_05_17$location_name == "Iran, Islamic Rep."] <- "Iran, Islamic Republic of"

phys_05_17$location_name[phys_05_17$location_name == "Korea, Rep."] <- "Korea (Republic of)"
#Democratic People's Republic of Korea = north korea, Republic of Korea = south korea Korea (republic of) is in gsni tables

phys_05_17$location_name[phys_05_17$location_name == "Moldova"] <- "Moldova, Republic of"

gsni_cvd_lifexp_phys <- merge(gsni_cvd_lifexp, phys_05_17, by = "location_name", all = TRUE) 
gsni_cvd_lifexp_phys <- na.omit(gsni_cvd_lifexp_phys, cols = "GSNI_PERIOD") #omit countries without GSNI
```

## HEALTH EXPENDITURE (% of GDP)

from: <https://data.worldbank.org/indicator/SH.XPD.CHEX.GD.ZS>

``` r
HE <- read_excel("API_SH.XPD.CHEX.GD.ZS_DS2_en_excel_v2_2445365.xls")
```

    ## New names:
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * `` -> ...7
    ## * ...

``` r
HE1 <- HE[, c(1,50:62)]


names(HE1) <- c("location_name", "HE05", "HE06", "HE07", "HE08", "HE09", "HE10", "HE11", "HE12", "HE13", "HE14", "HE15", "HE16", "HE17")


#rename countries for merge
HE1$location_name[HE1$location_name == "Iran, Islamic Rep."] <- "Iran, Islamic Republic of"

HE1$location_name[HE1$location_name == "Korea, Rep."] <- "Korea (Republic of)"
#Democratic People's Republic of Korea = north korea, Republic of Korea = south korea Korea (republic of) is in gsni tables

HE1$location_name[HE1$location_name == "Moldova"] <- "Moldova, Republic of"

gsni_cvd_lifexp_phys <- merge(gsni_cvd_lifexp_phys, HE1, by = "location_name", all = TRUE) 
gsni_cvd_lifexp_phys <- na.omit(gsni_cvd_lifexp_phys, cols = "GSNI_PERIOD") #omit countries without GSNI
```

## MEAN YEARS OF SCHOOLING

from: <https://ourworldindata.org/global-education> up to 2017 want
2005-2017

``` r
mean_years_school <- read_csv("mean-years-of-schooling-1.csv")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   Entity = col_character(),
    ##   Code = col_character(),
    ##   Year = col_double(),
    ##   `Average Total Years of Schooling for Adult Population (Lee-Lee (2016), Barro-Lee (2018) and UNDP (2018))` = col_double()
    ## )

``` r
mean_years_school <- as.data.table(mean_years_school)

school_split <- split(mean_years_school, mean_years_school$Year)


scl2005 <- school_split$`2005`
names(scl2005) <- c("location_name","code", "year", "scl05")
scl2005 <- scl2005 %>% select(1,4)

scl2006 <- school_split$`2006`
names(scl2006)<- c("location_name","code", "year","scl06")
scl2006 <- scl2006 %>% select(1,4)

scl2007 <- school_split$`2007`
names(scl2007)<- c("location_name","code", "year","scl07")
scl2007 <- scl2007 %>% select(1,4)

scl2008 <- school_split$`2008`
names(scl2008)<- c("location_name","code", "year","scl08")
scl2008 <- scl2008 %>% select(1,4)

scl2009 <- school_split$`2009`
names(scl2009)<- c("location_name","code", "year","scl09")
scl2009 <- scl2009 %>% select(1,4)

scl2010 <- school_split$`2010`
names(scl2010)<- c("location_name","code", "year","scl10")
scl2010 <- scl2010 %>% select(1,4)

scl2011 <- school_split$`2011`
names(scl2011)<- c("location_name","code", "year","scl11")
scl2011 <- scl2011 %>% select(1,4)

scl2012 <- school_split$`2012`
names(scl2012)<- c("location_name","code", "year","scl12")
scl2012 <- scl2012 %>% select(1,4)

scl2013 <- school_split$`2013`
names(scl2013)<- c("location_name","code", "year","scl13")
scl2013 <- scl2013 %>% select(1,4)

scl2014 <- school_split$`2014`
names(scl2014) <- c("location_name", "code", "year", "scl14")
scl2014 <- scl2014 %>% select(1,4)

scl2015 <- school_split$`2015`
names(scl2015)<- c("location_name","code", "year","scl15")
scl2015 <- scl2015 %>% select(1,4)

scl2016 <- school_split$`2016`
names(scl2016)<- c("location_name","code", "year","scl16")
scl2016 <- scl2016 %>% select(1,4)

scl2017 <- school_split$`2017`
names(scl2017)<- c("location_name","code", "year","scl17")
scl2017 <- scl2017 %>% select(1,4)

MyMerge <- function(x, y){
  df <- merge(x, y, by= c("location_name"), all.x= TRUE, all.y= TRUE)
  return(df)
}
scl0517 <- Reduce(MyMerge, list(scl2005, scl2006, scl2007, scl2008, scl2009, scl2010, scl2011, scl2012, scl2013, scl2014, scl2015, scl2016, scl2017))
```

``` r
#rename columns 
mean_years_school$location_name[mean_years_school$location_name == "Iran"] <- "Iran, Islamic Republic of"
mean_years_school$location_name[mean_years_school$location_name == "South Korea"] <- "Korea (Republic of)"

mean_years_school$location_name[mean_years_school$location_name == "Moldova"] <- "Moldova, Republic of"

mean_years_school$location_name[mean_years_school$location_name == "Palestine"] <- "Palestine, State of"


gsni_cvd_lifexp_phys_sch <- merge(gsni_cvd_lifexp_phys, scl0517, by = "location_name", all = TRUE) 
gsni_cvd_lifexp_phys_sch <- na.omit(gsni_cvd_lifexp_phys_sch, cols = "GSNI_PERIOD") #omit countries without GSNI
```

## GDP

GDP per capita (current US dollar)

from: <https://data.worldbank.org/indicator/NY.GDP.PCAP.CD>

``` r
library(readxl)
GDPpercapita <- read_excel("GDPpercapita.xls")
```

    ## New names:
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * `` -> ...7
    ## * ...

``` r
GDPpercapita <- as.data.table(GDPpercapita)


#2005-2017 

GDP0517 <- GDPpercapita %>% select(1,50:62)
GDP0517 <- GDP0517[-c(1:3),]

names(GDP0517) <- c("location_name", "GDP05", "GDP06", "GDP07", "GDP08", "GDP09", "GDP10", "GDP11", "GDP12", "GDP13", "GDP14", "GDP15", "GDP16", "GDP17")
```

``` r
#rename columns 
GDPpercapita$location_name[GDPpercapita$location_name == "Iran, Islamic Rep."] <- "Iran, Islamic Republic of"

GDPpercapita$location_name[GDPpercapita$location_name == "Korea, Rep."] <- "Korea (Republic of)"

GDPpercapita$location_name[GDPpercapita$location_name == "Moldova"] <- "Moldova, Republic of"


gsni_cvd_lifexp_phys_sch_gdp <- merge(gsni_cvd_lifexp_phys_sch, GDP0517, by = "location_name", all = TRUE) 
gsni_cvd_lifexp_phys_sch_gdp <- na.omit(gsni_cvd_lifexp_phys_sch_gdp, cols = "GSNI_PERIOD") #omit countries without GSNI
```

## MATERNAL MORTALITY RATIO

from: <https://data.worldbank.org/indicator/SH.STA.MMRT>

``` r
MMR <- read_excel("API_SH.STA.MMRT_DS2_en_excel_v2_2056748.xls")
```

    ## New names:
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * `` -> ...7
    ## * ...

``` r
MMR <- as.data.table(MMR)

#2005-2017 

MMR0517 <- MMR %>% select(1,50:62)
MMR0517 <- MMR0517[-c(1:3),]

names(MMR0517) <- c("location_name", "MMR05", "MMR06", "MMR07", "MMR08", "MMR09", "MMR10", "MMR11", "MMR12", "MMR13", "MMR14", "MMR15", "MMR16", "MMR17")
```

``` r
#rename columns 
MMR0517$location_name[MMR0517$location_name == "Iran, Islamic Rep."] <- "Iran, Islamic Republic of"

MMR0517$location_name[MMR0517$location_name == "Korea, Rep."] <- "Korea (Republic of)"

MMR0517$location_name[MMR0517$location_name == "Moldova"] <- "Moldova, Republic of"


gsni_cvd_lifexp_phys_sch_gdp_mmr <- merge(gsni_cvd_lifexp_phys_sch_gdp, MMR0517, by = "location_name", all = TRUE) 
gsni_cvd_lifexp_phys_sch_gdp_mmr <- na.omit(gsni_cvd_lifexp_phys_sch_gdp_mmr, cols = "GSNI_PERIOD") #omit countries without GSNI
```

\#\#COUNTRY INCOME LEVEL

from:
<https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups>
(historical classification by income)

``` r
LMH <- read_excel("OGHIST.xls", sheet = 3)
```

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...3
    ## * `` -> ...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * ...

``` r
LMH <- as.data.table(LMH)

LMH1 <- LMH[5:228, c(2, 25, 30)] #extract required data

LMH2 <- LMH1[-c(2:6), ]

#LMH09 and LMH14 = world bank classification in 2009 & 2014 using GNI per capita 
setnames(LMH2, "...25", "LMH09") 
setnames(LMH2, "...30", "LMH14")
setnames(LMH2,"World Bank Analytical Classifications", "location_name")

LMH2 <- LMH2[-1,]

#rename columns 
LMH2$location_name[LMH2$location_name == "Iran, Islamic Rep."] <- "Iran, Islamic Republic of"
LMH2$location_name[LMH2$location_name == "Korea, Rep."] <- "Korea (Republic of)"
LMH2$location_name[LMH2$location_name == "Moldova"] <- "Moldova, Republic of"

#merge with table
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh <- merge(gsni_cvd_lifexp_phys_sch_gdp_mmr, LMH2, by = "location_name", all = TRUE) 
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh <- na.omit(gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh, cols = "GSNI_PERIOD") #omit countries without GSNI

head(gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh)
```

    ##    location_name GSNI_PERIOD onebias twobias             nobias
    ## 1:       Algeria   2010–2014   97.83      87               2.17
    ## 2:       Andorra   2005–2009   27.01    7.43 72.989999999999995
    ## 3:     Argentina   2010–2014   75.41   42.49              24.59
    ## 4:       Armenia   2010–2014   94.11   81.28               5.89
    ## 5:     Australia   2010–2014   46.24      23              53.76
    ## 6:    Azerbaijan   2010–2014   99.14   93.82               0.86
    ##             political           economic educational physical  cvd05fem
    ## 1:              80.08              74.08       37.17    86.75 500.26865
    ## 2:              14.08               8.73        1.81    12.01  94.99074
    ## 3:              43.35              30.43       17.04    52.86 169.99370
    ## 4: 72.819999999999993              75.91       24.89    66.14 399.51778
    ## 5: 32.479999999999997 18.059999999999999        4.09    20.93 121.85287
    ## 6:              85.13              91.97        30.9    72.16 653.88539
    ##     cvd06fem  cvd07fem  cvd08fem  cvd09fem  cvd10fem  cvd11fem  cvd12fem
    ## 1: 494.78751 490.17488 486.31725 483.19933 484.49190 484.48211 479.07575
    ## 2:  94.10549  92.00298  91.82684  93.03548  93.64847  94.72272  94.77537
    ## 3: 165.87751 165.37005 156.11837 153.96832 154.88168 152.74845 149.31115
    ## 4: 387.58279 372.19335 367.54267 356.24738 345.06490 332.54986 324.41710
    ## 5: 117.56935 115.44308 112.43414 107.29165 102.53083  99.64511  95.33617
    ## 6: 662.74433 659.53238 654.59319 655.75062 661.62994 662.91437 665.11545
    ##     cvd13fem  cvd14fem  cvd15fem  cvd16fem  cvd17fem  cvd18fem  cvd19fem
    ## 1: 472.94269 471.02644 468.44578 458.09789 450.74425 451.34683 447.67418
    ## 2:  95.38794  95.79920  96.76678  96.28366  95.83302  95.48617  95.05596
    ## 3: 147.59175 146.19661 147.86150 153.18030 154.09804 150.26997 149.24294
    ## 4: 312.28518 305.51958 306.31351 298.74575 297.69330 296.63522 294.02914
    ## 5:  91.97374  91.08401  90.33633  87.58080  87.33218  88.85876  89.00590
    ## 6: 673.97385 688.85984 689.28400 688.89897 685.80022 648.14175 627.27975
    ##    cvd05male cvd06male cvd07male cvd08male cvd09male cvd10male cvd11male
    ## 1:  462.4656  450.5957  439.3599  428.9066  419.1618  408.5190  403.3053
    ## 2:  133.5110  131.0153  128.3052  126.3548  125.2114  124.1698  124.0550
    ## 3:  265.4632  258.6674  258.7628  246.9013  240.5962  239.1611  236.2446
    ## 4:  537.4210  521.6946  500.3150  491.6596  484.3347  479.2840  469.5447
    ## 5:  181.3310  172.6294  168.0884  163.8370  155.6688  149.6376  144.4923
    ## 6:  795.4780  806.0879  797.5581  795.9201  792.0192  792.6029  806.8414
    ##    cvd12male cvd13male cvd14male cvd15male cvd16male cvd17male cvd18male
    ## 1:  397.5104  389.1503  384.7680  383.2925  379.8222  377.6635  373.4875
    ## 2:  123.0521  122.3036  120.9398  119.3653  118.6682  117.7883  116.9475
    ## 3:  233.3321  230.6517  226.4522  227.7888  234.6520  235.0038  228.2053
    ## 4:  446.4262  427.0121  426.5800  423.7013  415.9128  413.7329  412.6031
    ## 5:  137.6570  132.9428  131.1893  130.2281  126.8570  127.6255  129.0003
    ## 6:  826.6460  825.9170  835.2344  833.7264  842.2704  843.8297  784.6385
    ##    cvd19male cvd05both cvd06both cvd07both cvd08both cvd09both cvd10both
    ## 1:  371.5185  479.0988  469.5897  460.6100  452.3262  444.6443  437.7233
    ## 2:  115.8910  114.4628  112.7694  110.3326  109.3167  109.3833  109.1886
    ## 3:  226.1503  212.7781  207.4933  207.1775  196.6322  192.6343  192.5741
    ## 4:  410.1931  460.3967  448.1819  430.5289  425.2376  415.1551  405.3203
    ## 5:  129.3734  148.9638  142.8732  139.8263  136.2677  129.7996  124.4187
    ## 6:  760.3724  725.8544  735.0585  728.3514  724.4674  722.6059  725.7938
    ##    cvd11both cvd12both cvd13both cvd14both cvd15both cvd16both cvd17both
    ## 1:  433.4500  426.8407  418.7057  415.1672  412.8064  405.9023  401.3947
    ## 2:  109.4555  108.8728  108.7915  108.3474  108.1292  107.5617  106.9530
    ## 3:  190.0731  186.7114  184.5299  181.8886  183.4215  189.4145  190.1406
    ## 4:  393.6405  380.1264  364.7953  360.0052  359.2512  351.2326  349.4778
    ## 5:  120.5740  115.1855  111.2427  110.0305  109.2401  106.1615  106.3680
    ## 6:  732.1759  739.5796  744.0043  757.2168  756.2165  758.3307  756.7578
    ##    cvd18both cvd19both LEbirth2000 LE602000 LEbirth2010 LE602010 LEbirth2015
    ## 1:  399.5052  396.9652        73.5     20.3        76.8     21.9        77.5
    ## 2:  106.3957  105.7119        <NA>     <NA>        <NA>     <NA>        <NA>
    ## 3:  185.2892  183.9331        77.8     22.5        78.6     22.7        79.3
    ## 4:  348.1915  345.4950        74.9     20.2        76.6     20.5        77.8
    ## 5:  107.8681  108.1264        82.2     24.9        84.0     26.1        84.2
    ## 6:  710.3898  688.0782        68.5     17.9        71.8     17.6        73.4
    ##    LE602015 LEbirth2019 LE602019  phy05  phy06  phy07  phy08  phy09  phy10
    ## 1:     22.3        78.1     22.6 1.0242     NA 1.1958     NA     NA 1.2070
    ## 2:     <NA>        <NA>     <NA> 3.2319 3.0123 3.0109     NA 3.1479 4.0000
    ## 3:     23.0        79.5     23.1 3.2100     NA     NA     NA     NA 3.2100
    ## 4:     21.2        79.2     22.1 2.5643 2.5897 2.6724 2.7432 2.7679 2.8419
    ## 5:     26.3        84.8     26.8 2.7794 2.8338 2.9954 3.0056 3.1085 3.3429
    ## 6:     17.9        74.1     18.3 3.5819 3.5649 3.7124 3.6844 3.6751 3.6629
    ##     phy11  phy12  phy13  phy14  phy15  phy16  phy17               HE05
    ## 1:     NA     NA     NA     NA     NA 1.8325 1.7879 3.2351613000000001
    ## 2:     NA     NA     NA     NA 3.3333     NA     NA 5.5754260999999996
    ## 3:     NA     NA 3.9385     NA     NA 4.0013 3.9901 7.6107888199999998
    ## 4: 2.8677 2.8968 2.9031 2.8928 2.9143     NA 4.4023 5.8618803000000002
    ## 5: 3.2878 3.2858 3.3530 3.4314 3.4886 3.5672 3.6778 7.9900822600000003
    ## 6: 3.4376 3.4901 3.4558 3.4460     NA     NA     NA 2.2610766899999999
    ##                  HE06               HE07               HE08               HE09
    ## 1:         3.35510325 3.8214178099999998 4.2018823599999999 5.3593983700000001
    ## 2: 4.9350943599999999 4.9255475999999998 5.8059859300000003 6.2023372700000001
    ## 3: 7.6403284100000004 7.8345508600000002 8.1826972999999992 9.4559955599999999
    ## 4: 5.8500709500000001         5.49814034 6.6397657399999996 8.4437761299999998
    ## 5: 7.9894385300000001 8.0675535200000006 8.2558565099999992 8.5631647100000006
    ## 6: 2.0667839099999998         1.99021435         1.93880868 2.6137836000000001
    ##                  HE10               HE11               HE12               HE13
    ## 1: 5.1171698599999997 5.2674808500000001         6.00050974 6.0357627899999997
    ## 2:         6.64963865 6.2465286300000002 6.1015033699999996 5.9878034600000003
    ## 3: 9.4454641299999995 9.4181985899999994 9.8272666900000001 9.7809791599999993
    ## 4: 9.2353830299999995 9.3758878699999997 9.1345500899999994        10.32345009
    ## 5: 8.4308423999999995 8.5415382399999995 8.6756477400000005 8.7611637099999999
    ## 6: 2.4899666300000001 2.4464180500000001         2.96481562 3.0367231399999999
    ##                  HE14               HE15               HE16               HE17
    ## 1: 6.5472140300000001 6.9784917799999997         6.60778189         6.38032866
    ## 2: 5.9791245499999999 6.2324533500000001 6.3434934600000004 6.5443186799999999
    ## 3:         9.67129993 10.229337689999999 9.0018968600000004 10.457043649999999
    ## 4:        10.17827797 10.117628099999999 9.9519586600000007        10.36270523
    ## 5: 9.0359706899999992 9.3233118099999999 9.2007131599999994 9.2054481500000005
    ## 6: 3.3771374199999999 4.1066360499999996 4.0365543400000004 3.7365145700000002
    ##    scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12 scl13 scl14 scl15 scl16
    ## 1:   6.9   7.0   7.2   6.7   6.9   7.1   7.4   7.6   7.8   7.9   7.9   8.0
    ## 2:   9.8  10.1  10.1  10.1  10.1  10.1  10.2  10.2  10.2  10.2  10.2  10.2
    ## 3:   9.1   9.0   8.9   8.9   9.3   9.8   9.8   9.8   9.8   9.8   9.8   9.9
    ## 4:  10.9  10.9  11.0  11.0  11.1  11.1  11.2  11.3  11.4  11.5  11.6  11.7
    ## 5:  11.7  11.9  12.0  12.3  12.3  12.4  12.5  12.6  12.6  12.7  12.8  12.9
    ## 6:  10.7  10.7  10.2  10.2  10.7  10.7  10.7  10.7  10.8  10.7  10.7  10.7
    ##    scl17              GDP05              GDP06              GDP07
    ## 1:   8.0 3113.0953308508388 3478.7109285912129 3946.6644517917011
    ## 2:  10.2 40066.256918514569  42675.81275692967 47803.693607526606
    ## 3:   9.9 5109.8513252262128 5919.0120370775321 7245.4483172894461
    ## 4:  11.7 1643.7530293610992  2158.143697101787 3139.2774989433315
    ## 5:  12.9 33999.242857583544 36044.922810848482 40960.054494819851
    ## 6:  10.7 1578.4023902960269 2473.0818186353627 3851.4378687117223
    ##                 GDP08              GDP09              GDP10              GDP11
    ## 1: 4923.8431850419083  3883.132425297737 4479.3417195800466  5462.260897231834
    ## 2:  48718.49686919128 43503.185515676858 40852.666777443432 43335.328861828319
    ## 3: 9020.8730980719447 8225.1371762641174 10385.964431955526  12848.86419697053
    ## 4:  4010.857242552257 2994.3425444850923 3218.3727066056331 3525.8047467115348
    ## 5: 49601.656708217793 42772.359166449794 52022.125596187558 62517.833747150289
    ## 6: 5574.6038021861259 4950.2947914237511 5842.8057835857626 7189.6912292076549
    ##                 GDP12              GDP13              GDP14              GDP15
    ## 1: 5591.2123532240421 5498.7840949782312  5494.352336042296 4187.5097274720447
    ## 2:  38686.46126350751 39538.766722042601 41303.929371446415 35762.523073757991
    ## 3: 13082.664325571988 13080.254732336658 12334.798245389289 13789.060424772022
    ## 4: 3681.8574560431011 3838.1858014838167 3986.2316237671262 3607.2966967227062
    ## 5: 68012.147900593409 68150.107041321491 62510.791170564138   56755.7217124249
    ## 6: 7496.2946476826328  7875.756952542878  7891.313147499859 5500.3103824440796
    ##                 GDP16              GDP17 MMR05 MMR06 MMR07 MMR08 MMR09 MMR10
    ## 1: 3945.4820813385431 4111.2941101326878   127   122   119   117   117   115
    ## 2: 37474.665405724452 38962.880353670327  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
    ## 3: 12790.242473244707  14613.04182466961    59    57    56    53    56    51
    ## 4: 3591.8292755302309 3914.5012684127969    35    36    32    36    32    32
    ## 5: 49971.131456129013 54027.966818456778     5     5     5     5     5     5
    ## 6: 3880.7387308955604 4147.0897156917072    42    35    34    32    32    31
    ##    MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17 LMH09 LMH14
    ## 1:   116   116   115   114   114   113   112    UM    UM
    ## 2:  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>     H     H
    ## 3:    48    47    44    42    41    40    39    UM     H
    ## 4:    30    30    26    27    28    26    26    LM    LM
    ## 5:     6     6     6     6     6     6     6     H     H
    ## 6:    30    29    28    28    27    26    26    UM    UM

``` r
na.omit(gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh, c("LMH09", "LMH14"), invert = TRUE)
```

    ##          location_name GSNI_PERIOD onebias twobias             nobias political
    ## 1:          Kyrgyzstan   2010–2014   96.73   84.87               3.27      76.8
    ## 2: Palestine, State of   2010–2014      98    92.3                  2      89.3
    ## 3:            Viet Nam   2005–2009   92.89   69.17               7.11      59.4
    ## 4:               Yemen   2010–2014    97.8    92.1 2.2000000000000002      87.4
    ##    economic educational physical cvd05fem cvd06fem cvd07fem cvd08fem cvd09fem
    ## 1:    71.53          41    81.73 546.8872 553.7142 543.2533 536.8522 507.4175
    ## 2:     79.5        26.7     83.5 367.3750 362.0774 355.8204 350.5859 343.9703
    ## 3:    62.49       20.36    70.56 256.6274 256.6231 255.9745 255.1929 254.4622
    ## 4:     87.2        45.3       81 493.0600 489.2440 485.1008 483.3384 478.3407
    ##    cvd10fem cvd11fem cvd12fem cvd13fem cvd14fem cvd15fem cvd16fem cvd17fem
    ## 1: 488.1873 483.2938 465.0509 442.7975 429.3656 435.0685 415.0933 408.2852
    ## 2: 341.7886 337.6966 320.1783 314.6376 323.3969 338.5276 350.9355 354.1550
    ## 3: 252.5587 250.1793 247.0552 243.0562 239.2128 234.9403 230.6141 226.8228
    ## 4: 472.1759 471.3592 468.9990 467.1613 464.4741 466.8673 466.8844 469.9199
    ##    cvd18fem cvd19fem cvd05male cvd06male cvd07male cvd08male cvd09male
    ## 1: 395.2001 390.5737  748.0504  780.9492  781.9687  761.4547  734.8094
    ## 2: 349.6899 345.6669  524.2705  515.4991  498.1858  484.7670  474.6089
    ## 3: 223.3228 219.5527  448.4061  449.7884  453.6026  455.8597  457.1055
    ## 4: 473.8849 476.9648  579.9540  575.3879  568.8200  566.4953  558.3360
    ##    cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male cvd16male
    ## 1:  702.9963  649.3122  632.0656  609.2108  620.6342  644.2163  618.5638
    ## 2:  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321  433.5059
    ## 3:  456.0361  453.6313  450.9162  449.0089  446.5765  443.7173  440.1349
    ## 4:  547.3893  545.5135  541.5427  538.4431  532.5702  536.2482  536.1051
    ##    cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both cvd08both
    ## 1:  613.3843  580.3176  569.3491  634.0089  651.1682  645.5907  632.7615
    ## 2:  438.1050  440.8897  433.9799  434.1983  427.1939  415.8933  407.1588
    ## 3:  436.8855  433.6069  429.9362  334.7621  335.3238  336.5239  337.0268
    ## 4:  541.0473  547.2489  550.6548  535.1088  530.9694  525.7088  523.7193
    ##    cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both cvd15both
    ## 1:  603.3141  581.8522  562.6225  543.9251  519.5371  513.0161  523.1598
    ## 2:  399.0904  394.3262  387.9774  366.8452  355.9001  368.8565  380.8886
    ## 3:  337.1512  335.4539  333.0107  329.9312  326.6030  323.1828  319.1363
    ## 4:  517.1861  508.7890  507.4918  504.3873  501.9957  497.8142  500.9039
    ##    cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000 LEbirth2010
    ## 1:  500.1438  493.8208  473.2671  466.3130        70.2     18.3        73.3
    ## 2:  384.4684  388.1755  386.6795  381.6978        <NA>     <NA>        <NA>
    ## 3:  314.9546  311.2375  307.7080  303.9047        75.6     21.0        77.1
    ## 4:  500.8572  504.8275  509.8769  513.0910        64.7     17.9        69.6
    ##    LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019 phy05 phy06 phy07 phy08
    ## 1:     19.3        75.3     20.2        77.3     21.7    NA    NA    NA    NA
    ## 2:     <NA>        <NA>     <NA>        <NA>     <NA>    NA    NA    NA    NA
    ## 3:     21.5        77.6     21.8        78.1     22.0    NA    NA    NA    NA
    ## 4:     18.7        69.6     18.9        68.9     18.7    NA    NA    NA    NA
    ##    phy09 phy10 phy11 phy12 phy13 phy14 phy15 phy16 phy17 HE05 HE06 HE07 HE08
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA <NA> <NA> <NA> <NA>
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA <NA> <NA> <NA> <NA>
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA <NA> <NA> <NA> <NA>
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    NA <NA> <NA> <NA> <NA>
    ##    HE09 HE10 HE11 HE12 HE13 HE14 HE15 HE16 HE17 scl05 scl06 scl07 scl08 scl09
    ## 1: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>  10.2  10.2  10.3  10.3  10.4
    ## 2: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>    NA    NA    NA    NA    NA
    ## 3: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>    NA    NA    NA    NA    NA
    ## 4: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>   1.9   2.0   2.2   2.3   2.5
    ##    scl10 scl11 scl12 scl13 scl14 scl15 scl16 scl17 GDP05 GDP06 GDP07 GDP08
    ## 1:  10.6  10.6  10.7  10.7  10.8  10.8  10.9  10.9  <NA>  <NA>  <NA>  <NA>
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>  <NA>  <NA>
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>  <NA>  <NA>
    ## 4:   2.6   2.8   3.0   3.0   3.0   3.0   3.0   3.0  <NA>  <NA>  <NA>  <NA>
    ##    GDP09 GDP10 GDP11 GDP12 GDP13 GDP14 GDP15 GDP16 GDP17 MMR05 MMR06 MMR07
    ## 1:  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
    ## 2:  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
    ## 3:  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
    ## 4:  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
    ##    MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17 LMH09 LMH14
    ## 1:  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
    ## 2:  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
    ## 3:  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
    ## 4:  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>

``` r
LMH[c(118, 223, 226, 225),]
```

    ##    ...1 World Bank Analytical Classifications ...3 ...4 ...5 ...6 ...7 ...8
    ## 1:  KGZ                       Kyrgyz Republic   ..   ..   ..   ..   LM   LM
    ## 2:  VNM                               Vietnam    L    L    L    L    L    L
    ## 3:  YEM                           Yemen, Rep.  LM*  LM*   LM   LM    L    L
    ## 4:  PSE                    West Bank and Gaza   ..   ..   ..   ..   ..   ..
    ##    ...9 ...10 ...11 ...12 ...13 ...14 ...15 ...16 ...17 ...18 ...19 ...20 ...21
    ## 1:   LM     L     L     L     L     L     L     L     L     L     L     L     L
    ## 2:    L     L     L     L     L     L     L     L     L     L     L     L     L
    ## 3:    L     L     L     L     L     L     L     L     L     L     L     L     L
    ## 4:   ..    LM    LM    LM    LM    LM    LM    LM    LM    LM    LM    LM    LM
    ##    ...22 ...23 ...24 ...25 ...26 ...27 ...28 ...29 ...30 ...31 ...32 ...33
    ## 1:     L     L     L     L     L     L     L    LM    LM    LM    LM    LM
    ## 2:     L     L     L    LM    LM    LM    LM    LM    LM    LM    LM    LM
    ## 3:     L     L     L    LM    LM    LM    LM    LM    LM    LM    LM     L
    ## 4:    LM    LM    LM    LM    LM    LM    LM    LM    LM    LM    LM    LM
    ##    ...34 ...35
    ## 1:    LM    LM
    ## 2:    LM    LM
    ## 3:     L     L
    ## 4:    LM    LM

``` r
#Kyrgzstan, viet nam and yemen are low middle income (names didn't sync)
```

\#\#\#as.numeric

``` r
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, twobias := as.numeric(twobias)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, twobias := as.numeric(twobias)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, onebias := as.numeric(onebias)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, nobias := as.numeric(nobias)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, political:= as.numeric(political)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, economic:= as.numeric(economic)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, educational:= as.numeric(educational)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, physical := as.numeric(physical)]

gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LEbirth2000 := as.numeric(LEbirth2000)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LEbirth2010 := as.numeric(LEbirth2010)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LEbirth2015 := as.numeric(LEbirth2015)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LEbirth2019 := as.numeric(LEbirth2019)]

gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LE602000 := as.numeric(LE602000)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LE602010 := as.numeric(LE602010)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LE602015 := as.numeric(LE602015)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LE602019 := as.numeric(LE602019)]

gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP05 := as.numeric(GDP05)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP06 := as.numeric(GDP06)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP07 := as.numeric(GDP07)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP08 := as.numeric(GDP08)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP09 := as.numeric(GDP09)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP10 := as.numeric(GDP10)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP11 := as.numeric(GDP11)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP12 := as.numeric(GDP12)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP13 := as.numeric(GDP13)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP14 := as.numeric(GDP14)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP15 := as.numeric(GDP15)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP16 := as.numeric(GDP16)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GDP17 := as.numeric(GDP17)]

gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR05 := as.numeric(MMR05)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR06 := as.numeric(MMR06)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR07 := as.numeric(MMR07)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR08 := as.numeric(MMR08)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR09 := as.numeric(MMR09)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR10 := as.numeric(MMR10)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR11 := as.numeric(MMR11)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR12 := as.numeric(MMR12)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR13 := as.numeric(MMR13)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR14 := as.numeric(MMR14)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR15 := as.numeric(MMR15)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR16 := as.numeric(MMR16)]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, MMR17 := as.numeric(MMR17)]

gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, GSNI_PERIOD := as.factor(GSNI_PERIOD)]

gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LMH09 := factor(LMH09, 
                           levels = c("L", "LM", "UM", "H"),
                           labels = c("low income", "lower middle income", "upper middle income", "high income")
                      )]
gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, LMH14 := factor(LMH14, 
                           levels = c("L", "LM", "UM", "H"),
                           labels = c("low income", "lower middle income", "upper middle income", "high income")
                      )]
```

``` r
glimpse(gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh)
```

    ## Rows: 75
    ## Columns: 129
    ## $ location_name <chr> "Algeria", "Andorra", "Argentina", "Armenia", "Australia…
    ## $ GSNI_PERIOD   <fct> 2010–2014, 2005–2009, 2010–2014, 2010–2014, 2010–2014, 2…
    ## $ onebias       <dbl> 97.83, 27.01, 75.41, 94.11, 46.24, 99.14, 90.37, 89.50, …
    ## $ twobias       <dbl> 87.00, 7.43, 42.49, 81.28, 23.00, 93.82, 71.70, 52.39, 4…
    ## $ nobias        <dbl> 2.17, 72.99, 24.59, 5.89, 53.76, 0.86, 9.63, 10.50, 23.1…
    ## $ political     <dbl> 80.08, 14.08, 43.35, 72.82, 32.48, 85.13, 77.82, 43.41, …
    ## $ economic      <dbl> 74.08, 8.73, 30.43, 75.91, 18.06, 91.97, 58.45, 36.63, 3…
    ## $ educational   <dbl> 37.17, 1.81, 17.04, 24.89, 4.09, 30.90, 21.19, 9.32, 10.…
    ## $ physical      <dbl> 86.75, 12.01, 52.86, 66.14, 20.93, 72.16, 55.52, 77.95, …
    ## $ cvd05fem      <dbl> 500.26865, 94.99074, 169.99370, 399.51778, 121.85287, 65…
    ## $ cvd06fem      <dbl> 494.78751, 94.10549, 165.87751, 387.58279, 117.56935, 66…
    ## $ cvd07fem      <dbl> 490.17488, 92.00298, 165.37005, 372.19335, 115.44308, 65…
    ## $ cvd08fem      <dbl> 486.31725, 91.82684, 156.11837, 367.54267, 112.43414, 65…
    ## $ cvd09fem      <dbl> 483.19933, 93.03548, 153.96832, 356.24738, 107.29165, 65…
    ## $ cvd10fem      <dbl> 484.49190, 93.64847, 154.88168, 345.06490, 102.53083, 66…
    ## $ cvd11fem      <dbl> 484.48211, 94.72272, 152.74845, 332.54986, 99.64511, 662…
    ## $ cvd12fem      <dbl> 479.07575, 94.77537, 149.31115, 324.41710, 95.33617, 665…
    ## $ cvd13fem      <dbl> 472.94269, 95.38794, 147.59175, 312.28518, 91.97374, 673…
    ## $ cvd14fem      <dbl> 471.02644, 95.79920, 146.19661, 305.51958, 91.08401, 688…
    ## $ cvd15fem      <dbl> 468.44578, 96.76678, 147.86150, 306.31351, 90.33633, 689…
    ## $ cvd16fem      <dbl> 458.09789, 96.28366, 153.18030, 298.74575, 87.58080, 688…
    ## $ cvd17fem      <dbl> 450.74425, 95.83302, 154.09804, 297.69330, 87.33218, 685…
    ## $ cvd18fem      <dbl> 451.34683, 95.48617, 150.26997, 296.63522, 88.85876, 648…
    ## $ cvd19fem      <dbl> 447.67418, 95.05596, 149.24294, 294.02914, 89.00590, 627…
    ## $ cvd05male     <dbl> 462.4656, 133.5110, 265.4632, 537.4210, 181.3310, 795.47…
    ## $ cvd06male     <dbl> 450.5957, 131.0153, 258.6674, 521.6946, 172.6294, 806.08…
    ## $ cvd07male     <dbl> 439.3599, 128.3052, 258.7628, 500.3150, 168.0884, 797.55…
    ## $ cvd08male     <dbl> 428.9066, 126.3548, 246.9013, 491.6596, 163.8370, 795.92…
    ## $ cvd09male     <dbl> 419.1618, 125.2114, 240.5962, 484.3347, 155.6688, 792.01…
    ## $ cvd10male     <dbl> 408.5190, 124.1698, 239.1611, 479.2840, 149.6376, 792.60…
    ## $ cvd11male     <dbl> 403.3053, 124.0550, 236.2446, 469.5447, 144.4923, 806.84…
    ## $ cvd12male     <dbl> 397.5104, 123.0521, 233.3321, 446.4262, 137.6570, 826.64…
    ## $ cvd13male     <dbl> 389.1503, 122.3036, 230.6517, 427.0121, 132.9428, 825.91…
    ## $ cvd14male     <dbl> 384.7680, 120.9398, 226.4522, 426.5800, 131.1893, 835.23…
    ## $ cvd15male     <dbl> 383.2925, 119.3653, 227.7888, 423.7013, 130.2281, 833.72…
    ## $ cvd16male     <dbl> 379.8222, 118.6682, 234.6520, 415.9128, 126.8570, 842.27…
    ## $ cvd17male     <dbl> 377.6635, 117.7883, 235.0038, 413.7329, 127.6255, 843.82…
    ## $ cvd18male     <dbl> 373.4875, 116.9475, 228.2053, 412.6031, 129.0003, 784.63…
    ## $ cvd19male     <dbl> 371.5185, 115.8910, 226.1503, 410.1931, 129.3734, 760.37…
    ## $ cvd05both     <dbl> 479.0988, 114.4628, 212.7781, 460.3967, 148.9638, 725.85…
    ## $ cvd06both     <dbl> 469.5897, 112.7694, 207.4933, 448.1819, 142.8732, 735.05…
    ## $ cvd07both     <dbl> 460.6100, 110.3326, 207.1775, 430.5289, 139.8263, 728.35…
    ## $ cvd08both     <dbl> 452.3262, 109.3167, 196.6322, 425.2376, 136.2677, 724.46…
    ## $ cvd09both     <dbl> 444.6443, 109.3833, 192.6343, 415.1551, 129.7996, 722.60…
    ## $ cvd10both     <dbl> 437.7233, 109.1886, 192.5741, 405.3203, 124.4187, 725.79…
    ## $ cvd11both     <dbl> 433.4500, 109.4555, 190.0731, 393.6405, 120.5740, 732.17…
    ## $ cvd12both     <dbl> 426.8407, 108.8728, 186.7114, 380.1264, 115.1855, 739.57…
    ## $ cvd13both     <dbl> 418.70568, 108.79150, 184.52990, 364.79531, 111.24268, 7…
    ## $ cvd14both     <dbl> 415.16724, 108.34739, 181.88855, 360.00518, 110.03050, 7…
    ## $ cvd15both     <dbl> 412.8064, 108.1292, 183.4215, 359.2512, 109.2401, 756.21…
    ## $ cvd16both     <dbl> 405.90226, 107.56173, 189.41453, 351.23256, 106.16151, 7…
    ## $ cvd17both     <dbl> 401.39469, 106.95304, 190.14065, 349.47783, 106.36798, 7…
    ## $ cvd18both     <dbl> 399.50515, 106.39570, 185.28922, 348.19153, 107.86810, 7…
    ## $ cvd19both     <dbl> 396.96522, 105.71185, 183.93309, 345.49496, 108.12643, 6…
    ## $ LEbirth2000   <dbl> 73.5, NA, 77.8, 74.9, 82.2, 68.5, 74.6, 75.2, 75.0, 53.1…
    ## $ LE602000      <dbl> 20.3, NA, 22.5, 20.2, 24.9, 17.9, 19.4, 21.3, 19.2, 16.6…
    ## $ LEbirth2010   <dbl> 76.8, NA, 78.6, 76.6, 84.0, 71.8, 76.4, 77.7, 77.2, 60.8…
    ## $ LE602010      <dbl> 21.9, NA, 22.7, 20.5, 26.1, 17.6, 20.5, 22.4, 20.9, 17.4…
    ## $ LEbirth2015   <dbl> 77.5, NA, 79.3, 77.8, 84.2, 73.4, 78.8, 78.6, 78.0, 63.4…
    ## $ LE602015      <dbl> 22.3, NA, 23.0, 21.2, 26.3, 17.9, 21.9, 23.0, 21.5, 17.8…
    ## $ LEbirth2019   <dbl> 78.1, NA, 79.5, 79.2, 84.8, 74.1, 79.6, 79.4, 78.6, 65.2…
    ## $ LE602019      <dbl> 22.6, NA, 23.1, 22.1, 26.8, 18.3, 22.5, 23.5, 22.0, 18.1…
    ## $ phy05         <dbl> 1.0242, 3.2319, 3.2100, 2.5643, 2.7794, 3.5819, 3.3145, …
    ## $ phy06         <dbl> NA, 3.0123, NA, 2.5897, 2.8338, 3.5649, 3.3795, 1.7007, …
    ## $ phy07         <dbl> 1.1958, 3.0109, NA, 2.6724, 2.9954, 3.7124, 3.5306, 1.73…
    ## $ phy08         <dbl> NA, NA, NA, 2.7432, 3.0056, 3.6844, 3.1477, 1.7802, 3.64…
    ## $ phy09         <dbl> NA, 3.1479, NA, 2.7679, 3.1085, 3.6751, 3.2845, 1.8171, …
    ## $ phy10         <dbl> 1.2070, 4.0000, 3.2100, 2.8419, 3.3429, 3.6629, 3.2498, …
    ## $ phy11         <dbl> NA, NA, NA, 2.8677, 3.2878, 3.4376, 4.7611, 1.8491, 3.84…
    ## $ phy12         <dbl> NA, NA, NA, 2.8968, 3.2858, 3.4901, 4.8352, NA, 3.8995, …
    ## $ phy13         <dbl> NA, NA, 3.9385, 2.9031, 3.3530, 3.4558, 4.8534, 1.8820, …
    ## $ phy14         <dbl> NA, NA, NA, 2.8928, 3.4314, 3.4460, 5.0120, NA, 3.9750, …
    ## $ phy15         <dbl> NA, 3.3333, NA, 2.9143, 3.4886, NA, 5.1905, NA, 4.0332, …
    ## $ phy16         <dbl> 1.8325, NA, 4.0013, NA, 3.5672, NA, NA, NA, NA, 0.0645, …
    ## $ phy17         <dbl> 1.7879, NA, 3.9901, 4.4023, 3.6778, NA, NA, 2.1652, NA, …
    ## $ HE05          <chr> "3.2351613000000001", "5.5754260999999996", "7.610788819…
    ## $ HE06          <chr> "3.35510325", "4.9350943599999999", "7.6403284100000004"…
    ## $ HE07          <chr> "3.8214178099999998", "4.9255475999999998", "7.834550860…
    ## $ HE08          <chr> "4.2018823599999999", "5.8059859300000003", "8.182697299…
    ## $ HE09          <chr> "5.3593983700000001", "6.2023372700000001", "9.455995559…
    ## $ HE10          <chr> "5.1171698599999997", "6.64963865", "9.4454641299999995"…
    ## $ HE11          <chr> "5.2674808500000001", "6.2465286300000002", "9.418198589…
    ## $ HE12          <chr> "6.00050974", "6.1015033699999996", "9.8272666900000001"…
    ## $ HE13          <chr> "6.0357627899999997", "5.9878034600000003", "9.780979159…
    ## $ HE14          <chr> "6.5472140300000001", "5.9791245499999999", "9.67129993"…
    ## $ HE15          <chr> "6.9784917799999997", "6.2324533500000001", "10.22933768…
    ## $ HE16          <chr> "6.60778189", "6.3434934600000004", "9.0018968600000004"…
    ## $ HE17          <chr> "6.38032866", "6.5443186799999999", "10.457043649999999"…
    ## $ scl05         <dbl> 6.9, 9.8, 9.1, 10.9, 11.7, 10.7, 9.3, 6.3, 10.2, 1.3, 12…
    ## $ scl06         <dbl> 7.0, 10.1, 9.0, 10.9, 11.9, 10.7, 10.0, 6.4, 10.4, 1.3, …
    ## $ scl07         <dbl> 7.2, 10.1, 8.9, 11.0, 12.0, 10.2, 10.6, 6.5, 10.5, 1.3, …
    ## $ scl08         <dbl> 6.7, 10.1, 8.9, 11.0, 12.3, 10.2, 11.3, 6.7, 10.5, 1.3, …
    ## $ scl09         <dbl> 6.9, 10.1, 9.3, 11.1, 12.3, 10.7, 11.9, 6.8, 10.6, 1.4, …
    ## $ scl10         <dbl> 7.1, 10.1, 9.8, 11.1, 12.4, 10.7, 12.0, 6.9, 10.6, 1.4, …
    ## $ scl11         <dbl> 7.4, 10.2, 9.8, 11.2, 12.5, 10.7, 12.0, 7.1, 10.7, 1.4, …
    ## $ scl12         <dbl> 7.6, 10.2, 9.8, 11.3, 12.6, 10.7, 12.0, 7.3, 10.8, 1.4, …
    ## $ scl13         <dbl> 7.8, 10.2, 9.8, 11.4, 12.6, 10.8, 12.0, 7.4, 10.9, 1.4, …
    ## $ scl14         <dbl> 7.9, 10.2, 9.8, 11.5, 12.7, 10.7, 12.1, 7.4, 10.9, 1.4, …
    ## $ scl15         <dbl> 7.9, 10.2, 9.8, 11.6, 12.8, 10.7, 12.2, 7.6, 11.8, 1.4, …
    ## $ scl16         <dbl> 8.0, 10.2, 9.9, 11.7, 12.9, 10.7, 12.3, 7.8, 11.8, 1.5, …
    ## $ scl17         <dbl> 8.0, 10.2, 9.9, 11.7, 12.9, 10.7, 12.3, 7.8, 11.8, 1.5, …
    ## $ GDP05         <dbl> 3113.0953, 40066.2569, 5109.8513, 1643.7530, 33999.2429,…
    ## $ GDP06         <dbl> 3478.7109, 42675.8128, 5919.0120, 2158.1437, 36044.9228,…
    ## $ GDP07         <dbl> 3946.6645, 47803.6936, 7245.4483, 3139.2775, 40960.0545,…
    ## $ GDP08         <dbl> 4923.8432, 48718.4969, 9020.8731, 4010.8572, 49601.6567,…
    ## $ GDP09         <dbl> 3883.1324, 43503.1855, 8225.1372, 2994.3425, 42772.3592,…
    ## $ GDP10         <dbl> 4479.3417, 40852.6668, 10385.9644, 3218.3727, 52022.1256…
    ## $ GDP11         <dbl> 5462.2609, 43335.3289, 12848.8642, 3525.8047, 62517.8337…
    ## $ GDP12         <dbl> 5591.2124, 38686.4613, 13082.6643, 3681.8575, 68012.1479…
    ## $ GDP13         <dbl> 5498.7841, 39538.7667, 13080.2547, 3838.1858, 68150.1070…
    ## $ GDP14         <dbl> 5494.3523, 41303.9294, 12334.7982, 3986.2316, 62510.7912…
    ## $ GDP15         <dbl> 4187.5097, 35762.5231, 13789.0604, 3607.2967, 56755.7217…
    ## $ GDP16         <dbl> 3945.4821, 37474.6654, 12790.2425, 3591.8293, 49971.1315…
    ## $ GDP17         <dbl> 4111.2941, 38962.8804, 14613.0418, 3914.5013, 54027.9668…
    ## $ MMR05         <dbl> 127, NA, 59, 35, 5, 42, 11, 71, 15, 437, 11, 25, 44, 83,…
    ## $ MMR06         <dbl> 122, NA, 57, 36, 5, 35, 9, 72, 14, 422, 11, 25, 42, 82, …
    ## $ MMR07         <dbl> 119, NA, 56, 32, 5, 34, 7, 71, 13, 410, 11, 23, 40, 83, …
    ## $ MMR08         <dbl> 117, NA, 53, 36, 5, 32, 6, 70, 13, 401, 12, 21, 40, 84, …
    ## $ MMR09         <dbl> 117, NA, 56, 32, 5, 32, 6, 69, 12, 393, 12, 21, 37, 87, …
    ## $ MMR10         <dbl> 115, NA, 51, 32, 5, 31, 5, 65, 12, 385, 11, 20, 36, 85, …
    ## $ MMR11         <dbl> 116, NA, 48, 30, 6, 30, 5, 61, 12, 377, 11, 18, 34, 84, …
    ## $ MMR12         <dbl> 116, NA, 47, 30, 6, 29, 4, 60, 11, 369, 11, 17, 33, 85, …
    ## $ MMR13         <dbl> 115, NA, 44, 26, 6, 28, 3, 61, 10, 362, 11, 16, 32, 85, …
    ## $ MMR14         <dbl> 114, NA, 42, 27, 6, 28, 3, 62, 11, 353, 11, 15, 31, 85, …
    ## $ MMR15         <dbl> 114, NA, 41, 28, 6, 27, 3, 63, 10, 343, 11, 14, 30, 85, …
    ## $ MMR16         <dbl> 113, NA, 40, 26, 6, 26, 3, 62, 10, 331, 10, 13, 29, 84, …
    ## $ MMR17         <dbl> 112, NA, 39, 26, 6, 26, 2, 60, 10, 320, 10, 13, 29, 83, …
    ## $ LMH09         <fct> upper middle income, high income, upper middle income, l…
    ## $ LMH14         <fct> upper middle income, high income, high income, lower mid…

\#\#examine missing values and insert replacements

\#\#\#2014

``` r
#examine NAs for 2014
edit <- cbind(gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh) #make new table for editing (cbind) means a duplicate will be made otherwise changing 'edit' will also change the gsni table


na.omit(edit, c("cvd14fem", "cvd14both", "LEbirth2015", "phy14", "scl14", "GDP14", "MMR14"), invert = TRUE) #28 rows with one missing value for 2014
```

    ##                 location_name GSNI_PERIOD onebias twobias nobias political
    ##  1:                   Algeria   2010–2014   97.83   87.00   2.17     80.08
    ##  2:                   Andorra   2005–2009   27.01    7.43  72.99     14.08
    ##  3:                 Argentina   2010–2014   75.41   42.49  24.59     43.35
    ##  4:                    Brazil   2010–2014   89.50   52.39  10.50     43.41
    ##  5:                   Ecuador   2010–2014   93.34   58.90   6.66     46.34
    ##  6:                  Ethiopia   2005–2009   85.27   35.14  14.73     30.27
    ##  7:                     Ghana   2010–2014   99.16   92.69   0.84     86.84
    ##  8:                     Haiti   2010–2014   98.91   92.82   1.09     76.33
    ##  9:                 Indonesia   2005–2009   97.44   80.36   2.56     66.47
    ## 10: Iran, Islamic Republic of   2005–2009   98.54   92.49   1.46     84.63
    ## 11:       Korea (Republic of)   2010–2014   87.07   62.91  12.93     63.68
    ## 12:                Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80
    ## 13:                  Malaysia   2010–2014   98.54   88.38   1.46     79.69
    ## 14:                      Mali   2005–2009   98.82   93.36   1.18     81.89
    ## 15:      Moldova, Republic of   2005–2009   90.06   67.21   9.94     60.33
    ## 16:                   Nigeria   2010–2014   99.73   94.99   0.27     85.83
    ## 17:       Palestine, State of   2010–2014   98.00   92.30   2.00     89.30
    ## 18:                      Peru   2010–2014   87.96   49.99  12.04     38.44
    ## 19:               Philippines   2010–2014   98.87   86.80   1.13     70.62
    ## 20:                   Romania   2010–2014   85.50   60.84  14.50     48.78
    ## 21:        Russian Federation   2010–2014   86.83   68.56  13.17     68.43
    ## 22:                  Thailand   2010–2014   95.47   74.50   4.53     67.30
    ## 23:       Trinidad and Tobago   2010–2014   85.99   51.25  14.01     39.14
    ## 24:            United Kingdom   2005–2009   54.60   25.50  45.40     26.07
    ## 25:                   Uruguay   2010–2014   74.60   36.70  25.40     28.60
    ## 26:                  Viet Nam   2005–2009   92.89   69.17   7.11     59.40
    ## 27:                     Yemen   2010–2014   97.80   92.10   2.20     87.40
    ## 28:                    Zambia   2005–2009   96.84   80.56   3.16     66.04
    ##                 location_name GSNI_PERIOD onebias twobias nobias political
    ##     economic educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem
    ##  1:    74.08       37.17    86.75 500.26865 494.78751 490.17488 486.31725
    ##  2:     8.73        1.81    12.01  94.99074  94.10549  92.00298  91.82684
    ##  3:    30.43       17.04    52.86 169.99370 165.87751 165.37005 156.11837
    ##  4:    36.63        9.32    77.95 192.71662 188.83657 184.09958 179.74663
    ##  5:    36.44       23.46    84.36 161.28268 164.29742 163.35005 160.95424
    ##  6:    22.00        8.00    80.60 261.82080 255.18410 248.43715 242.03145
    ##  7:    78.01       30.02    90.73 325.79930 328.95890 329.44442 331.09718
    ##  8:    72.06       59.91    88.13 526.03693 523.13684 519.11249 514.70529
    ##  9:    66.40       19.31    90.55 369.65859 371.69064 371.74037 373.60996
    ## 10:    88.86       55.42    78.69 346.45487 333.03926 318.53806 305.98136
    ## 11:    54.33       25.67    58.27 155.91408 143.80539 132.67130 120.85549
    ## 12:    71.53       41.00    81.73 546.88719 553.71417 543.25326 536.85221
    ## 13:    74.54       43.00    94.31 278.24360 271.41947 262.56217 262.28229
    ## 14:    88.87       47.61    84.87 320.56474 319.77363 317.97481 317.98693
    ## 15:    58.80       16.73    65.20 513.32695 491.99307 497.11512 484.52829
    ## 16:    83.42       46.18    92.78 277.35636 271.85405 263.52070 259.79173
    ## 17:    79.50       26.70    83.50 367.37503 362.07740 355.82036 350.58586
    ## 18:    27.05       14.36    79.76  97.82452  91.17324  84.14951  83.74437
    ## 19:    73.80       39.08    91.48 293.05106 293.56564 287.95050 287.91056
    ## 20:    55.88       20.69    63.54 458.08365 437.32047 413.66866 400.71262
    ## 21:    58.77       22.66    50.02 569.51494 527.82419 500.58817 492.70377
    ## 22:    50.86       29.02    84.53 148.01004 142.41904 137.40121 132.15039
    ## 23:    37.74        5.61    72.17 244.42399 237.87706 229.26124 224.70676
    ## 24:    25.15        6.65    30.34 147.03415 138.97711 132.69706 127.54333
    ## 25:    34.30        9.20    51.40 181.89537 171.40400 172.51409 160.79502
    ## 26:    62.49       20.36    70.56 256.62741 256.62311 255.97453 255.19294
    ## 27:    87.20       45.30    81.00 493.05999 489.24403 485.10075 483.33838
    ## 28:    55.41       23.53    89.07 301.19131 304.24329 301.25347 299.44540
    ##     economic educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem
    ##      cvd09fem  cvd10fem  cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem
    ##  1: 483.19933 484.49190 484.48211 479.07575 472.94269 471.02644 468.44578
    ##  2:  93.03548  93.64847  94.72272  94.77537  95.38794  95.79920  96.76678
    ##  3: 153.96832 154.88168 152.74845 149.31115 147.59175 146.19661 147.86150
    ##  4: 176.62840 172.89359 169.38552 163.58428 159.43643 155.71901 153.99486
    ##  5: 157.69322 157.35397 149.29192 153.73253 152.93464 152.05617 148.90913
    ##  6: 236.24831 231.66182 230.61082 229.20585 225.91810 224.28592 223.92249
    ##  7: 332.15021 332.89254 332.14372 330.11120 328.64703 326.15716 329.48438
    ##  8: 510.66940 508.21671 512.24372 507.65492 502.18482 497.94515 495.34101
    ##  9: 373.65398 372.87334 371.44090 372.29798 370.74477 368.80629 368.52390
    ## 10: 298.01274 290.36702 283.44570 278.28489 277.07263 275.18836 277.67423
    ## 11: 111.78620 105.34493 100.12794  95.58209  90.21631  85.58348  83.53081
    ## 12: 507.41753 488.18726 483.29378 465.05092 442.79747 429.36559 435.06853
    ## 13: 263.15505 252.21083 237.09570 233.23215 226.51494 228.10326 227.29230
    ## 14: 317.14954 316.24847 314.96309 313.19762 310.27834 307.23772 309.21589
    ## 15: 467.91012 459.10667 400.91032 387.36233 372.28763 377.82255 382.27748
    ## 16: 257.07280 254.32157 253.99749 254.09391 253.49508 249.71126 252.45876
    ## 17: 343.97033 341.78855 337.69661 320.17827 314.63758 323.39695 338.52757
    ## 18:  93.03498  94.03452  91.93528  89.98908  87.97633  83.77122  80.26885
    ## 19: 287.31183 284.21639 281.45974 280.20698 277.95194 274.33572 275.99697
    ## 20: 397.43462 386.99741 365.99837 359.62475 340.34775 338.83849 330.12565
    ## 21: 467.54550 459.66489 424.43583 406.44126 391.62857 386.28781 376.79609
    ## 22: 125.40487 122.07400 115.60825 109.16780 103.96458 100.77038  97.84838
    ## 23: 212.68349 204.48233 192.87097 183.77914 188.94522 190.52675 189.97215
    ## 24: 120.00812 115.83452 112.45472 112.30358 111.23225 108.82079 108.30317
    ## 25: 150.85425 147.47007 150.67799 146.49314 140.61544 136.51041 136.27482
    ## 26: 254.46217 252.55867 250.17926 247.05521 243.05616 239.21285 234.94030
    ## 27: 478.34072 472.17590 471.35922 468.99902 467.16132 464.47411 466.86730
    ## 28: 297.43078 300.60435 303.67459 305.16350 306.41551 307.02384 308.97725
    ##      cvd09fem  cvd10fem  cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem
    ##      cvd16fem  cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male
    ##  1: 458.09789 450.74425 451.34683 447.67418  462.4656  450.5957  439.3599
    ##  2:  96.28366  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052
    ##  3: 153.18030 154.09804 150.26997 149.24294  265.4632  258.6674  258.7628
    ##  4: 153.37633 149.03982 148.29788 146.95291  269.7419  264.7693  259.8278
    ##  5: 149.01126 149.63616 148.94035 146.37173  195.4146  197.5540  198.2290
    ##  6: 222.61808 221.95432 222.81058 223.55265  276.7356  271.0027  264.5905
    ##  7: 327.68822 326.79742 324.50422 324.16728  308.0771  311.7312  310.7842
    ##  8: 489.48808 483.52984 479.46926 475.27316  431.5242  430.9686  431.2913
    ##  9: 365.10278 361.90945 358.11468 354.06925  386.6718  391.2950  397.2689
    ## 10: 278.51259 277.88105 271.04348 268.72894  400.2154  389.5281  376.5086
    ## 11:  81.91117  81.88305  82.38176  83.11824  190.0032  178.2896  167.8587
    ## 12: 415.09331 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687
    ## 13: 224.59851 222.86437 221.09851 220.62850  326.1480  318.3576  312.0718
    ## 14: 307.72590 305.13664 304.19901 301.76476  250.4399  251.6409  253.7490
    ## 15: 381.40961 368.12912 361.05127 341.11337  702.8658  671.8424  666.6718
    ## 16: 247.75393 245.20993 242.25928 239.99721  274.4590  267.8654  259.5376
    ## 17: 350.93555 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858
    ## 18:  78.92477  78.51316  77.75822  77.12523  118.1332  114.0556  109.6057
    ## 19: 273.52319 271.92029 269.31716 266.26944  375.8930  378.2835  374.6201
    ## 20: 320.96970 317.98703 317.44903 319.76750  594.6991  574.2798  550.0335
    ## 21: 366.11070 348.59567 349.51106 351.22730  956.8934  876.9782  830.0730
    ## 22:  97.65457  96.75148  96.78591  96.50541  179.0414  171.3873  164.7609
    ## 23: 194.01692 194.33779 194.26330 193.63737  339.4050  312.2339  301.5650
    ## 24: 106.14944 104.51284 106.93075 107.12455  224.9909  213.8383  203.5031
    ## 25: 133.88372 134.15097 133.06846 132.74195  264.6658  253.9937  254.2766
    ## 26: 230.61406 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026
    ## 27: 466.88438 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200
    ## 28: 311.38853 308.67000 306.10250 303.60330  396.9908  395.9378  389.5186
    ##      cvd16fem  cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male
    ##     cvd08male cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male
    ##  1:  428.9066  419.1618  408.5190  403.3053  397.5104  389.1503  384.7680
    ##  2:  126.3548  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398
    ##  3:  246.9013  240.5962  239.1611  236.2446  233.3321  230.6517  226.4522
    ##  4:  255.9295  252.0371  248.5607  244.3169  236.3190  231.1611  225.6046
    ##  5:  197.6351  191.5033  191.5863  191.7630  187.9508  186.7919  185.9278
    ##  6:  258.7780  252.8036  245.2730  242.6073  240.1706  236.7341  234.6738
    ##  7:  310.4548  309.2160  308.0875  307.7036  305.1269  301.1806  296.2265
    ##  8:  431.2199  430.6858  431.0238  433.7781  433.2272  432.0381  430.4305
    ##  9:  402.7375  406.8324  409.3400  411.8711  416.3984  418.3803  419.7302
    ## 10:  362.0445  350.5229  338.1660  323.1285  311.8184  304.8604  299.1565
    ## 11:  158.2836  149.4912  143.7630  137.6913  131.7820  123.9082  117.5575
    ## 12:  761.4547  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342
    ## 13:  316.5872  315.2901  303.3995  295.3241  285.1897  264.2568  267.6031
    ## 14:  256.6272  259.4479  259.6777  258.0625  256.5280  255.5163  253.8127
    ## 15:  648.2151  638.1296  641.1604  572.9933  565.3869  534.3178  552.1560
    ## 16:  255.2531  252.9699  250.7906  250.3251  250.0944  248.4156  244.0515
    ## 17:  484.7670  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364
    ## 18:  107.1766  117.1724  120.0147  118.2343  116.0098  113.0409  107.4914
    ## 19:  378.4958  380.0939  378.0533  378.7095  380.0489  377.3414  376.7860
    ## 20:  547.3480  543.1993  535.6756  501.2370  496.4683  468.9440  476.7701
    ## 21:  825.6957  776.4243  770.5636  706.6609  675.9903  650.0326  646.2977
    ## 22:  159.7038  155.0085  153.1022  147.5108  142.4819  139.5502  139.3282
    ## 23:  311.0354  288.0985  279.1031  263.7642  248.9993  253.3945  255.4942
    ## 24:  195.1599  185.3431  178.1320  171.7848  168.7043  167.4859  163.1700
    ## 25:  236.7589  226.8973  220.8039  222.2131  216.9713  210.2661  203.4770
    ## 26:  455.8597  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765
    ## 27:  566.4953  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702
    ## 28:  383.8555  377.2154  376.4333  380.5640  379.4144  379.6064  381.3379
    ##     cvd08male cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male
    ##     cvd15male cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both
    ##  1:  383.2925  379.8222  377.6635  373.4875  371.5185  479.0988  469.5897
    ##  2:  119.3653  118.6682  117.7883  116.9475  115.8910  114.4628  112.7694
    ##  3:  227.7888  234.6520  235.0038  228.2053  226.1503  212.7781  207.4933
    ##  4:  222.4439  222.8537  215.6675  211.9121  210.6516  227.7234  223.2779
    ##  5:  184.5895  186.8145  189.4857  189.0448  182.2878  177.6049  180.0888
    ##  6:  233.0782  230.5664  228.5969  228.0727  227.3755  269.7873  263.5363
    ##  7:  297.1589  294.3840  292.5341  288.0546  285.0981  320.4876  323.9869
    ##  8:  429.9405  426.5772  424.1333  423.1650  419.7853  481.0125  479.0757
    ##  9:  420.4248  418.4559  416.6262  414.8512  412.4565  378.9587  382.2896
    ## 10:  300.1241  297.2979  294.9042  290.2879  288.5816  374.2741  361.9923
    ## 11:  114.1778  110.9073  110.6175  108.2689  108.8687  172.0355  160.0969
    ## 12:  644.2163  618.5638  613.3843  580.3176  569.3491  634.0089  651.1682
    ## 13:  268.1236  275.7751  278.4519  279.6520  290.3302  304.0469  296.6711
    ## 14:  257.2386  257.6357  257.6290  255.5112  252.2948  284.7410  284.8986
    ## 15:  587.3132  566.6405  523.8055  511.8000  498.5963  589.7769  564.1553
    ## 16:  246.2866  241.7979  238.7568  235.4675  232.2572  278.5825  272.1292
    ## 17:  442.4321  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939
    ## 18:  103.9618  102.7772  102.2740  101.7696  101.1460  107.5290  102.1196
    ## 19:  376.6431  373.1642  366.5366  359.5821  354.9366  331.7122  332.2420
    ## 20:  469.2057  463.8929  457.5351  455.3877  454.2815  519.7798  499.0081
    ## 21:  623.4263  604.2681  561.3748  549.7797  549.1739  727.6035  670.5814
    ## 22:  140.4503  143.4768  143.6757  144.4356  144.1269  163.4339  156.8782
    ## 23:  257.6763  263.8739  264.4925  263.7471  263.2442  289.8596  274.1400
    ## 24:  161.8943  159.0024  157.5910  160.8536  161.2392  181.8112  172.5648
    ## 25:  201.5381  198.5235  198.8982  198.1326  197.6541  218.9647  208.2510
    ## 26:  443.7173  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238
    ## 27:  536.2482  536.1051  541.0473  547.2489  550.6548  535.1088  530.9694
    ## 28:  385.9456  390.7901  386.0968  382.3781  377.5337  348.3714  349.1627
    ##     cvd15male cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both
    ##     cvd07both cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both
    ##  1: 460.60996 452.32621  444.6443  437.7233  433.4500  426.8407  418.7057
    ##  2: 110.33258 109.31671  109.3833  109.1886  109.4555  108.8728  108.7915
    ##  3: 207.17746 196.63216  192.6343  192.5741  190.0731  186.7114  184.5299
    ##  4: 218.36193 214.14929  210.6505  206.9642  203.0646  196.2723  191.6573
    ##  5: 179.95002 178.65734  174.1443  173.9953  169.8008  170.3395  169.4053
    ##  6: 256.88434 250.72570  244.8102  238.6314  236.7101  234.7540  231.3693
    ##  7: 323.92028 324.82630  324.9608  324.9526  324.4065  322.1898  319.7047
    ##  8: 477.03676 474.64371  472.2518  471.2477  474.8282  472.1792  468.7702
    ##  9: 385.13312 388.81052  390.8450  391.6793  392.1266  394.7447  394.8640
    ## 10: 347.98144 334.19398  324.1761  313.9673  302.7896  294.4027  290.2060
    ## 11: 148.94438 137.66707  128.5412  122.2008  116.6089  111.6049  105.3768
    ## 12: 645.59071 632.76151  603.3141  581.8522  562.6225  543.9251  519.5371
    ## 13: 289.07676 291.31778  291.2486  279.5220  267.4026  260.4545  246.4554
    ## 14: 285.02779 286.46916  287.4945  287.1683  285.6945  284.0338  282.0897
    ## 15: 566.45157 552.62338  539.0321  535.4923  471.5840  460.0871  438.8196
    ## 16: 263.44130 259.09633  256.2984  253.4984  252.8035  252.4680  251.0900
    ## 17: 415.89329 407.15878  399.0904  394.3262  387.9774  366.8452  355.9001
    ## 18:  96.34764  94.97714  104.6293  106.4933  104.5499  102.4466   99.9540
    ## 19: 327.14809 328.81843  329.1745  326.6859  325.5405  325.4797  322.9125
    ## 20: 475.04244 466.40175  462.7888  453.2380  426.2066  420.4295  397.5164
    ## 21: 635.16266 628.21607  592.7751  585.0586  537.7079  514.5833  495.2656
    ## 22: 151.11043 145.88745  139.9720  137.2788  131.1732  125.2963  121.0188
    ## 23: 264.62657 266.62697  249.3102  240.6988  227.3527  215.5430  220.2906
    ## 24: 164.68453 158.18593  149.6043  144.0946  139.3859  137.9374  136.8413
    ## 25: 208.91637 194.57793  184.5496  180.0692  182.5667  177.8057  171.3896
    ## 26: 336.52387 337.02684  337.1512  335.4539  333.0107  329.9312  326.6030
    ## 27: 525.70885 523.71933  517.1861  508.7890  507.4918  504.3873  501.9957
    ## 28: 344.28588 340.42203  336.0299  337.2284  340.7799  340.9955  341.7355
    ##     cvd07both cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both
    ##     cvd14both cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000
    ##  1: 415.16724 412.80644 405.90226 401.39469 399.50515 396.96522        73.5
    ##  2: 108.34739 108.12924 107.56173 106.95304 106.39570 105.71185          NA
    ##  3: 181.88855 183.42152 189.41453 190.14065 185.28922 183.93309        77.8
    ##  4: 187.13184 184.72431 184.51530 178.94359 176.98645 175.66145        75.2
    ##  5: 168.52380 166.17046 167.16092 168.67782 168.10363 163.65500        77.3
    ##  6: 229.49538 228.51537 226.57527 225.23902 225.38551 225.38539        51.7
    ##  7: 316.18344 318.52444 316.32848 315.05671 311.95072 310.60655        60.8
    ##  8: 465.82635 464.27802 459.66137 455.40585 452.87085 449.11377        57.2
    ##  9: 394.48450 394.73382 392.00034 389.44109 386.56637 383.26115        68.6
    ## 10: 286.31401 287.96550 286.88535 285.32918 279.68642 277.73359        74.6
    ## 11: 100.13781  97.49873  95.22265  95.17711  94.68896  95.40530        79.8
    ## 12: 513.01612 523.15984 500.14376 493.82082 473.26712 466.31300        70.2
    ## 13: 248.03609 247.15687 249.70380 250.28741 250.11953 255.54454        75.3
    ## 14: 279.73392 282.44832 281.89484 280.58319 279.03154 276.18486        52.7
    ## 15: 449.03836 463.27943 456.27984 432.82716 423.77893 404.95508        70.5
    ## 16: 246.82989 249.11815 244.38574 241.54042 238.38554 235.65202        55.2
    ## 17: 368.85654 380.88863 384.46844 388.17552 386.67955 381.69777          NA
    ## 18:  95.08345  91.54823  90.28000  89.85563  89.23082  88.61384        75.9
    ## 19: 320.55214 321.36521 318.50800 315.14778 310.89918 307.32206        72.9
    ## 20: 400.02892 391.65760 384.18410 379.99900 378.78972 379.62640        74.9
    ## 21: 490.38527 475.65621 461.71309 434.85592 431.69136 432.91864        72.3
    ## 22: 119.09116 117.90308 119.14971 118.67228 118.96768 118.66561        75.2
    ## 23: 222.12845 222.78825 227.76604 228.23401 227.86298 227.29800        72.6
    ## 24: 133.69815 132.99741 130.60903 129.12143 132.02638 132.37531          NA
    ## 25: 165.97775 164.96208 162.32582 162.77541 161.88740 161.55863        78.8
    ## 26: 323.18282 319.13626 314.95461 311.23748 307.70802 303.90466        75.6
    ## 27: 497.81417 500.90386 500.85723 504.82748 509.87687 513.09104        64.7
    ## 28: 342.88671 346.10827 349.73020 346.15135 343.09096 339.55436        45.2
    ##     cvd14both cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000
    ##     LE602000 LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019
    ##  1:     20.3        76.8     21.9        77.5     22.3        78.1     22.6
    ##  2:       NA          NA       NA          NA       NA          NA       NA
    ##  3:     22.5        78.6     22.7        79.3     23.0        79.5     23.1
    ##  4:     21.3        77.7     22.4        78.6     23.0        79.4     23.5
    ##  5:     23.1        77.9     22.8        79.5     23.6        80.5     24.3
    ##  6:     14.9        64.5     18.3        68.6     19.1        70.5     19.4
    ##  7:     17.3        64.5     17.9        67.4     18.6        69.2     18.9
    ##  8:     15.9        35.4     12.0        63.1     16.7        64.8     17.0
    ##  9:     18.2        71.0     18.4        72.5     18.8        73.3     19.1
    ## 10:     20.5        78.0     22.3        78.5     22.3        79.1     22.5
    ## 11:     22.7        83.8     25.9        85.1     26.9        86.1     27.9
    ## 12:     18.3        73.3     19.3        75.3     20.2        77.3     21.7
    ## 13:     19.2        76.5     20.1        77.1     20.7        77.1     20.6
    ## 14:     15.9        59.5     17.0        61.4     17.2        63.4     17.6
    ## 15:     17.2        73.1     18.2        75.0     19.5        77.1     20.9
    ## 16:     17.0        60.8     18.0        62.8     18.5        64.1     18.9
    ## 17:       NA          NA       NA          NA       NA          NA       NA
    ## 18:     22.9        78.9     23.8        80.6     24.7        81.3     25.1
    ## 19:     19.7        73.5     19.9        73.7     19.7        73.6     19.6
    ## 20:     19.7        77.4     21.1        78.4     21.9        79.3     22.4
    ## 21:     18.7        74.7     20.2        76.6     21.4        78.0     22.2
    ## 22:     22.1        79.3     23.8        80.7     24.7        81.0     24.8
    ## 23:     19.8        75.7     21.5        78.3     23.7        79.9     25.0
    ## 24:       NA          NA       NA          NA       NA          NA       NA
    ## 25:     23.1        80.0     23.7        80.6     24.1        80.6     24.0
    ## 26:     21.0        77.1     21.5        77.6     21.8        78.1     22.0
    ## 27:     17.9        69.6     18.7        69.6     18.9        68.9     18.7
    ## 28:     14.7        59.1     17.1        63.0     17.7        65.4     18.0
    ##     LE602000 LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019
    ##      phy05  phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14
    ##  1: 1.0242     NA 1.1958     NA     NA 1.2070     NA     NA     NA     NA
    ##  2: 3.2319 3.0123 3.0109     NA 3.1479 4.0000     NA     NA     NA     NA
    ##  3: 3.2100     NA     NA     NA     NA 3.2100     NA     NA 3.9385     NA
    ##  4: 1.6663 1.7007 1.7306 1.7802 1.8171 1.8139 1.8491     NA 1.8820     NA
    ##  5:     NA     NA     NA     NA 1.5983 2.1111 1.6582     NA     NA     NA
    ##  6: 0.0321 0.0269 0.0224 0.0251 0.0252 0.0220     NA     NA     NA     NA
    ##  7:     NA     NA 0.0725 0.0787 0.0841 0.0938 0.0979 0.0956 0.1686     NA
    ##  8:     NA     NA     NA     NA     NA     NA 0.1378     NA     NA     NA
    ##  9:     NA 0.1300 0.2880     NA 0.1448 0.1395     NA 0.3075 0.3118     NA
    ## 10: 0.8869 0.5361     NA     NA     NA 0.8900     NA     NA     NA 1.5044
    ## 11: 1.7529 1.8047 1.8655 1.8407 1.9185 1.9839 2.0361 2.0798 2.1632 2.2070
    ## 12:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 13:     NA     NA     NA 0.9216     NA 1.1691 1.2777 1.3320     NA     NA
    ## 14:     NA     NA 0.0776 0.0490 0.0923 0.1021 0.1037 0.1071     NA     NA
    ## 15: 2.3783 2.3768 2.3871 2.3545 2.3912 2.3808 2.4142 2.4135 2.5028 2.4746
    ## 16: 0.2824 0.3481 0.3784 0.3762 0.3782 0.1836     NA     NA 0.3828     NA
    ## 17:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 18:     NA     NA 1.6648     NA 0.9472 0.9200     NA 1.1411     NA     NA
    ## 19: 1.2289 1.2361 1.2441 1.2558 1.2632 1.2717     NA     NA     NA     NA
    ## 20: 2.2126 2.2104 2.2915 2.4133 2.4414 2.4804 2.5153 2.5887 2.6240     NA
    ## 21: 2.3204 2.3721 2.3841 2.3812 2.3924 2.3930 6.6305 4.1303 4.0705 4.0114
    ## 22: 0.2930 0.2875 0.2959 0.3164 0.3387 0.3906     NA     NA     NA     NA
    ## 23:     NA 1.2222 1.1792     NA 1.5081 1.8086 1.8193     NA     NA     NA
    ## 24: 2.4120 2.4590 2.4844 2.5543 2.6279 2.6265 2.6603 2.6667 2.6774 2.7149
    ## 25:     NA     NA 4.1810 3.9510     NA 3.7360     NA     NA     NA     NA
    ## 26:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 27:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 28: 0.0545 0.0533     NA 0.0619 0.0606 0.0614 0.1649 0.1658     NA     NA
    ##      phy05  phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14
    ##      phy15  phy16  phy17               HE05               HE06
    ##  1:     NA 1.8325 1.7879 3.2351613000000001         3.35510325
    ##  2: 3.3333     NA     NA 5.5754260999999996 4.9350943599999999
    ##  3:     NA 4.0013 3.9901 7.6107888199999998 7.6403284100000004
    ##  4:     NA     NA 2.1652 8.0440778700000006 8.2589139899999999
    ##  5: 2.0648 2.0368     NA         5.57841349 5.6872220000000002
    ##  6:     NA 0.0464 0.0986 4.1009812400000003 4.4575729400000004
    ##  7:     NA 0.1270 0.1359 3.9777960800000001 3.9613141999999999
    ##  8: 0.0852     NA     NA 5.5093555500000004 5.5010376000000001
    ##  9: 0.2738     NA 0.3767         2.58409047         2.67202187
    ## 10: 1.1526     NA 1.1292         5.30572176 5.1986260399999997
    ## 11: 2.2494 2.3037 2.3608 4.6178216900000004 4.9439678200000001
    ## 12:     NA     NA     NA               <NA>               <NA>
    ## 13: 1.5358     NA     NA 2.8012416400000002 3.1245324600000002
    ## 14:     NA 0.1395     NA 5.1953811600000002 5.4565606100000004
    ## 15: 2.4836     NA 3.2066 7.9639606499999998 8.7857027100000007
    ## 16:     NA 0.4494     NA 4.4659194900000001 4.2577514599999997
    ## 17:     NA     NA     NA               <NA>               <NA>
    ## 18:     NA 1.3048     NA 4.5863194500000004 4.5380153700000001
    ## 19:     NA     NA 0.6004 3.9004185200000001 3.9458506099999999
    ## 20:     NA 2.2565 2.9807 5.5277814899999997 5.0710606599999997
    ## 21: 3.7494 4.0139     NA         4.76693487         4.76170969
    ## 22: 0.4651 0.4450 0.8075 3.1597218499999999 3.0997068900000002
    ## 23: 2.6498     NA 3.3646 4.4345116600000001 4.1543202399999997
    ## 24: 2.7465 2.7563 2.7863 8.5339870500000004 8.6973781599999995
    ## 25:     NA 3.9558 5.0794 8.4265871000000008 8.4113969799999992
    ## 26:     NA     NA     NA               <NA>               <NA>
    ## 27:     NA     NA     NA               <NA>               <NA>
    ## 28:     NA 0.1628     NA 6.8633222600000003 5.8736734400000001
    ##      phy15  phy16  phy17               HE05               HE06
    ##                   HE07               HE08               HE09               HE10
    ##  1: 3.8214178099999998 4.2018823599999999 5.3593983700000001 5.1171698599999997
    ##  2: 4.9255475999999998 5.8059859300000003 6.2023372700000001         6.64963865
    ##  3: 7.8345508600000002 8.1826972999999992 9.4559955599999999 9.4454641299999995
    ##  4: 8.2077617600000004 8.0159883500000007 8.4025468799999992 7.9491324399999996
    ##  5: 5.8648901000000002 5.8163766900000002 6.4387331000000003 7.1219563499999996
    ##  6: 5.0012836500000004 4.2806391699999997 4.6498341600000002 5.4663720099999997
    ##  7: 4.0661277800000004 4.1973075900000003 4.7066750500000003 4.6920676200000004
    ##  8: 5.9015803299999998 6.0159010899999998 6.1691598900000004 8.1455984099999998
    ##  9: 2.8761706399999998         2.61300087 2.6849434400000001 2.9608998299999998
    ## 10:         5.03977919 5.2816843999999996 6.5595455200000004 6.7547311800000003
    ## 11: 5.1149072599999998         5.39896727 5.7822899799999998 5.9173440900000003
    ## 12:               <NA>               <NA>               <NA>               <NA>
    ## 13:         3.08224082 3.0235738799999998 3.2762939900000001         3.18466234
    ## 14: 5.2536678300000004 5.0454120600000003 5.2291111900000002 4.6792340299999999
    ## 15: 9.1826009800000001 9.1230525999999994        11.39545822 10.131128309999999
    ## 16:         3.90997219 3.6958153199999999 3.5801973299999998 3.2965328700000001
    ## 17:               <NA>               <NA>               <NA>               <NA>
    ## 18: 4.4038014399999996 4.4459280999999997 4.9506058700000004 4.7206191999999998
    ## 19: 3.9195413600000002 4.0279226299999999 4.3535318399999996 4.3127627400000002
    ## 20: 5.0222334899999996 5.0227422700000002 5.2784128199999998 5.7690625200000003
    ## 21: 4.7431106600000001 4.8986678100000001 5.6382026700000001 4.9660777999999999
    ## 22: 3.1927750100000001 3.4593396200000002          3.6193974         3.39009976
    ## 23: 4.1536026000000001 3.6575703599999998         5.48676443         5.08042908
    ## 24: 8.8732242600000006 9.1662340199999992        10.01600361 9.9891519500000001
    ## 25: 8.1404838599999998 8.8268699599999998 8.6362743399999999 8.5942153900000005
    ## 26:               <NA>               <NA>               <NA>               <NA>
    ## 27:               <NA>               <NA>               <NA>               <NA>
    ## 28: 4.3526496899999998 4.0120754200000004         4.42680454 3.7192959800000001
    ##                   HE07               HE08               HE09               HE10
    ##                   HE11               HE12               HE13               HE14
    ##  1: 5.2674808500000001         6.00050974 6.0357627899999997 6.5472140300000001
    ##  2: 6.2465286300000002 6.1015033699999996 5.9878034600000003 5.9791245499999999
    ##  3: 9.4181985899999994 9.8272666900000001 9.7809791599999993         9.67129993
    ##  4: 7.7883334199999998         7.73536205 7.9772124299999998 8.3962497700000007
    ##  5: 7.8652181600000004 8.4796819699999997 8.5598344799999992 8.6212749500000001
    ##  6: 4.4689779300000003 4.5395960799999999 4.0750651400000004 4.0336551700000003
    ##  7: 4.7444992099999999 4.1872339199999997 4.6230053900000003 4.1004481300000002
    ##  8:         10.2313633 9.6682958600000006         7.23788214 7.7976360299999996
    ##  9: 2.9554202599999999 2.9028665999999999 2.9606506800000001 3.1171209800000002
    ## 10: 6.6072506899999999 6.6364860500000002         5.99379873 6.9135108000000001
    ## 11:         6.00844383         6.13262033 6.2478942899999996 6.4743809700000003
    ## 12:               <NA>               <NA>               <NA>               <NA>
    ## 13: 3.3399648700000002 3.4879069299999998 3.5224008599999999 3.7252702700000002
    ## 14: 4.0157337200000001 3.7521157299999999 3.9675071200000001 4.4815611799999999
    ## 15: 9.0967035299999992 9.1396207799999996 8.6830034299999994 8.6327571899999995
    ## 16: 3.3207793200000002 3.3598427800000001 3.4206934000000002 3.3484041699999998
    ## 17:               <NA>               <NA>               <NA>               <NA>
    ## 18: 4.6209321000000001 4.7565517399999999 4.7019739200000004 4.9947280899999997
    ## 19: 4.2060928300000002 4.3746528600000003          4.4638114 4.1274123200000004
    ## 20: 4.6990156199999999 4.7180356999999997 5.1926212300000003 5.0300502800000002
    ## 21: 4.7900524100000004 4.9408035300000002 5.0798091899999998 5.1802287099999997
    ## 22: 3.5682563799999998 3.5229399199999998 3.4531335799999998 3.6844804299999998
    ## 23: 4.7219791400000002 4.8760519000000002 5.0591573700000003 5.2278909699999998
    ## 24: 9.9734649700000002 10.051768300000001 9.9785518599999996 9.9575843800000001
    ## 25: 8.6352901499999994 8.7641210600000008 8.8199577300000005 8.7702360200000005
    ## 26:               <NA>               <NA>               <NA>               <NA>
    ## 27:               <NA>               <NA>               <NA>               <NA>
    ## 28: 3.4605324300000002 3.9305291200000001 4.6909103400000003 3.8292422300000002
    ##                   HE11               HE12               HE13               HE14
    ##                   HE15               HE16               HE17 scl05 scl06 scl07
    ##  1: 6.9784917799999997         6.60778189         6.38032866   6.9   7.0   7.2
    ##  2: 6.2324533500000001 6.3434934600000004 6.5443186799999999   9.8  10.1  10.1
    ##  3: 10.229337689999999 9.0018968600000004 10.457043649999999   9.1   9.0   8.9
    ##  4: 8.8709096899999995 9.2074222599999995 9.4693374600000002   6.3   6.4   6.5
    ##  5: 8.5884571100000002 8.2959604299999992 8.2574291199999994   7.3   7.3   7.3
    ##  6:         3.82316828         3.66298747 3.5033159299999999   1.9   2.0   2.1
    ##  7: 4.6217145899999998 3.4615774199999998 3.3297784300000002   6.4   6.5   6.5
    ##  8: 8.6285295499999997 8.4368677099999996 8.0846834199999993   4.3   4.4   4.5
    ##  9:         2.99113512 3.0856990799999999 2.8682405900000001   7.4   7.9   7.1
    ## 10: 7.7605791100000001 8.8595066100000004 8.6596641499999993    NA    NA    NA
    ## 11: 6.6527166400000004 6.9143271400000001         7.10694933    NA    NA    NA
    ## 12:               <NA>               <NA>               <NA>  10.2  10.2  10.3
    ## 13: 3.8160853399999999         3.68835807         3.71246052   7.6   8.2   8.8
    ## 14: 4.1114335100000003 3.7753798999999999 4.1038827900000001   1.7   1.7   1.8
    ## 15: 8.5576353100000002 7.5355224600000001 7.0132851599999997    NA    NA    NA
    ## 16: 3.5819501900000001 3.6477367900000002 3.7555384599999999   5.2   5.2   5.2
    ## 17:               <NA>               <NA>               <NA>    NA    NA    NA
    ## 18:         5.03110838 5.0657653800000002 4.9937844299999998   8.7   8.1   8.1
    ## 19: 4.3213686899999999 4.4068460500000004 4.4460597000000002   8.7   8.8   8.9
    ## 20: 4.9426484100000003 4.9937720299999997 5.1508317000000003  10.1  10.3  10.5
    ## 21: 5.2956042300000004 5.2652196900000003         5.34388065    NA    NA    NA
    ## 22: 3.6678931700000001 3.7633202099999998 3.8320283900000001   7.0   7.0   7.1
    ## 23: 6.0395116800000004 7.0093059499999999 7.0202856100000002  10.0   9.9  10.5
    ## 24: 9.9043521900000009 9.8667116200000002 9.8251123400000004  12.2  12.4  12.6
    ## 25: 9.0082111400000002         9.42438793 9.5163564699999998   8.0   8.0   8.2
    ## 26:               <NA>               <NA>               <NA>    NA    NA    NA
    ## 27:               <NA>               <NA>               <NA>   1.9   2.0   2.2
    ## 28: 4.4351024600000004 4.4772071799999997 4.3974895500000004   6.3   6.4   6.4
    ##                   HE15               HE16               HE17 scl05 scl06 scl07
    ##     scl08 scl09 scl10 scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05
    ##  1:   6.7   6.9   7.1   7.4   7.6   7.8   7.9   7.9   8.0   8.0  3113.0953
    ##  2:  10.1  10.1  10.1  10.2  10.2  10.2  10.2  10.2  10.2  10.2 40066.2569
    ##  3:   8.9   9.3   9.8   9.8   9.8   9.8   9.8   9.8   9.9   9.9  5109.8513
    ##  4:   6.7   6.8   6.9   7.1   7.3   7.4   7.4   7.6   7.8   7.8  4790.4371
    ##  5:   7.9   7.9   7.9   8.0   8.1   8.3   8.5   8.4   8.7   8.7  3002.1369
    ##  6:   2.2   2.3   2.3   2.4   2.4   2.5   2.5   2.6   2.7   2.7   162.4327
    ##  7:   6.6   6.7   6.7   6.8   6.8   6.9   6.9   6.9   7.1   7.1   492.5442
    ##  8:   4.6   4.7   4.7   4.8   4.9   5.0   5.1   5.2   5.2   5.3   766.6921
    ##  9:   7.1   7.4   7.4   7.6   7.6   7.8   7.8   7.9   8.0   8.0  1263.2873
    ## 10:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 11:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 12:  10.3  10.4  10.6  10.6  10.7  10.7  10.8  10.8  10.9  10.9         NA
    ## 13:   9.4   9.6   9.8  10.1  10.1  10.1  10.1  10.2  10.2  10.2  5587.0256
    ## 14:   1.9   1.9   2.0   2.0   2.1   2.2   2.3   2.3   2.3   2.3   489.0211
    ## 15:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 16:   5.2   5.2   5.2   5.5   5.7   5.9   5.9   6.0   6.2   6.2  1268.3834
    ## 17:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 18:   8.4   8.4   8.4   9.1   8.6   8.8   9.4   9.1   9.2   9.2  2729.4987
    ## 19:   9.0   9.0   8.9   9.0   9.1   9.1   9.2   9.3   9.3   9.3  1244.3490
    ## 20:  10.6  10.6  10.7  10.8  10.9  10.9  10.9  10.9  11.0  11.0  4617.9290
    ## 21:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA  5323.4631
    ## 22:   7.3   7.5   7.7   7.5   7.7   7.5   7.6   7.6   7.6   7.6  2894.0627
    ## 23:  10.7  10.7  10.7  10.8  10.8  10.8  10.8  10.8  10.9  10.9 12327.2332
    ## 24:  12.8  13.1  13.2  13.0  12.9  12.6  12.7  12.8  12.9  12.9 42030.2866
    ## 25:   8.4   8.4   8.4   8.4   8.5   8.5   8.6   8.7   8.7   8.7  5226.9378
    ## 26:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 27:   2.3   2.5   2.6   2.8   3.0   3.0   3.0   3.0   3.0   3.0         NA
    ## 28:   6.5   6.5   6.6   6.7   6.7   6.8   6.9   6.9   7.0   7.0   702.7409
    ##     scl08 scl09 scl10 scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05
    ##          GDP06      GDP07      GDP08      GDP09      GDP10      GDP11
    ##  1:  3478.7109  3946.6645  4923.8432  3883.1324  4479.3417  5462.2609
    ##  2: 42675.8128 47803.6936 48718.4969 43503.1855 40852.6668 43335.3289
    ##  3:  5919.0120  7245.4483  9020.8731  8225.1372 10385.9644 12848.8642
    ##  4:  5886.4636  7348.0308  8831.0231  8597.9154 11286.2430 13245.6125
    ##  5:  3328.8830  3567.8364  4249.0193  4231.6158  4633.5904  5200.5558
    ##  6:   194.6874   244.2860   326.4368   380.5690   341.5541   354.4796
    ##  7:   913.3939  1081.1663  1217.0648  1077.6622  1299.3449  1549.4629
    ##  8:   792.8259   981.1113  1076.7013  1150.2111  1172.0985  1287.9541
    ##  9:  1589.8015  1860.0028  2166.8542  2261.2472  3122.3628  3643.0439
    ## 10:         NA         NA         NA         NA         NA         NA
    ## 11:         NA         NA         NA         NA         NA         NA
    ## 12:         NA         NA         NA         NA         NA         NA
    ## 13:  6209.1245  7243.4560  8474.5868  7292.4944  9040.5663 10399.3728
    ## 14:   523.0386   596.6902   694.2777   698.8989   710.2742   837.6034
    ## 15:         NA         NA         NA         NA         NA         NA
    ## 16:  1656.4248  1883.4613  2242.8719  1891.3354  2280.4374  2487.5982
    ## 17:         NA         NA         NA         NA         NA         NA
    ## 18:  3154.3312  3606.0704  4220.6170  4196.3128  5082.3548  5869.3231
    ## 19:  1452.4387  1744.6403  1991.2315  1905.8947  2217.4740  2450.7337
    ## 20:  5757.4964  8360.1663 10435.0440  8548.1187  8214.0769  9099.2175
    ## 21:  6920.1891  9101.2550 11635.2729  8562.8133 10674.9958 14311.0843
    ## 22:  3369.5434  3973.0170  4379.6585  4213.0063  5076.3402  5492.1213
    ## 23: 14102.4958 16539.8781 21204.1050 14514.1417 16683.3554 19034.1492
    ## 24: 44599.6976 50566.8266 47286.9985 38713.1374 39435.8399 42038.5723
    ## 25:  5887.8487  7026.5115  9091.0790  9451.9324 11992.0166 14236.6812
    ## 26:         NA         NA         NA         NA         NA         NA
    ## 27:         NA         NA         NA         NA         NA         NA
    ## 28:  1047.9192  1124.2906  1394.0006  1159.9078  1489.4593  1672.9083
    ##          GDP06      GDP07      GDP08      GDP09      GDP10      GDP11
    ##          GDP12      GDP13      GDP14      GDP15      GDP16      GDP17 MMR05
    ##  1:  5591.2124  5498.7841  5494.3523  4187.5097  3945.4821  4111.2941   127
    ##  2: 38686.4613 39538.7667 41303.9294 35762.5231 37474.6654 38962.8804    NA
    ##  3: 13082.6643 13080.2547 12334.7982 13789.0604 12790.2425 14613.0418    59
    ##  4: 12370.0242 12300.3249 12112.5882  8814.0010  8710.0967  9925.3862    71
    ##  5:  5682.0450  6056.3308  6377.0915  6124.4916  6060.0933  6213.5013    94
    ##  6:   467.0779   499.5316   566.9265   640.5419   717.1246   768.5223   865
    ##  7:  1587.5612  2345.3929  1971.0333  1743.8510  1931.3895  2025.9324   371
    ##  8:  1337.3354  1393.9560  1402.1002  1389.1195  1265.9876  1294.2397   459
    ##  9:  3694.3489  3623.9116  3491.6248  3331.6951  3562.8458  3837.6517   252
    ## 10:         NA         NA         NA         NA         NA         NA    34
    ## 11:         NA         NA         NA         NA         NA         NA    15
    ## 12:         NA         NA         NA         NA         NA         NA    NA
    ## 13: 10817.4429 10970.1233 11319.0798  9955.2437  9817.7385 10259.1818    31
    ## 14:   778.6193   805.0328   848.2741   751.4748   780.7186   830.0184   691
    ## 15:         NA         NA         NA         NA         NA         NA    34
    ## 16:  2723.8228  2961.5503  3098.9863  2687.4801  2176.0022  1968.5647  1080
    ## 17:         NA         NA         NA         NA         NA         NA    NA
    ## 18:  6528.9722  6756.7528  6672.8803  6229.1017  6204.9973  6710.5080   118
    ## 19:  2694.3055  2871.4309  2959.6485  3001.0404  3073.6536  3123.2342   156
    ## 20:  8507.1048  9547.8522 10043.6774  8969.1489  9548.5874 10807.7954    35
    ## 21: 15420.8745 15974.6446 14095.6487  9313.0136  8704.8984 10720.3326    42
    ## 22:  5860.5825  6168.2630  5951.8837  5840.0465  5994.2315  6592.9149    43
    ## 23: 19157.4170 20143.6644 20270.8594 18289.7043 16176.9474 16238.1932    76
    ## 24: 42462.7716 43444.5330 47425.6077 44974.8319 41064.1334 40361.4174    11
    ## 25: 15171.5847 16973.6742 16831.9729 15613.7643 15387.1440 17322.1474    22
    ## 26:         NA         NA         NA         NA         NA         NA    NA
    ## 27:         NA         NA         NA         NA         NA         NA    NA
    ## 28:  1763.0727  1878.9097  1763.0626  1337.7956  1280.5784  1534.8668   421
    ##          GDP12      GDP13      GDP14      GDP15      GDP16      GDP17 MMR05
    ##     MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ##  1:   122   119   117   117   115   116   116   115   114   114   113   112
    ##  2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  3:    57    56    53    56    51    48    47    44    42    41    40    39
    ##  4:    72    71    70    69    65    61    60    61    62    63    62    60
    ##  5:    90    85    82    80    78    76    71    67    65    63    61    59
    ##  6:   795   731   681   638   597   558   527   498   472   446   422   401
    ##  7:   359   349   342   339   339   339   336   331   325   320   314   308
    ##  8:   467   473   484   484   506   496   500   496   492   488   489   480
    ##  9:   249   243   239   234   228   221   214   207   199   192   184   177
    ## 10:    32    30    28    25    22    19    18    17    17    17    16    16
    ## 11:    14    15    15    16    15    14    13    13    12    12    11    11
    ## 12:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 13:    30    30    29    29    30    30    30    30    30    30    29    29
    ## 14:   675   663   661   661   660   663   663   663   642   620   590   562
    ## 15:    31    31    29    28    29    21    22    21    23    22    20    19
    ## 16:  1040  1010   996   987   978   972   963   951   943   931   925   917
    ## 17:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 18:   114   112   108   106   104   102   100    98    96    94    91    88
    ## 19:   156   149   148   149   144   141   139   136   131   127   124   121
    ## 20:    32    30    28    28    27    24    23    21    21    21    21    19
    ## 21:    36    32    30    27    25    23    22    20    19    18    18    17
    ## 22:    42    42    43    43    42    41    39    39    38    38    37    37
    ## 23:    73    65    72    74    71    72    71    70    69    68    68    67
    ## 24:    11    11    11    10    10     9     8     8     8     8     7     7
    ## 25:    20    20    19    19    17    17    17    18    17    18    18    17
    ## 26:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 27:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 28:   406   387   356   329   305   283   267   254   242   232   222   213
    ##     MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ##                   LMH09               LMH14
    ##  1: upper middle income upper middle income
    ##  2:         high income         high income
    ##  3: upper middle income         high income
    ##  4: upper middle income upper middle income
    ##  5: lower middle income upper middle income
    ##  6:          low income          low income
    ##  7:          low income lower middle income
    ##  8:          low income          low income
    ##  9: lower middle income lower middle income
    ## 10: upper middle income upper middle income
    ## 11:         high income         high income
    ## 12:                <NA>                <NA>
    ## 13: upper middle income upper middle income
    ## 14:          low income          low income
    ## 15: lower middle income lower middle income
    ## 16: lower middle income lower middle income
    ## 17:                <NA>                <NA>
    ## 18: upper middle income upper middle income
    ## 19: lower middle income lower middle income
    ## 20: upper middle income upper middle income
    ## 21: upper middle income         high income
    ## 22: lower middle income upper middle income
    ## 23:         high income         high income
    ## 24:         high income         high income
    ## 25: upper middle income         high income
    ## 26:                <NA>                <NA>
    ## 27:                <NA>                <NA>
    ## 28:          low income lower middle income
    ##                   LMH09               LMH14

``` r
edit
```

    ##                 location_name GSNI_PERIOD onebias twobias nobias political
    ##  1:                   Algeria   2010–2014   97.83   87.00   2.17     80.08
    ##  2:                   Andorra   2005–2009   27.01    7.43  72.99     14.08
    ##  3:                 Argentina   2010–2014   75.41   42.49  24.59     43.35
    ##  4:                   Armenia   2010–2014   94.11   81.28   5.89     72.82
    ##  5:                 Australia   2010–2014   46.24   23.00  53.76     32.48
    ##  6:                Azerbaijan   2010–2014   99.14   93.82   0.86     85.13
    ##  7:                   Belarus   2010–2014   90.37   71.70   9.63     77.82
    ##  8:                    Brazil   2010–2014   89.50   52.39  10.50     43.41
    ##  9:                  Bulgaria   2005–2009   76.84   44.40  23.16     53.67
    ## 10:              Burkina Faso   2005–2009   98.38   85.86   1.62     66.42
    ## 11:                    Canada   2005–2009   51.53   23.26  48.47     25.40
    ## 12:                     Chile   2010–2014   74.40   42.20  25.60     42.10
    ## 13:                     China   2010–2014   88.27   64.42  11.73     55.47
    ## 14:                  Colombia   2010–2014   91.40   57.21   8.60     49.34
    ## 15:                    Cyprus   2010–2014   81.05   49.44  18.95     48.14
    ## 16:                   Ecuador   2010–2014   93.34   58.90   6.66     46.34
    ## 17:                   Estonia   2010–2014   76.34   51.19  23.66     57.05
    ## 18:                  Ethiopia   2005–2009   85.27   35.14  14.73     30.27
    ## 19:                   Finland   2005–2009   51.16   22.67  48.84     24.58
    ## 20:                    France   2005–2009   56.00   26.81  44.00     35.25
    ## 21:                   Georgia   2010–2014   94.09   77.12   5.91     65.89
    ## 22:                   Germany   2010–2014   62.60   33.07  37.40     26.59
    ## 23:                     Ghana   2010–2014   99.16   92.69   0.84     86.84
    ## 24:                     Haiti   2010–2014   98.91   92.82   1.09     76.33
    ## 25:                   Hungary   2005–2009   65.89   40.36  34.11     42.84
    ## 26:                     India   2010–2014   98.28   83.25   1.72     64.10
    ## 27:                 Indonesia   2005–2009   97.44   80.36   2.56     66.47
    ## 28: Iran, Islamic Republic of   2005–2009   98.54   92.49   1.46     84.63
    ## 29:                      Iraq   2010–2014   97.50   90.58   2.50     88.33
    ## 30:                     Japan   2010–2014   68.81   41.67  31.19     46.87
    ## 31:                    Jordan   2010–2014   99.33   95.67   0.67     91.17
    ## 32:                Kazakhstan   2010–2014   96.22   79.02   3.78     75.22
    ## 33:       Korea (Republic of)   2010–2014   87.07   62.91  12.93     63.68
    ## 34:                    Kuwait   2010–2014   97.77   91.56   2.23     88.10
    ## 35:                Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80
    ## 36:                   Lebanon   2010–2014   96.08   82.33   3.92     75.42
    ## 37:                     Libya   2010–2014   99.13   92.89   0.87     83.14
    ## 38:                  Malaysia   2010–2014   98.54   88.38   1.46     79.69
    ## 39:                      Mali   2005–2009   98.82   93.36   1.18     81.89
    ## 40:                    Mexico   2010–2014   87.70   51.00  12.30     41.40
    ## 41:      Moldova, Republic of   2005–2009   90.06   67.21   9.94     60.33
    ## 42:                   Morocco   2010–2014   96.25   80.58   3.75     69.00
    ## 43:               Netherlands   2010–2014   39.75   15.88  60.25     21.29
    ## 44:               New Zealand   2010–2014   46.14   21.28  53.86     27.23
    ## 45:                   Nigeria   2010–2014   99.73   94.99   0.27     85.83
    ## 46:                    Norway   2005–2009   41.27   16.00  58.73     19.51
    ## 47:                  Pakistan   2010–2014   99.81   98.07   0.19     81.32
    ## 48:       Palestine, State of   2010–2014   98.00   92.30   2.00     89.30
    ## 49:                      Peru   2010–2014   87.96   49.99  12.04     38.44
    ## 50:               Philippines   2010–2014   98.87   86.80   1.13     70.62
    ## 51:                    Poland   2010–2014   79.75   47.31  20.25     43.74
    ## 52:                     Qatar   2010–2014   99.73   94.90   0.27     91.56
    ## 53:                   Romania   2010–2014   85.50   60.84  14.50     48.78
    ## 54:        Russian Federation   2010–2014   86.83   68.56  13.17     68.43
    ## 55:                    Rwanda   2010–2014   99.15   89.39   0.85     67.78
    ## 56:                    Serbia   2005–2009   82.62   48.61  17.38     47.05
    ## 57:                 Singapore   2010–2014   92.34   73.20   7.66     76.18
    ## 58:                  Slovenia   2010–2014   59.21   28.25  40.79     33.58
    ## 59:              South Africa   2010–2014   96.32   80.90   3.68     75.56
    ## 60:                     Spain   2010–2014   50.50   25.16  49.50     29.40
    ## 61:                    Sweden   2010–2014   30.01   10.75  69.99     16.05
    ## 62:               Switzerland   2005–2009   56.03   26.94  43.97     20.56
    ## 63:                  Thailand   2010–2014   95.47   74.50   4.53     67.30
    ## 64:       Trinidad and Tobago   2010–2014   85.99   51.25  14.01     39.14
    ## 65:                   Tunisia   2010–2014   96.35   84.07   3.65     78.42
    ## 66:                    Turkey   2010–2014   96.52   85.70   3.48     76.02
    ## 67:                   Ukraine   2010–2014   86.53   65.40  13.47     62.63
    ## 68:            United Kingdom   2005–2009   54.60   25.50  45.40     26.07
    ## 69:             United States   2010–2014   57.31   30.07  42.69     39.90
    ## 70:                   Uruguay   2010–2014   74.60   36.70  25.40     28.60
    ## 71:                Uzbekistan   2010–2014   97.93   87.73   2.07     78.67
    ## 72:                  Viet Nam   2005–2009   92.89   69.17   7.11     59.40
    ## 73:                     Yemen   2010–2014   97.80   92.10   2.20     87.40
    ## 74:                    Zambia   2005–2009   96.84   80.56   3.16     66.04
    ## 75:                  Zimbabwe   2010–2014   99.52   84.78   0.48     78.16
    ##                 location_name GSNI_PERIOD onebias twobias nobias political
    ##     economic educational physical   cvd05fem   cvd06fem  cvd07fem  cvd08fem
    ##  1:    74.08       37.17    86.75  500.26865  494.78751 490.17488 486.31725
    ##  2:     8.73        1.81    12.01   94.99074   94.10549  92.00298  91.82684
    ##  3:    30.43       17.04    52.86  169.99370  165.87751 165.37005 156.11837
    ##  4:    75.91       24.89    66.14  399.51778  387.58279 372.19335 367.54267
    ##  5:    18.06        4.09    20.93  121.85287  117.56935 115.44308 112.43414
    ##  6:    91.97       30.90    72.16  653.88539  662.74433 659.53238 654.59319
    ##  7:    58.45       21.19    55.52  476.75969  456.91769 436.00228 424.59960
    ##  8:    36.63        9.32    77.95  192.71662  188.83657 184.09958 179.74663
    ##  9:    37.13       10.95    39.42  563.51958  553.94503 534.14462 513.25045
    ## 10:    77.27       33.20    88.81  263.21606  267.03358 264.70693 266.75755
    ## 11:    21.39        4.91    30.50  116.99764  110.81993 106.27193 101.96989
    ## 12:    28.40       20.10    52.60  140.36018  135.71522 133.00137 125.93748
    ## 13:    54.87       22.02    67.01  293.52071  281.01190 271.51485 268.11582
    ## 14:    33.73       10.78    82.28  161.61647  158.77002 152.36513 147.40325
    ## 15:    43.85       14.03    53.31  250.96373  242.99750 231.03900 217.51610
    ## 16:    36.44       23.46    84.36  161.28268  164.29742 163.35005 160.95424
    ## 17:    45.29       15.79    36.24  330.82583  315.93493 304.29041 287.20949
    ## 18:    22.00        8.00    80.60  261.82080  255.18410 248.43715 242.03145
    ## 19:    23.08        6.22    29.69  176.62785  169.44401 166.12157 162.55154
    ## 20:    25.55        6.71    22.41   99.53019   95.81306  92.19573  90.31795
    ## 21:    66.97       18.14    74.63  490.15738  460.58540 405.68057 386.15315
    ## 22:    30.91       15.78    44.68  164.45651  160.30599 157.68068 154.89910
    ## 23:    78.01       30.02    90.73  325.79930  328.95890 329.44442 331.09718
    ## 24:    72.06       59.91    88.13  526.03693  523.13684 519.11249 514.70529
    ## 25:    37.86       18.75    30.99  321.85927  305.13190 304.31202 290.71653
    ## 26:    69.91       35.24    88.38  244.04782  250.59255 251.95527 249.71809
    ## 27:    66.40       19.31    90.55  369.65859  371.69064 371.74037 373.60996
    ## 28:    88.86       55.42    78.69  346.45487  333.03926 318.53806 305.98136
    ## 29:    79.75       31.33    85.08  425.08001  419.48272 425.47451 429.32978
    ## 30:    41.79       16.21    26.28   80.62892   77.54224  74.58392  72.08078
    ## 31:    89.42       28.75    81.50  456.66983  436.64573 355.58034 306.73448
    ## 32:    67.54       21.71    68.51  602.99412  598.51899 592.41855 573.33154
    ## 33:    54.33       25.67    58.27  155.91408  143.80539 132.67130 120.85549
    ## 34:    77.13       36.45    83.12  181.89122  175.94790 179.89550 194.47948
    ## 35:    71.53       41.00    81.73  546.88719  553.71417 543.25326 536.85221
    ## 36:    60.17       31.08    82.83  303.23657  299.39932 295.93058 292.27397
    ## 37:    84.45       31.49    92.15  237.22727  236.08059 262.78556 263.14267
    ## 38:    74.54       43.00    94.31  278.24360  271.41947 262.56217 262.28229
    ## 39:    88.87       47.61    84.87  320.56474  319.77363 317.97481 317.98693
    ## 40:    29.35       20.70    75.55  151.15964  144.19896 140.29190 141.04622
    ## 41:    58.80       16.73    65.20  513.32695  491.99307 497.11512 484.52829
    ## 42:    72.50       19.58    82.50  467.01239  470.58727 476.71380 475.72512
    ## 43:    13.56        4.63    22.03  132.29110  124.55363 117.11491 112.35547
    ## 44:    16.65        5.35    25.33  155.37028  148.49070 141.69390 137.45964
    ## 45:    83.42       46.18    92.78  277.35636  271.85405 263.52070 259.79173
    ## 46:    21.85        3.71    16.78  133.68482  129.54509 126.20872 120.38503
    ## 47:    91.02       51.11    93.75  377.94485  375.01056 370.38114 367.57322
    ## 48:    79.50       26.70    83.50  367.37503  362.07740 355.82036 350.58586
    ## 49:    27.05       14.36    79.76   97.82452   91.17324  84.14951  83.74437
    ## 50:    73.80       39.08    91.48  293.05106  293.56564 287.95050 287.91056
    ## 51:    41.99       11.91    53.02  266.44041  259.67242 253.75756 245.74676
    ## 52:    81.66       27.60    87.25  515.93710  565.15699 590.64553 588.12862
    ## 53:    55.88       20.69    63.54  458.08365  437.32047 413.66866 400.71262
    ## 54:    58.77       22.66    50.02  569.51494  527.82419 500.58817 492.70377
    ## 55:    65.68       36.15    97.64  278.42226  268.41828 261.94552 258.00089
    ## 56:    35.49       13.20    66.56  561.66401  547.13295 531.03292 528.63016
    ## 57:    52.23       26.18    65.66  123.75950  119.41610 114.07452 110.44957
    ## 58:    25.91        8.04    29.93  177.44491  165.38997 164.02554 155.04768
    ## 59:    57.06       38.80    88.80  270.47569  273.36293 268.57381 264.84106
    ## 60:    20.48       11.61    28.05  125.87664  118.95395 114.41647 110.13104
    ## 61:     9.16        2.61    14.13  148.45246  147.10784 144.55349 139.74976
    ## 62:    29.80        9.28    31.18  122.88300  118.65244 115.21043 112.20618
    ## 63:    50.86       29.02    84.53  148.01004  142.41904 137.40121 132.15039
    ## 64:    37.74        5.61    72.17  244.42399  237.87706 229.26124 224.70676
    ## 65:    79.34       24.48    83.82  322.17241  317.71257 315.51246 314.01649
    ## 66:    80.25       32.04    77.56  183.55911  194.36521 206.41355 222.78746
    ## 67:    57.69       18.23    56.61  577.57784  553.20902 554.13746 550.85070
    ## 68:    25.15        6.65    30.34  147.03415  138.97711 132.69706 127.54333
    ## 69:    14.81        6.54    34.57  158.05263  151.99954 145.99098 142.15274
    ## 70:    34.30        9.20    51.40  181.89537  171.40400 172.51409 160.79502
    ## 71:    80.33       48.60    83.93 1025.82262 1005.30790 983.98523 997.02332
    ## 72:    62.49       20.36    70.56  256.62741  256.62311 255.97453 255.19294
    ## 73:    87.20       45.30    81.00  493.05999  489.24403 485.10075 483.33838
    ## 74:    55.41       23.53    89.07  301.19131  304.24329 301.25347 299.44540
    ## 75:    57.30       16.20    96.27  289.77211  299.70819 309.57355 329.62200
    ##     economic educational physical   cvd05fem   cvd06fem  cvd07fem  cvd08fem
    ##      cvd09fem   cvd10fem   cvd11fem   cvd12fem   cvd13fem   cvd14fem   cvd15fem
    ##  1: 483.19933  484.49190  484.48211  479.07575  472.94269  471.02644  468.44578
    ##  2:  93.03548   93.64847   94.72272   94.77537   95.38794   95.79920   96.76678
    ##  3: 153.96832  154.88168  152.74845  149.31115  147.59175  146.19661  147.86150
    ##  4: 356.24738  345.06490  332.54986  324.41710  312.28518  305.51958  306.31351
    ##  5: 107.29165  102.53083   99.64511   95.33617   91.97374   91.08401   90.33633
    ##  6: 655.75062  661.62994  662.91437  665.11545  673.97385  688.85984  689.28400
    ##  7: 427.87579  422.21601  428.07519  392.30102  389.72174  383.05228  373.94641
    ##  8: 176.62840  172.89359  169.38552  163.58428  159.43643  155.71901  153.99486
    ##  9: 497.57334  495.06305  483.56995  465.16759  445.93731  456.08244  452.74524
    ## 10: 268.43926  264.90564  263.36825  262.29191  261.66123  258.70423  264.08260
    ## 11:  97.40916   94.08300   90.66281   88.18456   86.23911   85.74291   84.61436
    ## 12: 125.43462  125.62331  122.17205  119.59622  116.95291  113.22186  111.62328
    ## 13: 267.63625  268.52922  265.54747  253.08796  247.15234  241.30135  233.12895
    ## 14: 141.44052  137.02505  128.36209  124.15288  120.87237  121.67218  120.71119
    ## 15: 195.44933  188.74835  188.54621  184.83879  181.23758  181.02862  179.81251
    ## 16: 157.69322  157.35397  149.29192  153.73253  152.93464  152.05617  148.90913
    ## 17: 270.68512  250.54530  249.09076  240.37134  234.94049  223.77404  215.99088
    ## 18: 236.24831  231.66182  230.61082  229.20585  225.91810  224.28592  223.92249
    ## 19: 159.13643  155.55659  151.01600  149.29429  146.09508  142.89271  139.40322
    ## 20:  87.71812   84.70602   82.21519   80.70658   77.56393   74.42390   73.92490
    ## 21: 400.05120  396.62398  388.33180  373.19132  358.88707  351.76835  346.48069
    ## 22: 148.99784  143.04586  139.70816  138.68965  139.45365  135.66241  137.60739
    ## 23: 332.15021  332.89254  332.14372  330.11120  328.64703  326.15716  329.48438
    ## 24: 510.66940  508.21671  512.24372  507.65492  502.18482  497.94515  495.34101
    ## 25: 290.03948  285.44636  281.26391  275.54613  267.00697  261.70868  266.30519
    ## 26: 230.92592  219.76903  219.51956  221.49818  228.70394  229.45567  228.12642
    ## 27: 373.65398  372.87334  371.44090  372.29798  370.74477  368.80629  368.52390
    ## 28: 298.01274  290.36702  283.44570  278.28489  277.07263  275.18836  277.67423
    ## 29: 424.94870  418.48356  410.42655  401.38233  391.98322  384.19680  378.94274
    ## 30:  69.30532   68.18071   67.97138   65.88326   63.94075   62.02763   59.92309
    ## 31: 283.45375  271.98117  269.82701  266.61303  261.65602  259.30701  250.50895
    ## 32: 547.90401  538.84766  523.68062  506.50361  483.03342  462.02411  446.05277
    ## 33: 111.78620  105.34493  100.12794   95.58209   90.21631   85.58348   83.53081
    ## 34: 187.99981  170.86166  155.06885  143.13273  131.57058  127.63836  121.04601
    ## 35: 507.41753  488.18726  483.29378  465.05092  442.79747  429.36559  435.06853
    ## 36: 287.77177  283.67734  280.66965  276.61899  272.35569  268.86559  265.65261
    ## 37: 278.29158  264.40989  260.81532  258.33264  257.40970  259.48420  263.10594
    ## 38: 263.15505  252.21083  237.09570  233.23215  226.51494  228.10326  227.29230
    ## 39: 317.14954  316.24847  314.96309  313.19762  310.27834  307.23772  309.21589
    ## 40: 143.77080  142.07525  139.51818  139.67199  144.16564  145.50041  143.81407
    ## 41: 467.91012  459.10667  400.91032  387.36233  372.28763  377.82255  382.27748
    ## 42: 474.36588  471.62040  467.78485  463.56393  458.27303  452.96197  447.74923
    ## 43: 107.46307  104.32878  101.92943  100.04194   97.79521   95.90771   95.90896
    ## 44: 131.57251  128.16027  127.02856  122.67986  117.17536  117.01438  116.64341
    ## 45: 257.07280  254.32157  253.99749  254.09391  253.49508  249.71126  252.45876
    ## 46: 116.04100  111.96544  110.15403  107.20894  103.74549   98.13955   94.96062
    ## 47: 363.86544  361.14617  359.10034  356.65214  355.72275  352.48397  349.56586
    ## 48: 343.97033  341.78855  337.69661  320.17827  314.63758  323.39695  338.52757
    ## 49:  93.03498   94.03452   91.93528   89.98908   87.97633   83.77122   80.26885
    ## 50: 287.31183  284.21639  281.45974  280.20698  277.95194  274.33572  275.99697
    ## 51: 239.14348  223.02773  214.80331  211.78583  205.90402  198.63859  196.65466
    ## 52: 596.98343  580.94989  537.36225  503.19376  493.70697  487.72256  496.97364
    ## 53: 397.43462  386.99741  365.99837  359.62475  340.34775  338.83849  330.12565
    ## 54: 467.54550  459.66489  424.43583  406.44126  391.62857  386.28781  376.79609
    ## 55: 252.86863  250.72670  248.99257  248.50654  247.54727  246.84383  246.00868
    ## 56: 518.35528  493.55030  467.81093  455.97734  447.78602  454.53739  452.53237
    ## 57: 103.82613   99.15298   92.93724   91.20203   86.55430   83.43653   79.65879
    ## 58: 155.25430  150.58585  148.30057  144.80747  137.50103  128.59829  127.99270
    ## 59: 262.78823  260.07828  252.89593  244.75520  240.34291  242.68919  243.05204
    ## 60: 104.36714   99.13120   96.70095   94.55759   91.06756   89.52425   90.47800
    ## 61: 136.25239  132.65068  130.65338  127.62791  124.02870  120.35028  117.04866
    ## 62: 108.95456  104.21885   99.18577   98.65605   96.22728   93.21771   91.66481
    ## 63: 125.40487  122.07400  115.60825  109.16780  103.96458  100.77038   97.84838
    ## 64: 212.68349  204.48233  192.87097  183.77914  188.94522  190.52675  189.97215
    ## 65: 310.36168  307.20887  303.12223  299.46454  296.53305  292.87295  289.24495
    ## 66: 230.29024  229.62709  225.21398  222.71768  224.81972  219.17603  210.88283
    ## 67: 519.56979  506.60540  481.38459  473.11451  464.17229  436.92347  474.36156
    ## 68: 120.00812  115.83452  112.45472  112.30358  111.23225  108.82079  108.30317
    ## 69: 137.70771  133.79783  132.74355  130.79111  129.20879  128.48531  128.22456
    ## 70: 150.85425  147.47007  150.67799  146.49314  140.61544  136.51041  136.27482
    ## 71: 992.45481 1012.61930 1037.46364 1049.70253 1041.14657 1024.86478 1010.07674
    ## 72: 254.46217  252.55867  250.17926  247.05521  243.05616  239.21285  234.94030
    ## 73: 478.34072  472.17590  471.35922  468.99902  467.16132  464.47411  466.86730
    ## 74: 297.43078  300.60435  303.67459  305.16350  306.41551  307.02384  308.97725
    ## 75: 343.31830  344.91034  335.87436  330.32199  326.08086  326.36266  323.82062
    ##      cvd09fem   cvd10fem   cvd11fem   cvd12fem   cvd13fem   cvd14fem   cvd15fem
    ##      cvd16fem  cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male
    ##  1: 458.09789 450.74425 451.34683 447.67418  462.4656  450.5957  439.3599
    ##  2:  96.28366  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052
    ##  3: 153.18030 154.09804 150.26997 149.24294  265.4632  258.6674  258.7628
    ##  4: 298.74575 297.69330 296.63522 294.02914  537.4210  521.6946  500.3150
    ##  5:  87.58080  87.33218  88.85876  89.00590  181.3310  172.6294  168.0884
    ##  6: 688.89897 685.80022 648.14175 627.27975  795.4780  806.0879  797.5581
    ##  7: 375.58732 373.29793 365.27597 362.81115  841.2488  822.3199  768.8990
    ##  8: 153.37633 149.03982 148.29788 146.95291  269.7419  264.7693  259.8278
    ##  9: 444.74854 440.64650 452.67136 454.55472  814.3341  795.8899  767.4014
    ## 10: 263.37459 264.73246 264.32411 260.94948  321.0466  323.3540  321.8477
    ## 11:  84.05718  83.45293  85.66701  86.14853  181.5499  171.6346  165.4747
    ## 12: 107.64387 108.90039 108.43570 107.52371  195.4275  188.5012  184.1129
    ## 13: 229.92826 226.55616 222.93455 219.72779  423.6444  406.6910  401.4993
    ## 14: 118.52971 116.30213 117.34137 116.31989  198.3875  197.3086  186.1763
    ## 15: 178.14639 179.63631 174.81338 172.41410  340.5299  338.3851  330.8774
    ## 16: 149.01126 149.63616 148.94035 146.37173  195.4146  197.5540  198.2290
    ## 17: 223.72511 224.06573 222.73761 220.75317  580.3855  572.3399  567.1382
    ## 18: 222.61808 221.95432 222.81058 223.55265  276.7356  271.0027  264.5905
    ## 19: 136.26260 136.03946 135.91504 135.37010  292.7180  285.9363  280.4463
    ## 20:  71.61158  71.27759  71.86263  72.24576  165.8732  159.3346  153.5253
    ## 21: 354.29394 354.93187 352.69787 350.40802  789.5317  758.4226  714.3686
    ## 22: 136.79190 134.25372 133.40700 132.89737  233.5936  225.2332  219.1929
    ## 23: 327.68822 326.79742 324.50422 324.16728  308.0771  311.7312  310.7842
    ## 24: 489.48808 483.52984 479.46926 475.27316  431.5242  430.9686  431.2913
    ## 25: 248.56766 252.14267 253.81270 248.63860  499.4575  479.7642  472.1848
    ## 26: 224.03390 221.80827 224.21409 222.71461  323.1376  329.6939  333.5011
    ## 27: 365.10278 361.90945 358.11468 354.06925  386.6718  391.2950  397.2689
    ## 28: 278.51259 277.88105 271.04348 268.72894  400.2154  389.5281  376.5086
    ## 29: 371.75997 365.82647 370.18639 374.66161  531.4743  524.8855  531.3580
    ## 30:  59.05503  57.80935  58.18790  58.35637  136.7445  131.7984  128.0151
    ## 31: 251.45785 251.13060 249.70105 249.59510  294.3445  290.7921  284.2273
    ## 32: 433.43107 408.05109 402.99006 401.45939  929.4738  926.9272  918.2064
    ## 33:  81.91117  81.88305  82.38176  83.11824  190.0032  178.2896  167.8587
    ## 34: 120.12173 120.95883 121.22492 124.78355  282.0308  283.0347  271.4806
    ## 35: 415.09331 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687
    ## 36: 261.75633 257.89829 254.19234 250.92597  417.7924  423.3776  429.4667
    ## 37: 267.11855 269.00461 269.08361 267.94568  277.1266  274.9450  289.2356
    ## 38: 224.59851 222.86437 221.09851 220.62850  326.1480  318.3576  312.0718
    ## 39: 307.72590 305.13664 304.19901 301.76476  250.4399  251.6409  253.7490
    ## 40: 142.77859 142.69229 140.41210 140.20271  169.6199  163.9455  161.0535
    ## 41: 381.40961 368.12912 361.05127 341.11337  702.8658  671.8424  666.6718
    ## 42: 441.85947 436.58902 430.76329 424.09089  432.7351  431.6196  429.6188
    ## 43:  96.25005  95.80597  96.01044  95.83739  204.2457  190.8541  177.8760
    ## 44: 114.37102 113.32888 115.13131 114.70982  220.9654  212.2025  206.3657
    ## 45: 247.75393 245.20993 242.25928 239.99721  274.4590  267.8654  259.5376
    ## 46:  93.59243  91.43524  92.90655  91.26853  216.2011  207.2765  201.3580
    ## 47: 346.24658 343.28574 340.52887 337.33604  423.6880  422.0473  417.8427
    ## 48: 350.93555 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858
    ## 49:  78.92477  78.51316  77.75822  77.12523  118.1332  114.0556  109.6057
    ## 50: 273.52319 271.92029 269.31716 266.26944  375.8930  378.2835  374.6201
    ## 51: 190.17891 188.19442 189.08240 188.55414  414.9361  405.3786  397.2696
    ## 52: 496.43992 493.90243 476.04344 464.62893  366.7667  370.5184  364.8331
    ## 53: 320.96970 317.98703 317.44903 319.76750  594.6991  574.2798  550.0335
    ## 54: 366.11070 348.59567 349.51106 351.22730  956.8934  876.9782  830.0730
    ## 55: 244.49723 244.00247 246.14405 247.50654  316.1255  304.5015  296.7298
    ## 56: 439.25670 434.70288 432.93221 428.30908  605.1301  591.9054  575.6820
    ## 57:  76.29249  74.34356  73.94099  73.62812  186.6925  179.9437  176.8463
    ## 58: 123.95531 123.50120 122.89839 122.40448  264.4295  262.8556  252.0833
    ## 59: 238.75060 230.50741 211.82800 203.07327  329.9911  331.5420  324.4782
    ## 60:  87.69975  87.21801  88.63424  88.97564  184.0483  175.2505  169.7297
    ## 61: 116.75578 115.86243 116.73812 114.02886  236.4315  227.9933  219.7105
    ## 62:  90.25664  89.34129  89.46421  89.22168  187.1172  180.2592  173.7287
    ## 63:  97.65457  96.75148  96.78591  96.50541  179.0414  171.3873  164.7609
    ## 64: 194.01692 194.33779 194.26330 193.63737  339.4050  312.2339  301.5650
    ## 65: 286.00610 282.68837 278.31545 273.75934  432.1303  427.3782  420.7113
    ## 66: 209.01318 206.79101 201.07341 197.46845  300.6333  288.9889  279.9032
    ## 67: 460.24398 455.73238 466.00037 465.00257  906.8034  875.1121  894.2645
    ## 68: 106.14944 104.51284 106.93075 107.12455  224.9909  213.8383  203.5031
    ## 69: 127.32223 124.56113 124.07404 126.43820  233.8366  225.4416  216.8335
    ## 70: 133.88372 134.15097 133.06846 132.74195  264.6658  253.9937  254.2766
    ## 71: 975.22891 945.26316 908.52364 856.84712 1349.0110 1313.7526 1257.9630
    ## 72: 230.61406 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026
    ## 73: 466.88438 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200
    ## 74: 311.38853 308.67000 306.10250 303.60330  396.9908  395.9378  389.5186
    ## 75: 320.79236 316.83381 313.15585 310.65518  347.0392  344.8029  344.3671
    ##      cvd16fem  cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male
    ##     cvd08male cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male
    ##  1:  428.9066  419.1618  408.5190  403.3053  397.5104  389.1503  384.7680
    ##  2:  126.3548  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398
    ##  3:  246.9013  240.5962  239.1611  236.2446  233.3321  230.6517  226.4522
    ##  4:  491.6596  484.3347  479.2840  469.5447  446.4262  427.0121  426.5800
    ##  5:  163.8370  155.6688  149.6376  144.4923  137.6570  132.9428  131.1893
    ##  6:  795.9201  792.0192  792.6029  806.8414  826.6460  825.9170  835.2344
    ##  7:  771.9881  770.1596  779.5070  788.8996  710.1381  699.5572  687.9125
    ##  8:  255.9295  252.0371  248.5607  244.3169  236.3190  231.1611  225.6046
    ##  9:  740.1393  720.9119  704.5502  686.3721  666.1790  649.1804  660.2319
    ## 10:  324.6877  327.7689  329.5061  330.3046  331.4828  332.6494  330.4457
    ## 11:  159.1524  152.1508  145.6834  139.2655  136.1435  133.0473  131.5159
    ## 12:  176.6143  177.1817  177.1280  171.3006  167.2849  164.6824  160.6889
    ## 13:  406.6927  414.9338  423.2396  424.6874  423.3078  418.3329  407.6918
    ## 14:  183.5070  188.8747  181.7598  170.8570  166.9460  161.8714  153.1305
    ## 15:  312.6105  308.6625  303.2067  287.3138  275.9141  262.2377  252.0967
    ## 16:  197.6351  191.5033  191.5863  191.7630  187.9508  186.7919  185.9278
    ## 17:  515.3504  490.8314  451.1965  441.4808  429.0819  392.8894  398.8431
    ## 18:  258.7780  252.8036  245.2730  242.6073  240.1706  236.7341  234.6738
    ## 19:  271.9016  266.2553  260.7571  252.2236  244.8651  238.2376  231.7559
    ## 20:  148.4007  144.0251  139.7824  135.2442  131.5903  126.6114  120.7370
    ## 21:  700.3057  696.2731  710.0383  675.1747  630.1524  598.1975  590.5490
    ## 22:  213.4383  207.8089  203.9996  202.0398  202.2580  202.5074  198.3229
    ## 23:  310.4548  309.2160  308.0875  307.7036  305.1269  301.1806  296.2265
    ## 24:  431.2199  430.6858  431.0238  433.7781  433.2272  432.0381  430.4305
    ## 25:  455.5834  453.8592  442.8598  428.5252  421.4481  404.8402  402.1091
    ## 26:  339.3312  326.9937  317.6740  314.9239  312.8411  321.2987  316.9092
    ## 27:  402.7375  406.8324  409.3400  411.8711  416.3984  418.3803  419.7302
    ## 28:  362.0445  350.5229  338.1660  323.1285  311.8184  304.8604  299.1565
    ## 29:  533.9168  529.4913  521.5873  513.0925  503.3006  492.3579  484.0175
    ## 30:  124.7603  121.7149  119.7500  117.8518  114.2759  110.5415  106.8358
    ## 31:  280.5809  282.2404  290.8544  286.8479  273.0348  253.1607  245.1521
    ## 32:  877.5018  824.7192  809.5292  791.3657  766.3255  730.3873  693.7872
    ## 33:  158.2836  149.4912  143.7630  137.6913  131.7820  123.9082  117.5575
    ## 34:  295.6581  273.0090  237.9775  221.1212  224.7644  218.0751  208.6838
    ## 35:  761.4547  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342
    ## 36:  435.0033  438.0815  438.3123  441.6159  441.8114  440.4026  438.2263
    ## 37:  287.1150  285.6811  278.8281  279.6004  270.5042  272.5029  278.0476
    ## 38:  316.5872  315.2901  303.3995  295.3241  285.1897  264.2568  267.6031
    ## 39:  256.6272  259.4479  259.6777  258.0625  256.5280  255.5163  253.8127
    ## 40:  167.7170  171.5168  169.7953  166.9745  166.6525  173.9652  174.7192
    ## 41:  648.2151  638.1296  641.1604  572.9933  565.3869  534.3178  552.1560
    ## 42:  425.8729  421.7635  417.3211  412.7217  414.7864  498.5701  495.8274
    ## 43:  168.0391  159.6544  153.4482  146.7307  143.5591  139.3567  133.9315
    ## 44:  198.4618  190.4682  182.1603  178.1287  173.7814  167.9309  166.5693
    ## 45:  255.2531  252.9699  250.7906  250.3251  250.0944  248.4156  244.0515
    ## 46:  193.9676  183.1434  174.8670  171.7229  163.5821  157.0361  149.7302
    ## 47:  415.8061  413.1472  410.6374  407.1085  403.7996  401.3595  398.1383
    ## 48:  484.7670  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364
    ## 49:  107.1766  117.1724  120.0147  118.2343  116.0098  113.0409  107.4914
    ## 50:  378.4958  380.0939  378.0533  378.7095  380.0489  377.3414  376.7860
    ## 51:  386.0225  371.2604  353.6693  341.0147  336.7932  326.9953  311.7931
    ## 52:  352.0659  334.4922  319.9307  311.1768  304.3177  297.3830  303.6893
    ## 53:  547.3480  543.1993  535.6756  501.2370  496.4683  468.9440  476.7701
    ## 54:  825.6957  776.4243  770.5636  706.6609  675.9903  650.0326  646.2977
    ## 55:  290.2283  282.3209  278.0851  274.4761  273.5717  269.1670  267.0439
    ## 56:  567.4146  548.0041  524.2160  503.8235  494.4056  513.2331  517.1577
    ## 57:  168.2657  159.8412  154.4816  147.9723  141.9160  136.9070  131.4295
    ## 58:  242.3505  230.0857  224.8782  215.9844  213.3876  206.5004  191.7111
    ## 59:  320.3647  318.1047  315.7347  305.3509  295.2961  286.9575  285.3966
    ## 60:  162.5285  154.6107  148.3865  143.7648  140.5219  134.6683  132.3118
    ## 61:  211.9112  205.6428  198.6352  193.1647  187.9331  181.3070  174.5094
    ## 62:  166.1876  161.4280  155.4808  149.9229  145.6939  142.3391  138.2464
    ## 63:  159.7038  155.0085  153.1022  147.5108  142.4819  139.5502  139.3282
    ## 64:  311.0354  288.0985  279.1031  263.7642  248.9993  253.3945  255.4942
    ## 65:  415.2339  410.8270  407.0929  398.7661  394.3958  391.4548  387.7552
    ## 66:  271.6346  274.3569  272.2723  270.8643  268.4807  269.7801  270.6847
    ## 67:  895.7625  817.5231  795.2222  763.1596  754.7471  743.9361  691.8518
    ## 68:  195.1599  185.3431  178.1320  171.7848  168.7043  167.4859  163.1700
    ## 69:  211.1282  204.2811  198.2355  195.8800  193.1188  191.5215  190.1297
    ## 70:  236.7589  226.8973  220.8039  222.2131  216.9713  210.2661  203.4770
    ## 71: 1247.2106 1227.9049 1230.5979 1242.3562 1242.5922 1222.1098 1205.2298
    ## 72:  455.8597  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765
    ## 73:  566.4953  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702
    ## 74:  383.8555  377.2154  376.4333  380.5640  379.4144  379.6064  381.3379
    ## 75:  346.6972  343.4789  341.5723  337.1690  333.0484  328.7616  327.1498
    ##     cvd08male cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male
    ##     cvd15male cvd16male  cvd17male cvd18male  cvd19male cvd05both cvd06both
    ##  1:  383.2925  379.8222  377.66355  373.4875  371.51847  479.0988  469.5897
    ##  2:  119.3653  118.6682  117.78826  116.9475  115.89102  114.4628  112.7694
    ##  3:  227.7888  234.6520  235.00380  228.2053  226.15031  212.7781  207.4933
    ##  4:  423.7013  415.9128  413.73292  412.6031  410.19306  460.3967  448.1819
    ##  5:  130.2281  126.8570  127.62547  129.0003  129.37340  148.9638  142.8732
    ##  6:  833.7264  842.2704  843.82968  784.6385  760.37239  725.8544  735.0585
    ##  7:  667.1438  655.9036  654.16797  631.3911  624.05795  615.4166  593.5400
    ##  8:  222.4439  222.8537  215.66746  211.9121  210.65156  227.7234  223.2779
    ##  9:  653.9685  652.3623  649.20064  651.8298  649.05077  674.9428  661.4847
    ## 10:  334.0670  332.4640  331.47531  329.4918  323.62816  290.4411  293.5295
    ## 11:  129.5192  129.4765  129.11320  132.4272  133.14812  145.5847  137.9085
    ## 12:  157.2383  152.0863  151.69819  150.3967  149.09823  165.5632  159.9452
    ## 13:  399.8694  393.3424  379.89605  367.1805  361.93047  345.7819  331.1963
    ## 14:  154.1795  153.4587  152.18864  150.5566  149.63548  178.8461  176.7730
    ## 15:  245.5002  222.3039  198.85576  198.3850  199.87770  292.7366  286.9163
    ## 16:  184.5895  186.8145  189.48573  189.0448  182.28777  177.6049  180.0888
    ## 17:  374.4507  367.0606  357.63947  351.3586  344.13991  423.7947  410.7635
    ## 18:  233.0782  230.5664  228.59692  228.0727  227.37551  269.7873  263.5363
    ## 19:  225.2625  223.1535  223.00722  220.6973  219.35925  226.8430  219.8546
    ## 20:  120.7337  117.3579  117.47756  115.9012  115.81476  127.7714  122.9065
    ## 21:  592.7621  609.6593  609.01108  610.6648  609.70227  606.2000  578.7938
    ## 22:  201.0910  197.8205  193.74245  191.7700  189.87342  196.0660  190.4661
    ## 23:  297.1589  294.3840  292.53410  288.0546  285.09813  320.4876  323.9869
    ## 24:  429.9405  426.5772  424.13334  423.1650  419.78532  481.0125  479.0757
    ## 25:  400.2920  391.2868  390.15065  389.6155  373.77376  395.0659  376.9883
    ## 26:  302.9093  295.2628  290.83197  295.8840  292.41872  283.2956  289.8399
    ## 27:  420.4248  418.4559  416.62619  414.8512  412.45653  378.9587  382.2896
    ## 28:  300.1241  297.2979  294.90423  290.2879  288.58158  374.2741  361.9923
    ## 29:  480.9687  475.9448  473.03792  482.9916  501.75763  477.7885  471.6382
    ## 30:  104.0517  101.9864   99.50671   98.7673   98.55422  104.9456  101.2200
    ## 31:  249.6760  248.4860  245.08971  249.4198  249.64432  362.3087  349.5206
    ## 32:  668.8549  644.9578  595.59312  606.7502  583.04195  739.1100  735.1324
    ## 33:  114.1778  110.9073  110.61751  108.2689  108.86874  172.0355  160.0969
    ## 34:  215.4350  218.6227  221.07636  221.7215  225.49442  244.9834  243.3229
    ## 35:  644.2163  618.5638  613.38427  580.3176  569.34914  634.0089  651.1682
    ## 36:  434.6524  429.2526  422.91144  415.9471  409.41596  359.1691  359.7549
    ## 37:  281.1356  286.2081  291.05187  293.7518  295.47251  257.6545  255.8393
    ## 38:  268.1236  275.7751  278.45188  279.6520  290.33018  304.0469  296.6711
    ## 39:  257.2386  257.6357  257.62900  255.5112  252.29479  284.7410  284.8986
    ## 40:  175.0139  174.4028  178.93182  175.4565  176.66497  159.8220  153.4773
    ## 41:  587.3132  566.6405  523.80550  511.8000  498.59630  589.7769  564.1553
    ## 42:  494.5545  494.4542  495.88734  496.9326  496.18005  448.5695  448.9673
    ## 43:  133.8749  132.4773  133.10586  132.4661  132.46839  163.1843  153.1530
    ## 44:  161.5328  159.6560  157.60655  163.5709  164.01921  185.6618  177.9190
    ## 45:  246.2866  241.7979  238.75676  235.4675  232.25723  278.5825  272.1292
    ## 46:  144.1097  139.5523  133.01642  133.6343  134.33602  169.4385  163.5027
    ## 47:  394.5056  389.8809  385.93903  381.7719  376.83909  402.0087  399.7210
    ## 48:  442.4321  433.5059  438.10501  440.8897  433.97986  434.1983  427.1939
    ## 49:  103.9618  102.7772  102.27404  101.7696  101.14597  107.5290  102.1196
    ## 50:  376.6431  373.1642  366.53664  359.5821  354.93661  331.7122  332.2420
    ## 51:  313.6844  304.6838  302.23915  300.0382  297.02637  328.6994  321.3092
    ## 52:  341.7210  336.0309  329.63307  312.3641  301.85567  422.5834  440.2091
    ## 53:  469.2057  463.8929  457.53506  455.3877  454.28154  519.7798  499.0081
    ## 54:  623.4263  604.2681  561.37484  549.7797  549.17392  727.6035  670.5814
    ## 55:  265.4794  263.2658  261.64853  263.3737  264.35120  295.0202  284.3568
    ## 56:  508.8793  479.5325  475.06741  473.5821  468.65816  581.1130  566.8383
    ## 57:  126.8360  122.9414  119.67184  119.1105  115.46880  152.9234  147.4189
    ## 58:  199.1521  194.9078  194.68540  192.1922  189.82663  213.9796  206.0197
    ## 59:  285.1164  279.8575  275.47821  256.0636  245.28215  297.6666  300.0728
    ## 60:  133.1462  130.1517  129.42940  127.5763  126.55350  152.9046  145.0924
    ## 61:  171.7927  168.7297  166.76857  165.8889  165.85687  186.9808  182.9116
    ## 62:  136.6451  131.3437  130.18495  128.8970  128.26304  150.5390  145.2646
    ## 63:  140.4503  143.4768  143.67565  144.4356  144.12692  163.4339  156.8782
    ## 64:  257.6763  263.8739  264.49251  263.7471  263.24424  289.8596  274.1400
    ## 65:  384.5168  382.3199  380.22339  374.8871  367.27550  375.2209  370.3533
    ## 66:  263.5185  257.5101  251.44671  242.1389  238.30965  234.6479  235.9888
    ## 67:  816.2680  779.5641  770.54121  792.6953  786.12722  705.0232  676.2279
    ## 68:  161.8943  159.0024  157.59097  160.8536  161.23918  181.8112  172.5648
    ## 69:  190.0117  190.8381  189.95961  194.0004  193.50528  191.8960  184.9128
    ## 70:  201.5381  198.5235  198.89818  198.1326  197.65413  218.9647  208.2510
    ## 71: 1190.7704 1157.1051 1127.84654 1097.1030 1059.62943 1156.2007 1131.6463
    ## 72:  443.7173  440.1349  436.88548  433.6069  429.93624  334.7621  335.3238
    ## 73:  536.2482  536.1051  541.04732  547.2489  550.65478  535.1088  530.9694
    ## 74:  385.9456  390.7901  386.09677  382.3781  377.53367  348.3714  349.1627
    ## 75:  328.9792  329.1120  328.68327  328.7911  328.95207  319.5653  324.2560
    ##     cvd15male cvd16male  cvd17male cvd18male  cvd19male cvd05both cvd06both
    ##      cvd07both  cvd08both  cvd09both cvd10both  cvd11both  cvd12both  cvd13both
    ##  1:  460.60996  452.32621  444.64431  437.7233  433.44997  426.84067  418.70568
    ##  2:  110.33258  109.31671  109.38333  109.1886  109.45548  108.87280  108.79150
    ##  3:  207.17746  196.63216  192.63435  192.5741  190.07306  186.71141  184.52990
    ##  4:  430.52893  425.23757  415.15513  405.3203  393.64054  380.12640  364.79531
    ##  5:  139.82629  136.26768  129.79965  124.4187  120.57405  115.18550  111.24268
    ##  6:  728.35144  724.46736  722.60586  725.7938  732.17587  739.57962  744.00434
    ##  7:  562.05477  555.95422  557.54021  557.6104  565.03762  511.81059  505.55688
    ##  8:  218.36193  214.14929  210.65046  206.9642  203.06461  196.27232  191.65734
    ##  9:  637.59710  613.86478  596.49917  588.2128  574.25279  554.67232  536.10535
    ## 10:  291.52971  293.89885  296.20140  295.0080  294.40157  294.23630  294.29118
    ## 11:  132.80556  127.64102  122.06046  117.4848  112.84500  110.15239  107.78578
    ## 12:  156.64075  149.47060  149.45466  149.4454  144.92481  141.73330  139.11717
    ## 13:  323.03156  322.82620  325.56755  329.3606  327.38597  318.12768  312.22116
    ## 14:  168.15789  164.18912  163.35846  157.6546  147.88498  143.73563  139.60139
    ## 15:  276.58795  260.43612  243.58089  235.8788  228.93682  222.03099  215.59362
    ## 16:  179.95002  178.65734  174.14429  173.9953  169.80081  170.33946  169.40528
    ## 17:  401.88320  372.50766  352.38871  324.6654  321.35422  311.63365  295.66392
    ## 18:  256.88434  250.72570  244.81017  238.6314  236.71013  234.75400  231.36934
    ## 19:  216.02209  210.59758  206.37477  202.0771  195.89295  191.83921  187.40416
    ## 20:  118.38057  115.21983  111.86647  108.2457  104.91807  102.55197   98.66495
    ## 21:  527.18166  509.26618  516.76011  516.8081  501.65563  477.14664  456.15895
    ## 22:  186.70085  182.87282  176.93814  171.4475  168.12856  167.37721  167.88856
    ## 23:  323.92028  324.82630  324.96080  324.9526  324.40649  322.18977  319.70475
    ## 24:  477.03676  474.64371  472.25177  471.2477  474.82817  472.17919  468.77018
    ## 25:  373.57240  358.27118  357.11928  350.1698  342.34062  335.85963  323.97174
    ## 26:  292.40922  294.01890  278.21052  267.7877  266.24712  266.18318  273.56444
    ## 27:  385.13312  388.81052  390.84498  391.6793  392.12659  394.74468  394.86398
    ## 28:  347.98144  334.19398  324.17606  313.9673  302.78960  294.40271  290.20596
    ## 29:  477.78218  481.00233  476.54323  469.3776  461.09061  451.65625  441.47726
    ## 30:   98.02780   95.29858   92.50782   91.1429   90.36691   87.76339   85.18238
    ## 31:  311.15301  289.05536  280.25829  280.4379  277.27695  268.15740  254.70058
    ## 32:  727.00327  698.76475  663.07312  651.3492  633.77423  612.75404  584.05483
    ## 33:  148.94438  137.66707  128.54118  122.2008  116.60889  111.60489  105.37675
    ## 34:  237.55346  258.02380  241.09178  212.5469  195.85098  193.39730  184.59528
    ## 35:  645.59071  632.76151  603.31409  581.8522  562.62250  543.92509  519.53712
    ## 36:  360.70097  361.21048  360.02652  357.6147  357.21133  354.74525  351.29656
    ## 37:  276.03466  275.14254  281.85429  271.3479  269.79218  264.17648  264.79606
    ## 38:  289.07676  291.31778  291.24859  279.5220  267.40258  260.45450  246.45538
    ## 39:  285.02779  286.46916  287.49451  287.1683  285.69447  284.03384  282.08967
    ## 40:  150.05326  153.58355  156.82087  155.0856  152.38083  152.30268  158.12378
    ## 41:  566.45157  552.62338  539.03210  535.4923  471.58396  460.08707  438.81955
    ## 42:  450.46983  447.63346  444.49796  440.5657  435.82483  434.99619  478.63029
    ## 43:  143.38486  136.70774  130.45554  126.0231  121.87538  119.48358  116.53352
    ## 44:  171.61004  165.77065  158.89188  153.3834  151.08008  146.70499  140.98230
    ## 45:  263.44130  259.09633  256.29837  253.4984  252.80350  252.46797  251.08995
    ## 46:  158.95594  152.46085  145.72668  140.0403  137.84394  132.83520  128.12389
    ## 47:  395.28927  392.86218  389.67772  387.0477  384.19966  381.28576  379.56730
    ## 48:  415.89329  407.15878  399.09041  394.3262  387.97737  366.84516  355.90010
    ## 49:   96.34764   94.97714  104.62926  106.4933  104.54989  102.44656   99.95400
    ## 50:  327.14809  328.81843  329.17447  326.6859  325.54049  325.47966  322.91254
    ## 51:  314.89091  305.63526  295.88473  278.8661  268.80871  265.16420  257.56935
    ## 52:  440.78565  426.09773  410.17610  390.0523  370.32276  356.00976  348.18911
    ## 53:  475.04244  466.40175  462.78879  453.2380  426.20664  420.42948  397.51644
    ## 54:  635.16266  628.21607  592.77514  585.0586  537.70788  514.58328  495.26564
    ## 55:  277.36035  272.41517  266.18998  263.2510  260.86178  260.27630  258.10484
    ## 56:  550.81185  545.61221  531.45941  507.4926  484.72607  474.11523  477.56554
    ## 57:  143.05822  137.56764  130.09757  125.0091  118.70562  115.15732  110.43077
    ## 58:  201.17286  191.42629  187.48356  182.4060  177.43258  173.97802  166.72494
    ## 59:  294.40234  290.39158  288.18423  285.3615  276.78557  267.91310  261.99365
    ## 60:  140.07197  134.51761  127.70454  121.8686  118.47492  115.86126  111.36104
    ## 61:  178.18444  172.12691  167.47419  162.5909  159.25538  155.26815  150.44817
    ## 62:  140.65615  135.93503  132.14623  126.7967  121.43801  119.48846  116.66521
    ## 63:  151.11043  145.88745  139.97204  137.2788  131.17322  125.29626  121.01879
    ## 64:  264.62657  266.62697  249.31019  240.6988  227.35270  215.54302  220.29064
    ## 65:  365.84010  362.32529  358.16606  354.5996  348.47268  344.41187  341.42073
    ## 66:  239.36203  246.26620  252.53793  251.3682  248.06823  245.41466  247.09799
    ## 67:  684.18530  682.22877  631.59255  614.4575  586.67859  578.93263  569.92172
    ## 68:  164.68453  158.18593  149.60425  144.0946  139.38594  137.93744  136.84127
    ## 69:  177.83355  173.23094  167.75332  162.9318  161.39944  159.14120  157.59806
    ## 70:  208.91637  194.57793  184.54962  180.0692  182.56672  177.80567  171.38959
    ## 71: 1103.74692 1112.63928 1102.33911 1116.2918 1135.63642 1141.29449 1125.34402
    ## 72:  336.52387  337.02684  337.15124  335.4539  333.01066  329.93117  326.60300
    ## 73:  525.70885  523.71933  517.18606  508.7890  507.49177  504.38733  501.99569
    ## 74:  344.28588  340.42203  336.02994  337.2284  340.77995  340.99547  341.73548
    ## 75:  329.74047  342.50771  349.08803  348.9636  341.36946  335.97243  331.35682
    ##      cvd07both  cvd08both  cvd09both cvd10both  cvd11both  cvd12both  cvd13both
    ##      cvd14both  cvd15both  cvd16both  cvd17both cvd18both cvd19both LEbirth2000
    ##  1:  415.16724  412.80644  405.90226  401.39469 399.50515 396.96522        73.5
    ##  2:  108.34739  108.12924  107.56173  106.95304 106.39570 105.71185          NA
    ##  3:  181.88855  183.42152  189.41453  190.14065 185.28922 183.93309        77.8
    ##  4:  360.00518  359.25122  351.23256  349.47783 348.19153 345.49496        74.9
    ##  5:  110.03050  109.24008  106.16151  106.36798 107.86810 108.12643        82.2
    ##  6:  757.21682  756.21653  758.33069  756.75780 710.38982 688.07822        68.5
    ##  7:  497.23919  484.40187  481.77658  479.93207 466.89588 463.10778        74.6
    ##  8:  187.13184  184.72431  184.51530  178.94359 176.98645 175.66145        75.2
    ##  9:  546.62593  541.93803  536.54177  532.73637 541.03249 541.08270        75.0
    ## 10:  291.55750  296.00206  294.76411  294.96240 293.75229 289.19088        53.1
    ## 11:  106.88829  105.33905  104.97617  104.49910 107.30944 107.92121        81.5
    ## 12:  135.23521  132.80626  128.24717  128.79163 127.94314 126.83604        79.8
    ## 13:  305.19407  297.25033  293.02531  286.83741 280.63999 276.93750        74.2
    ## 14:  136.10905  136.05210  134.46480  132.62149 132.47683 131.47500        77.8
    ## 15:  212.30069  209.95313  202.90815  196.71239 192.06049 190.26941        81.0
    ## 16:  168.52380  166.17046  167.16092  168.67782 168.10363 163.65500        77.3
    ## 17:  290.15334  276.50832  279.68926  277.05514 274.20179 270.55998        76.2
    ## 18:  229.49538  228.51537  226.57527  225.23902 225.38551 225.38539        51.7
    ## 19:  182.96093  178.26308  175.55012  175.44476 174.60136 173.80771        80.9
    ## 20:   94.51047   94.28790   91.45197   91.33064  91.15499  91.41488        82.5
    ## 21:  447.40141  443.56185  456.53049  457.65120 456.06578 453.99935        73.1
    ## 22:  163.79547  166.21079  164.47294  161.48904 160.29687 159.35918        80.9
    ## 23:  316.18344  318.52444  316.32848  315.05671 311.95072 310.60655        60.8
    ## 24:  465.82635  464.27802  459.66137  455.40585 452.87085 449.11377        57.2
    ## 25:  319.83137  322.44798  307.73607  309.65678 310.65905 301.61621        75.6
    ## 26:  271.45865  264.15079  258.48563  255.25524 258.77283 256.36711        62.9
    ## 27:  394.48450  394.73382  392.00034  389.44109 386.56637 383.26115        68.6
    ## 28:  286.31401  287.96550  286.88535  285.32918 279.68642 277.73359        74.6
    ## 29:  433.34843  429.06362  422.81317  418.20754 425.24034 436.19081        71.3
    ## 30:   82.57175   80.18604   78.84510   77.08885  77.00931  77.00994        84.4
    ## 31:  249.19197  248.28335  248.26387  246.40087 248.57947 248.88754        72.5
    ## 32:  556.36666  536.19078  519.14503  485.39932 485.31026 476.01008        69.2
    ## 33:  100.13781   97.49873   95.22265   95.17711  94.68896  95.40530        79.8
    ## 34:  177.12714  178.35299  179.50619  181.00846 181.17922 184.57616        80.1
    ## 35:  513.01612  523.15984  500.14376  493.82082 473.26712 466.31300        70.2
    ## 36:  347.82812  343.85649  338.70321  333.17316 327.48605 322.29636        76.1
    ## 37:  268.61704  272.21233  276.95465  280.36079 281.76929 282.07693        76.3
    ## 38:  248.03609  247.15687  249.70380  250.28741 250.11953 255.54454        75.3
    ## 39:  279.73392  282.44832  281.89484  280.58319 279.03154 276.18486        52.7
    ## 40:  159.19817  158.44430  157.61612  159.67123 156.84117 157.26956        77.1
    ## 41:  449.03836  463.27943  456.27984  432.82716 423.77893 404.95508        70.5
    ## 42:  474.52529  471.20181  468.07610  465.99838 463.40321 459.50111        70.6
    ## 43:  113.26423  113.28973  112.95751  113.04848 113.03565 113.01010        80.4
    ## 44:  140.23894  137.82627  135.76510  134.28600 137.96721 137.92603        81.1
    ## 45:  246.82989  249.11815  244.38574  241.54042 238.38554 235.65202        55.2
    ## 46:  121.65215  117.42773  114.79198  110.80097 112.06587 111.48064        81.2
    ## 47:  376.31985  373.01182  368.99134  365.49359 361.99122 357.88370        61.0
    ## 48:  368.85654  380.88863  384.46844  388.17552 386.67955 381.69777          NA
    ## 49:   95.08345   91.54823   90.28000   89.85563  89.23082  88.61384        75.9
    ## 50:  320.55214  321.36521  318.50800  315.14778 310.89918 307.32206        72.9
    ## 51:  247.03456  246.60147  239.14086  237.07020 236.95143 235.58774        77.9
    ## 52:  351.45387  379.26155  373.23720  366.86193 348.93461 337.29764        71.7
    ## 53:  400.02892  391.65760  384.18410  379.99900 378.78972 379.62640        74.9
    ## 54:  490.38527  475.65621  461.71309  434.85592 431.69136 432.91864        72.3
    ## 55:  256.92512  255.83494  254.10575  253.25939 255.36438 256.67638        49.2
    ## 56:  482.73602  478.06378  458.04063  453.96042 453.05802 448.83636        74.3
    ## 57:  106.32256  102.22058   98.56112   95.85146  95.34721  93.68210        80.7
    ## 58:  155.26424  157.55448  153.27578  153.01437 152.09684 151.14320        79.7
    ## 59:  262.97552  263.14625  258.29303  251.40748 232.26968 222.65017        58.6
    ## 60:  109.44359  110.37097  107.46888  106.87928 106.99528 106.78356        82.5
    ## 61:  145.54025  142.50612  141.00249  139.76469 140.20408 138.59168        81.8
    ## 62:  113.17945  111.52252  108.60315  107.67469 107.36517 107.06392        82.3
    ## 63:  119.09116  117.90308  119.14971  118.67228 118.96768 118.66561        75.2
    ## 64:  222.12845  222.78825  227.76604  228.23401 227.86298 227.29800        72.6
    ## 65:  337.68476  334.17972  331.36592  328.54926 323.75365 317.87661        76.5
    ## 66:  243.97917  235.98434  232.34278  228.48935 221.14770 217.45921        76.8
    ## 67:  534.81353  609.75995  588.08212  581.24578 594.30207 591.00624        73.2
    ## 68:  133.69815  132.99741  130.60903  129.12143 132.02638 132.37531          NA
    ## 69:  156.64527  156.49798  156.35053  154.30903 155.74220 157.00954        79.2
    ## 70:  165.97775  164.96208  162.32582  162.77541 161.88740 161.55863        78.8
    ## 71: 1107.01301 1091.31310 1056.26572 1026.66904 992.40366 945.97305        68.2
    ## 72:  323.18282  319.13626  314.95461  311.23748 307.70802 303.90466        75.6
    ## 73:  497.81417  500.90386  500.85723  504.82748 509.87687 513.09104        64.7
    ## 74:  342.88671  346.10827  349.73020  346.15135 343.09096 339.55436        45.2
    ## 75:  330.58252  329.56980  327.63566  324.91664 322.57215 321.02280        48.1
    ##      cvd14both  cvd15both  cvd16both  cvd17both cvd18both cvd19both LEbirth2000
    ##     LE602000 LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019
    ##  1:     20.3        76.8     21.9        77.5     22.3        78.1     22.6
    ##  2:       NA          NA       NA          NA       NA          NA       NA
    ##  3:     22.5        78.6     22.7        79.3     23.0        79.5     23.1
    ##  4:     20.2        76.6     20.5        77.8     21.2        79.2     22.1
    ##  5:     24.9        84.0     26.1        84.2     26.3        84.8     26.8
    ##  6:     17.9        71.8     17.6        73.4     17.9        74.1     18.3
    ##  7:     19.4        76.4     20.5        78.8     21.9        79.6     22.5
    ##  8:     21.3        77.7     22.4        78.6     23.0        79.4     23.5
    ##  9:     19.2        77.2     20.9        78.0     21.5        78.6     22.0
    ## 10:     16.6        60.8     17.4        63.4     17.8        65.2     18.1
    ## 11:     24.3        83.3     25.7        83.6     26.0        84.1     26.4
    ## 12:     23.1        81.3     24.2        82.5     25.3        83.2     25.9
    ## 13:     20.1        77.9     21.5        79.9     22.7        80.5     23.1
    ## 14:     23.1        80.1     24.3        81.0     24.7        81.9     25.3
    ## 15:     23.2        83.1     24.8        84.1     25.6        85.1     26.4
    ## 16:     23.1        77.9     22.8        79.5     23.6        80.5     24.3
    ## 17:     20.9        80.4     23.2        81.7     24.3        82.6     25.0
    ## 18:     14.9        64.5     18.3        68.6     19.1        70.5     19.4
    ## 19:     23.6        83.0     25.3        83.9     25.7        84.0     25.8
    ## 20:     25.3        84.3     26.8        84.7     26.9        85.1     27.2
    ## 21:     18.4        76.3     20.1        76.9     20.5        77.8     21.0
    ## 22:     23.6        82.5     24.8        82.7     24.8        84.8     26.9
    ## 23:     17.3        64.5     17.9        67.4     18.6        69.2     18.9
    ## 24:     15.9        35.4     12.0        63.1     16.7        64.8     17.0
    ## 25:     20.1        78.4     21.8        78.7     21.9        79.6     22.3
    ## 26:     16.8        68.9     19.0        70.6     19.3        72.2     19.5
    ## 27:     18.2        71.0     18.4        72.5     18.8        73.3     19.1
    ## 28:     20.5        78.0     22.3        78.5     22.3        79.1     22.5
    ## 29:     19.6        72.8     20.0        73.4     20.5        75.0     20.7
    ## 30:     26.6        85.8     27.7        86.4     28.1        86.9     28.6
    ## 31:     17.9        77.6     21.4        78.8     22.1        78.8     22.1
    ## 32:     17.8        73.4     19.2        76.2     20.6        77.6     21.4
    ## 33:     22.7        83.8     25.9        85.1     26.9        86.1     27.9
    ## 34:     23.5        81.4     24.0        83.8     25.9        83.9     25.9
    ## 35:     18.3        73.3     19.3        75.3     20.2        77.3     21.7
    ## 36:     20.7        78.1     21.7        78.8     22.1        79.2     22.4
    ## 37:     22.0        77.3     22.0        76.9     21.9        77.3     21.9
    ## 38:     19.2        76.5     20.1        77.1     20.7        77.1     20.6
    ## 39:     15.9        59.5     17.0        61.4     17.2        63.4     17.6
    ## 40:     22.7        78.0     22.7        78.6     22.9        78.9     23.1
    ## 41:     17.2        73.1     18.2        75.0     19.5        77.1     20.9
    ## 42:     18.9        72.3     18.8        73.6     19.2        74.3     19.6
    ## 43:     23.2        82.5     24.8        82.8     24.9        83.1     25.1
    ## 44:     24.1        82.7     25.4        82.9     25.3        83.5     25.8
    ## 45:     17.0        60.8     18.0        62.8     18.5        64.1     18.9
    ## 46:     23.7        82.9     25.0        83.7     25.4        84.1     25.8
    ## 47:     16.4        63.9     17.1        65.4     17.4        66.7     17.8
    ## 48:       NA          NA       NA          NA       NA          NA       NA
    ## 49:     22.9        78.9     23.8        80.6     24.7        81.3     25.1
    ## 50:     19.7        73.5     19.9        73.7     19.7        73.6     19.6
    ## 51:     21.4        80.4     23.3        81.1     23.7        81.9     24.3
    ## 52:     15.2        73.9     16.1        75.9     17.9        76.6     18.4
    ## 53:     19.7        77.4     21.1        78.4     21.9        79.3     22.4
    ## 54:     18.7        74.7     20.2        76.6     21.4        78.0     22.2
    ## 55:     15.0        66.5     18.6        69.8     19.1        71.2     19.2
    ## 56:     18.4        76.8     20.0        77.8     20.7        78.3     21.1
    ## 57:     23.1        84.0     25.8        84.9     26.7        85.5     27.2
    ## 58:     22.7        82.7     24.8        83.3     25.2        84.1     25.6
    ## 59:     18.0        59.2     18.5        65.3     19.1        68.3     20.5
    ## 60:     24.8        84.6     26.5        84.9     26.7        85.7     27.3
    ## 61:     24.1        83.1     25.0        83.5     25.3        84.0     25.6
    ## 62:     24.8        84.2     26.2        84.5     26.2        85.1     26.6
    ## 63:     22.1        79.3     23.8        80.7     24.7        81.0     24.8
    ## 64:     19.8        75.7     21.5        78.3     23.7        79.9     25.0
    ## 65:     21.5        78.2     22.1        78.8     22.5        79.2     22.8
    ## 66:     22.7        79.1     22.6        80.1     22.9        80.7     23.2
    ## 67:     18.8        75.1     19.8        77.0     21.1        77.8     21.7
    ## 68:       NA          NA       NA          NA       NA          NA       NA
    ## 69:     22.8        80.8     24.2        80.8     24.4        80.7     24.4
    ## 70:     23.1        80.0     23.7        80.6     24.1        80.6     24.0
    ## 71:     17.1        72.9     18.7        73.9     19.0        75.2     19.8
    ## 72:     21.0        77.1     21.5        77.6     21.8        78.1     22.0
    ## 73:     17.9        69.6     18.7        69.6     18.9        68.9     18.7
    ## 74:     14.7        59.1     17.1        63.0     17.7        65.4     18.0
    ## 75:     17.1        53.2     15.4        61.0     16.5        63.6     17.0
    ##     LE602000 LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019
    ##      phy05  phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14
    ##  1: 1.0242     NA 1.1958     NA     NA 1.2070     NA     NA     NA     NA
    ##  2: 3.2319 3.0123 3.0109     NA 3.1479 4.0000     NA     NA     NA     NA
    ##  3: 3.2100     NA     NA     NA     NA 3.2100     NA     NA 3.9385     NA
    ##  4: 2.5643 2.5897 2.6724 2.7432 2.7679 2.8419 2.8677 2.8968 2.9031 2.8928
    ##  5: 2.7794 2.8338 2.9954 3.0056 3.1085 3.3429 3.2878 3.2858 3.3530 3.4314
    ##  6: 3.5819 3.5649 3.7124 3.6844 3.6751 3.6629 3.4376 3.4901 3.4558 3.4460
    ##  7: 3.3145 3.3795 3.5306 3.1477 3.2845 3.2498 4.7611 4.8352 4.8534 5.0120
    ##  8: 1.6663 1.7007 1.7306 1.7802 1.8171 1.8139 1.8491     NA 1.8820     NA
    ##  9: 3.6651 3.6805 3.6800 3.6488 3.7401 3.7661 3.8467 3.8995 3.9630 3.9750
    ## 10:     NA 0.0280     NA 0.0322 0.0446 0.0457 0.0386 0.0475 0.0470 0.0487
    ## 11:     NA 1.9086     NA 1.9590     NA 2.0384     NA 2.3962 2.4509 2.4961
    ## 12: 0.9804 1.0404 1.0365 1.0389 1.0294 1.4333 1.5854 1.7471 1.8806 2.0278
    ## 13: 1.2127 1.2470 1.2676 1.3166 1.3922 1.4532 1.4637 1.5367 1.6332 1.6877
    ## 14: 1.4727 1.4959 1.5192 1.5428 1.5668 1.5917 1.6966 1.7400 1.8431 1.8534
    ## 15: 1.8965 1.8374 2.0147 2.0507 2.1064 2.1562 2.2484 2.2960 2.4076 2.4993
    ## 16:     NA     NA     NA     NA 1.5983 2.1111 1.6582     NA     NA     NA
    ## 17: 3.1430 3.1762 3.2508 3.3348 3.2767 3.2422 3.2929 3.2822 3.3318 3.3564
    ## 18: 0.0321 0.0269 0.0224 0.0251 0.0252 0.0220     NA     NA     NA     NA
    ## 19: 2.9913 3.0262 3.0351 3.0618 3.0861 3.2653 3.1221 3.2854 3.2966 3.3922
    ## 20: 3.4410 3.4357 3.4189 3.4156 3.3693 3.3736 3.1622 3.1745 3.1948 3.2115
    ## 21: 3.8164 3.8683 3.9027 4.0001 4.2219 4.4466 4.5004 4.4868 4.5145 4.7754
    ## 22: 3.4314 3.4748 3.5287 3.5863 3.6638 3.7567 3.8491 3.9197 4.0087 4.0846
    ## 23:     NA     NA 0.0725 0.0787 0.0841 0.0938 0.0979 0.0956 0.1686     NA
    ## 24:     NA     NA     NA     NA     NA     NA 0.1378     NA     NA     NA
    ## 25: 2.7816 3.0406 2.8121 3.1049 3.0399 2.8896 2.9811 3.1062 3.2284 3.3443
    ## 26: 0.5758 0.5852 0.5984 0.6136 0.6220 0.6616 0.7376 0.6982 0.7182 0.7247
    ## 27:     NA 0.1300 0.2880     NA 0.1448 0.1395     NA 0.3075 0.3118     NA
    ## 28: 0.8869 0.5361     NA     NA     NA 0.8900     NA     NA     NA 1.5044
    ## 29: 0.6600     NA 0.5280     NA     NA 0.6636     NA     NA     NA 1.0364
    ## 30:     NA 2.0746     NA 2.1394     NA 2.2059     NA 2.2740     NA 2.3413
    ## 31: 2.2390 2.2911 2.4426 2.2231 2.1287 2.2326     NA     NA 2.1947 2.2038
    ## 32: 3.6038 3.6983 3.6547 3.7161 3.7808 3.9278 3.9483 3.9108 3.9508 3.9800
    ## 33: 1.7529 1.8047 1.8655 1.8407 1.9185 1.9839 2.0361 2.0798 2.1632 2.2070
    ## 34: 1.8000 1.4543 1.8814 1.8806 1.8929 2.4296 2.4560 2.4662 2.5332 2.6522
    ## 35:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 36:     NA 2.4707 2.3464     NA     NA 2.3385 2.6201     NA 2.2486 2.0735
    ## 37:     NA     NA     NA 1.8425 1.9578 1.9000     NA     NA     NA 2.0583
    ## 38:     NA     NA     NA 0.9216     NA 1.1691 1.2777 1.3320     NA     NA
    ## 39:     NA     NA 0.0776 0.0490 0.0923 0.1021 0.1037 0.1071     NA     NA
    ## 40: 1.7651 1.8810 1.9215 1.9494 1.9790 2.2362 2.1004 2.1118 2.1568 2.2070
    ## 41: 2.3783 2.3768 2.3871 2.3545 2.3912 2.3808 2.4142 2.4135 2.5028 2.4746
    ## 42:     NA     NA 0.5862     NA 0.6477 0.6200     NA     NA 0.6320 0.9135
    ## 43: 2.6997 2.7791 2.7724 2.8490 2.9057 2.9516 3.1243 3.2463 3.3058 3.3468
    ## 44: 2.7942 2.8402 2.9100 2.9793 3.0477 3.0658 3.1731 3.1957 3.2303 3.2374
    ## 45: 0.2824 0.3481 0.3784 0.3762 0.3782 0.1836     NA     NA 0.3828     NA
    ## 46: 2.2960 2.3597 2.4069 2.4077 2.4756 2.4949 2.5441 2.5532 2.5991 2.6832
    ## 47: 0.7882     NA 0.7619 0.7806 0.7951 0.8076 0.8311 0.8590 0.8771 0.8972
    ## 48:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 49:     NA     NA 1.6648     NA 0.9472 0.9200     NA 1.1411     NA     NA
    ## 50: 1.2289 1.2361 1.2441 1.2558 1.2632 1.2717     NA     NA     NA     NA
    ## 51: 2.1267 2.1671 2.1776 2.1482 2.1593 2.1707 2.1997 2.2242 2.2340 2.3020
    ## 52: 2.4844 2.2617     NA 3.2087 3.0975 3.7273     NA     NA     NA 1.7351
    ## 53: 2.2126 2.2104 2.2915 2.4133 2.4414 2.4804 2.5153 2.5887 2.6240     NA
    ## 54: 2.3204 2.3721 2.3841 2.3812 2.3924 2.3930 6.6305 4.1303 4.0705 4.0114
    ## 55: 0.0240     NA     NA 0.0520 0.0550 0.0550 0.0129     NA 0.1021 0.0909
    ## 56: 2.2609 2.2858 2.3447 2.4215 2.4467 2.4820 2.5003 2.4961 2.4945 2.4611
    ## 57: 1.5819 1.5744 1.6127 1.6418 1.6758 1.7187 1.7232 1.7964 1.8958 2.0125
    ## 58: 2.3519 2.3672 2.3925 2.3994 2.4167 2.4367 2.4965 2.5406 2.6252 2.7628
    ## 59:     NA     NA 0.6988 0.6968 0.7074 0.7341 0.7179 0.7254 0.7422 0.7541
    ## 60: 2.7605 2.8206 2.8747 2.9847 3.0395 3.0292 3.0473 3.1396 3.1425 3.1657
    ## 61: 3.5107 3.5960 3.6760 3.7353 3.8065 3.8779 3.9569 4.0368 4.1259 4.2037
    ## 62: 3.8207 3.8589 3.8518 3.8298 3.8476 3.8166 3.8355 3.9102 4.0303 4.1171
    ## 63: 0.2930 0.2875 0.2959 0.3164 0.3387 0.3906     NA     NA     NA     NA
    ## 64:     NA 1.2222 1.1792     NA 1.5081 1.8086 1.8193     NA     NA     NA
    ## 65: 0.9322     NA 1.0242 1.1074     NA 1.2220     NA     NA     NA 1.2769
    ## 66: 1.4852 1.5195 1.5579 1.6068 1.6635 1.7068 1.7160 1.7384 1.7619 1.7560
    ## 67: 3.0181 3.0838 3.0885 3.1177 3.4904 3.4830 3.4842 3.4910 3.4965 2.9923
    ## 68: 2.4120 2.4590 2.4844 2.5543 2.6279 2.6265 2.6603 2.6667 2.6774 2.7149
    ## 69: 2.4356 2.4285 2.4358 2.4412 2.4471 2.4354 2.4641 2.4985 2.5596 2.5740
    ## 70:     NA     NA 4.1810 3.9510     NA 3.7360     NA     NA     NA     NA
    ## 71: 2.6701 2.6515 2.6329 2.6048 2.5706 2.5432 2.5036 2.4451 2.4044 2.3742
    ## 72:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 73:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 74: 0.0545 0.0533     NA 0.0619 0.0606 0.0614 0.1649 0.1658     NA     NA
    ## 75: 0.1255     NA 0.0544 0.0596 0.1210 0.1272 0.0817 0.0807 0.0834 0.1240
    ##      phy05  phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14
    ##      phy15  phy16  phy17               HE05               HE06
    ##  1:     NA 1.8325 1.7879 3.2351613000000001         3.35510325
    ##  2: 3.3333     NA     NA 5.5754260999999996 4.9350943599999999
    ##  3:     NA 4.0013 3.9901 7.6107888199999998 7.6403284100000004
    ##  4: 2.9143     NA 4.4023 5.8618803000000002 5.8500709500000001
    ##  5: 3.4886 3.5672 3.6778 7.9900822600000003 7.9894385300000001
    ##  6:     NA     NA     NA 2.2610766899999999 2.0667839099999998
    ##  7: 5.1905     NA     NA 6.2765383699999999 5.7629585299999997
    ##  8:     NA     NA 2.1652 8.0440778700000006 8.2589139899999999
    ##  9: 4.0332     NA     NA 6.8901143100000004         6.52544594
    ## 10: 0.0657 0.0645 0.0847 4.4425587699999998 5.0915155399999996
    ## 11: 2.5388 2.3105 2.6102 9.0643863699999994 9.3451557199999993
    ## 12: 2.1470 2.2937 2.4411 6.5887155499999999 6.0351672199999999
    ## 13: 1.7732 1.8647 1.9798 4.1422152499999996         3.91870999
    ## 14: 1.9742 2.0335 2.1064 6.0670981399999997 6.3355150199999999
    ## 15: 2.6236 1.9509     NA 5.4358096099999997 5.4635748900000003
    ## 16: 2.0648 2.0368     NA         5.57841349 5.6872220000000002
    ## 17: 3.4152 3.4546 3.4629 5.0580143900000003 4.7653036100000001
    ## 18:     NA 0.0464 0.0986 4.1009812400000003 4.4575729400000004
    ## 19: 3.3309 3.8118     NA 8.2879562399999998 8.2841176999999995
    ## 20: 3.2239 3.2376 3.2565         10.2148447        10.39308834
    ## 21: 5.0055 5.9973 6.1297 8.3039331399999998 7.8462653199999997
    ## 22: 4.1342 4.1944 4.2488        10.31149673        10.17961693
    ## 23:     NA 0.1270 0.1359 3.9777960800000001 3.9613141999999999
    ## 24: 0.0852     NA     NA 5.5093555500000004 5.5010376000000001
    ## 25: 3.1178 3.2313 3.3447         8.00078201 7.7873735399999999
    ## 26:     NA 0.7590 0.7779 3.7911624900000001 3.6347780200000002
    ## 27: 0.2738     NA 0.3767         2.58409047         2.67202187
    ## 28: 1.1526     NA 1.1292         5.30572176 5.1986260399999997
    ## 29:     NA 0.8536 0.8375 2.8973991899999998 2.4224786800000002
    ## 30:     NA 2.4115     NA 7.7806687400000003         7.80784273
    ## 31: 2.8078 1.3954 2.3237 8.8627452899999994 8.0562067000000006
    ## 32:     NA     NA     NA 3.9017238600000002         3.39824891
    ## 33: 2.2494 2.3037 2.3608 4.6178216900000004 4.9439678200000001
    ## 34: 2.6463     NA     NA 2.3776223700000001         2.25313878
    ## 35:     NA     NA     NA               <NA>               <NA>
    ## 36: 2.1030 2.0115 2.0255 7.5512127900000001 8.6503219599999994
    ## 37: 1.8943     NA 2.0905 2.5977468500000001         2.51467609
    ## 38: 1.5358     NA     NA 2.8012416400000002 3.1245324600000002
    ## 39:     NA 0.1395     NA 5.1953811600000002 5.4565606100000004
    ## 40: 2.3246 2.3244 2.3827 5.8361215599999996 5.6552553200000002
    ## 41: 2.4836     NA 3.2066 7.9639606499999998 8.7857027100000007
    ## 42:     NA     NA 0.7308 4.7646975500000002         4.94123936
    ## 43: 3.4917 3.5470 3.6054 9.0966682399999996 9.0805511499999998
    ## 44: 3.2961 3.3833 3.4728 8.2733488099999999         8.63299561
    ## 45:     NA 0.4494     NA 4.4659194900000001 4.2577514599999997
    ## 46: 2.6474 2.6983 2.8342 8.3283958400000007 7.9127221099999998
    ## 47: 0.9262 0.9620 1.0005 2.7079358099999999 3.0071713899999999
    ## 48:     NA     NA     NA               <NA>               <NA>
    ## 49:     NA 1.3048     NA 4.5863194500000004 4.5380153700000001
    ## 50:     NA     NA 0.6004 3.9004185200000001 3.9458506099999999
    ## 51: 2.3252 2.4146 2.3788         5.80728388 5.8007226000000003
    ## 52:     NA 2.6944 0.0008 2.5690882199999998 2.2033810599999999
    ## 53:     NA 2.2565 2.9807 5.5277814899999997 5.0710606599999997
    ## 54: 3.7494 4.0139     NA         4.76693487         4.76170969
    ## 55: 0.1129 0.1226 0.1371         8.77702618 7.8278689400000001
    ## 56: 2.4603 3.1131     NA 8.2838058500000002         8.49389839
    ## 57:     NA 2.2936     NA         3.03367925 2.9347465000000001
    ## 58: 2.8148 3.0007 3.0861 7.9959573700000002 7.8232274100000003
    ## 59: 0.7814 0.7997 0.9054 6.7057571400000002 6.5870618800000003
    ## 60: 3.2166 3.8112 3.8723 7.7319531399999999 7.8291268299999999
    ## 61: 4.2856 3.9840     NA 8.1469831500000005 8.0473642299999995
    ## 62: 4.1898 4.2473 4.2957 10.294388769999999 9.8183813099999995
    ## 63: 0.4651 0.4450 0.8075 3.1597218499999999 3.0997068900000002
    ## 64: 2.6498     NA 3.3646 4.4345116600000001 4.1543202399999997
    ## 65: 1.3108 1.2834 1.3025 5.3973608000000004 5.3821816399999998
    ## 66: 1.7988 1.8142 1.8492 4.8938970599999996 5.1459169400000002
    ## 67:     NA     NA     NA 6.3461584999999996 6.4169516599999996
    ## 68: 2.7465 2.7563 2.7863 8.5339870500000004 8.6973781599999995
    ## 69: 2.5781 2.5881 2.6120 14.605676649999999        14.70215797
    ## 70:     NA 3.9558 5.0794 8.4265871000000008 8.4113969799999992
    ## 71:     NA     NA     NA 4.8773298299999999 4.8463411299999999
    ## 72:     NA     NA     NA               <NA>               <NA>
    ## 73:     NA     NA     NA               <NA>               <NA>
    ## 74:     NA 0.1628     NA 6.8633222600000003 5.8736734400000001
    ## 75: 0.1815 0.1788 0.1859               <NA>               <NA>
    ##      phy15  phy16  phy17               HE05               HE06
    ##                   HE07               HE08               HE09               HE10
    ##  1: 3.8214178099999998 4.2018823599999999 5.3593983700000001 5.1171698599999997
    ##  2: 4.9255475999999998 5.8059859300000003 6.2023372700000001         6.64963865
    ##  3: 7.8345508600000002 8.1826972999999992 9.4559955599999999 9.4454641299999995
    ##  4:         5.49814034 6.6397657399999996 8.4437761299999998 9.2353830299999995
    ##  5: 8.0675535200000006 8.2558565099999992 8.5631647100000006 8.4308423999999995
    ##  6:         1.99021435         1.93880868 2.6137836000000001 2.4899666300000001
    ##  7: 5.8853402099999998         5.47281408 5.3903784799999999 5.6569399799999998
    ##  8: 8.2077617600000004 8.0159883500000007 8.4025468799999992 7.9491324399999996
    ##  9: 6.1603593800000001 6.2877101900000003 6.5988688499999997         7.14063406
    ## 10: 5.8863782899999997 5.2273888599999996 5.7152666999999999 5.8929595900000002
    ## 11: 9.4389820100000001 9.5985326799999999 10.709112169999999         10.6837883
    ## 12: 6.1753754599999997 6.7404789899999997 7.2091846500000001 6.7748932799999997
    ## 13: 3.6586847300000001 3.8772277800000001 4.3218240699999999 4.2079915999999997
    ## 14: 6.4910283099999999 6.8419270499999998 7.3113465299999998 7.0713014599999999
    ## 15: 5.3574485799999998         6.01427698 6.4653158199999998 6.5176711100000002
    ## 16: 5.8648901000000002 5.8163766900000002 6.4387331000000003 7.1219563499999996
    ## 17: 4.9833087899999997 5.7103238100000002 6.5121889099999999 6.2722482700000004
    ## 18: 5.0012836500000004 4.2806391699999997 4.6498341600000002 5.4663720099999997
    ## 19: 8.0835933700000009 8.3431062699999998 9.1612224599999994 9.1420278499999998
    ## 20: 10.331021310000001        10.51212215        11.29670525 11.240113259999999
    ## 21: 7.6438741700000001 8.6815204599999998 9.8354272799999993 9.5453891800000008
    ## 22:        10.05104923         10.2510128        11.23758507 11.096864699999999
    ## 23: 4.0661277800000004 4.1973075900000003 4.7066750500000003 4.6920676200000004
    ## 24: 5.9015803299999998 6.0159010899999998 6.1691598900000004 8.1455984099999998
    ## 25: 7.2152929300000004 7.0968809100000003 7.2392807000000001          7.5076437
    ## 26: 3.5175421199999999         3.51468062 3.4853787399999998 3.2721190500000001
    ## 27: 2.8761706399999998         2.61300087 2.6849434400000001 2.9608998299999998
    ## 28:         5.03977919 5.2816843999999996 6.5595455200000004 6.7547311800000003
    ## 29: 3.1063773600000002         3.03573871 3.7682588099999998 3.2305560099999999
    ## 30: 7.8904604899999997 8.1995143899999992 9.0582961999999991 9.1567735700000004
    ## 31: 8.3010473299999994 8.7539281800000008 9.5021286000000007 8.3892974900000006
    ## 32: 2.7031483700000001 3.0495264500000001 3.4991376399999998 2.7364139600000001
    ## 33: 5.1149072599999998         5.39896727 5.7822899799999998 5.9173440900000003
    ## 34: 2.1331450900000002         1.93336701 3.8693599700000001 2.7570362099999999
    ## 35:               <NA>               <NA>               <NA>               <NA>
    ## 36: 8.5439138400000001 7.7885041199999998 6.9920244199999999 7.4433217000000003
    ## 37: 2.4379138899999999 2.6357171500000001 4.2881603200000002         3.58541131
    ## 38:         3.08224082 3.0235738799999998 3.2762939900000001         3.18466234
    ## 39: 5.2536678300000004 5.0454120600000003 5.2291111900000002 4.6792340299999999
    ## 40:         5.76644516 5.6988697100000003 6.1297845799999999 5.9751300799999996
    ## 41: 9.1826009800000001 9.1230525999999994        11.39545822 10.131128309999999
    ## 42: 5.4229354900000004 5.3025169400000003 5.8412446999999998         5.86408854
    ## 43: 9.0529251100000003 9.2766046499999995 9.9929266000000005        10.15508747
    ## 44: 8.3210659000000007 9.1129550899999998 9.6220016499999996 9.5861282299999999
    ## 45:         3.90997219 3.6958153199999999 3.5801973299999998 3.2965328700000001
    ## 46:         8.05085373 7.9607529599999998 9.0743150700000008 8.9055318799999998
    ## 47:         3.13823009         2.92312145 2.6133124799999998 2.5967585999999998
    ## 48:               <NA>               <NA>               <NA>               <NA>
    ## 49: 4.4038014399999996 4.4459280999999997 4.9506058700000004 4.7206191999999998
    ## 50: 3.9195413600000002 4.0279226299999999 4.3535318399999996 4.3127627400000002
    ## 51: 5.8729419700000003 6.3734822299999996 6.5868630399999999 6.4190917000000001
    ## 52:          1.9733299         1.60012186 2.1495227799999999 1.7892390499999999
    ## 53: 5.0222334899999996 5.0227422700000002 5.2784128199999998 5.7690625200000003
    ## 54: 4.7431106600000001 4.8986678100000001 5.6382026700000001 4.9660777999999999
    ## 55: 8.2388839699999998 7.9480862600000002 8.1252126699999998 8.4612665200000006
    ## 56: 9.3764915500000008 9.4824638399999994 9.3462324100000007 9.5273036999999992
    ## 57:         2.83749056 3.1948845399999999         3.39853096 3.2026536499999998
    ## 58: 7.5113916400000003 7.8546657599999996 8.5408019999999993 8.5650329599999999
    ## 59: 6.4313159000000004 6.5457606300000002 7.1145868300000004         7.41535425
    ## 60:          7.9183569 8.3818778999999992 9.1070718799999995 9.1185750999999993
    ## 61: 7.9901738199999999 8.2066011400000001 8.7731924100000001 8.3273897199999993
    ## 62: 9.6294012099999993         9.75355244        10.37947655        10.27625465
    ## 63: 3.1927750100000001 3.4593396200000002          3.6193974         3.39009976
    ## 64: 4.1536026000000001 3.6575703599999998         5.48676443         5.08042908
    ## 65: 5.4285693200000003 5.4171466800000001         5.67045975 5.8839559599999998
    ## 66: 5.2376079600000001 5.2176179899999999 5.4943871499999997         5.02053452
    ## 67: 6.0170521700000004 5.4972801200000001 6.6077189399999998 6.8122901899999997
    ## 68: 8.8732242600000006 9.1662340199999992        10.01600361 9.9891519500000001
    ## 69:        14.91858959        15.28626251        16.28372383 16.345378879999998
    ## 70: 8.1404838599999998 8.8268699599999998 8.6362743399999999 8.5942153900000005
    ## 71: 4.6331453299999996 4.7983584400000003 4.8924522399999999 5.1175293899999996
    ## 72:               <NA>               <NA>               <NA>               <NA>
    ## 73:               <NA>               <NA>               <NA>               <NA>
    ## 74: 4.3526496899999998 4.0120754200000004         4.42680454 3.7192959800000001
    ## 75:               <NA>               <NA>               <NA> 10.475838660000001
    ##                   HE07               HE08               HE09               HE10
    ##                   HE11               HE12               HE13               HE14
    ##  1: 5.2674808500000001         6.00050974 6.0357627899999997 6.5472140300000001
    ##  2: 6.2465286300000002 6.1015033699999996 5.9878034600000003 5.9791245499999999
    ##  3: 9.4181985899999994 9.8272666900000001 9.7809791599999993         9.67129993
    ##  4: 9.3758878699999997 9.1345500899999994        10.32345009        10.17827797
    ##  5: 8.5415382399999995 8.6756477400000005 8.7611637099999999 9.0359706899999992
    ##  6: 2.4464180500000001         2.96481562 3.0367231399999999 3.3771374199999999
    ##  7: 4.8743853599999998 5.2453269999999996 5.6955947900000004 5.3879337300000003
    ##  8: 7.7883334199999998         7.73536205 7.9772124299999998 8.3962497700000007
    ##  9: 7.1416268299999999 7.5788183199999999 7.1707920999999999 7.7096777000000003
    ## 10: 5.2297310799999996 5.1779704100000004 6.2564902299999998 5.6249132199999998
    ## 11: 10.339070319999999 10.428593640000001        10.34045982 10.250130649999999
    ## 12: 6.7717885999999998 7.0209197999999997 7.4447832099999998 7.8072986599999998
    ## 13: 4.3255133600000004 4.5493988999999999 4.7100224500000003 4.7732276899999997
    ## 14: 6.7807683900000004 6.7459964799999996 7.0172405199999996 7.1855320899999997
    ## 15: 6.4492850300000004         6.55474567 6.9451847100000004 6.9609560999999998
    ## 16: 7.8652181600000004 8.4796819699999997 8.5598344799999992 8.6212749500000001
    ## 17: 5.7675781300000004         5.79034519 5.9779148099999997 6.0807762099999998
    ## 18: 4.4689779300000003 4.5395960799999999 4.0750651400000004 4.0336551700000003
    ## 19:         9.22303009 9.5857954000000003 9.8053464899999998 9.7811508200000006
    ## 20: 11.201831820000001        11.31324291 11.436079980000001        11.58073521
    ## 21: 8.3881034900000007         8.37314224 8.3957242999999995 8.4388189300000001
    ## 22:        10.77629662        10.84700775 10.991871829999999        11.01583958
    ## 23: 4.7444992099999999 4.1872339199999997 4.6230053900000003 4.1004481300000002
    ## 24:         10.2313633 9.6682958600000006         7.23788214 7.7976360299999996
    ## 25: 7.5254559499999996 7.4488325099999999 7.2491192800000004 7.0704755800000001
    ## 26: 3.2463419400000002 3.3293530900000001 3.7494416199999998 3.6195654899999998
    ## 27: 2.9554202599999999 2.9028665999999999 2.9606506800000001 3.1171209800000002
    ## 28: 6.6072506899999999 6.6364860500000002         5.99379873 6.9135108000000001
    ## 29: 2.7913181800000002 2.6903941599999999 2.8193976900000002 2.7965490800000001
    ## 30: 10.616717339999999        10.79065323        10.79159355        10.83204937
    ## 31: 8.2993192699999998 7.8829235999999998 7.2476954500000002         7.31149483
    ## 32: 2.6023666900000002 3.0372598200000001 2.6628675500000001 2.9746842400000002
    ## 33:         6.00844383         6.13262033 6.2478942899999996 6.4743809700000003
    ## 34: 2.6180841899999998         2.57378602 2.5854389699999998 3.1975801000000001
    ## 35:               <NA>               <NA>               <NA>               <NA>
    ## 36: 8.1471137999999996 7.9314336799999996 7.5797805800000004 7.8462958299999999
    ## 37: 6.0501275100000003               <NA>               <NA>               <NA>
    ## 38: 3.3399648700000002 3.4879069299999998 3.5224008599999999 3.7252702700000002
    ## 39: 4.0157337200000001 3.7521157299999999 3.9675071200000001 4.4815611799999999
    ## 40: 5.7011413600000003 5.8422455800000002 5.9407772999999997 5.6303863500000002
    ## 41: 9.0967035299999992 9.1396207799999996 8.6830034299999994 8.6327571899999995
    ## 42: 5.7329936000000004 5.7494225500000002 5.6874351499999998 5.3034172100000001
    ## 43: 10.233578680000001        10.53898716        10.58363628 10.567037579999999
    ## 44: 9.5075111400000001 9.6493740100000007 9.3631696699999996 9.4200058000000002
    ## 45: 3.3207793200000002 3.3598427800000001 3.4206934000000002 3.3484041699999998
    ## 46: 8.7886810299999993 8.7778787600000001 8.9295339600000005 9.3449335100000006
    ## 47: 2.3444263900000002 2.3593993200000001 2.6028020399999998         2.72200847
    ## 48:               <NA>               <NA>               <NA>               <NA>
    ## 49: 4.6209321000000001 4.7565517399999999 4.7019739200000004 4.9947280899999997
    ## 50: 4.2060928300000002 4.3746528600000003          4.4638114 4.1274123200000004
    ## 51: 6.2338209200000003 6.2010831800000004 6.3754796999999996 6.3525400200000002
    ## 52:         1.59996212         1.74822152         2.07620168 2.4345641100000002
    ## 53: 4.6990156199999999 4.7180356999999997 5.1926212300000003 5.0300502800000002
    ## 54: 4.7900524100000004 4.9408035300000002 5.0798091899999998 5.1802287099999997
    ## 55: 8.2176322899999992 8.3569784200000008 6.9706468600000004 7.1703767799999998
    ## 56:         9.11380005 9.3282098799999993 9.3123245200000007 9.2459878900000003
    ## 57: 3.1579217900000001 3.3278367499999999 3.6856265100000001 3.8715567599999998
    ## 58: 8.5363960300000006 8.7150993299999993 8.7402830100000006 8.5019750599999995
    ## 59: 7.4973173099999997 7.7525506000000002 7.7209615700000001 7.9301500300000001
    ## 60: 9.1685924500000002 9.1569919599999992 9.0673770900000008 9.0901050600000008
    ## 61: 10.421509739999999        10.73550034        10.90368462 10.948073389999999
    ## 62:        10.34077358        10.61796474        10.83054256 10.993874549999999
    ## 63: 3.5682563799999998 3.5229399199999998 3.4531335799999998 3.6844804299999998
    ## 64: 4.7219791400000002 4.8760519000000002 5.0591573700000003 5.2278909699999998
    ## 65: 6.4320287699999996 6.6064229000000001 6.8566203100000003         6.80739641
    ## 66: 4.6530509000000002 4.4444465600000003 4.3709998099999998         4.32517862
    ## 67: 6.8161716500000002 7.1166605900000004         6.94156218 7.1896038100000004
    ## 68: 9.9734649700000002 10.051768300000001 9.9785518599999996 9.9575843800000001
    ## 69:        16.29982948 16.285289760000001 16.210422520000002 16.406448359999999
    ## 70: 8.6352901499999994 8.7641210600000008 8.8199577300000005 8.7702360200000005
    ## 71: 5.2821259500000002 5.5897636400000001 5.6617379200000002         4.67292738
    ## 72:               <NA>               <NA>               <NA>               <NA>
    ## 73:               <NA>               <NA>               <NA>               <NA>
    ## 74: 3.4605324300000002 3.9305291200000001 4.6909103400000003 3.8292422300000002
    ## 75: 8.0817375200000008 6.9183530800000002         7.11014795 8.1335239399999999
    ##                   HE11               HE12               HE13               HE14
    ##                   HE15               HE16               HE17 scl05 scl06 scl07
    ##  1: 6.9784917799999997         6.60778189         6.38032866   6.9   7.0   7.2
    ##  2: 6.2324533500000001 6.3434934600000004 6.5443186799999999   9.8  10.1  10.1
    ##  3: 10.229337689999999 9.0018968600000004 10.457043649999999   9.1   9.0   8.9
    ##  4: 10.117628099999999 9.9519586600000007        10.36270523  10.9  10.9  11.0
    ##  5: 9.3233118099999999 9.2007131599999994 9.2054481500000005  11.7  11.9  12.0
    ##  6: 4.1066360499999996 4.0365543400000004 3.7365145700000002  10.7  10.7  10.2
    ##  7: 6.0653333700000003 5.9105415299999997         5.75974846   9.3  10.0  10.6
    ##  8: 8.8709096899999995 9.2074222599999995 9.4693374600000002   6.3   6.4   6.5
    ##  9: 7.4133834800000002 7.4799427999999999 7.4532680500000001  10.2  10.4  10.5
    ## 10: 5.8254685400000001         7.02782106 6.9119019499999999   1.3   1.3   1.3
    ## 11:        10.68328094        10.97903442 10.789648059999999  12.2  12.3  12.4
    ## 12: 8.3026914600000001 8.5186672199999993 9.0682048799999997   9.5   9.5   9.4
    ## 13: 4.8887229000000003 4.9818806599999998         5.15119171   6.9   6.9   7.0
    ## 14: 7.5233855199999997 7.5311703699999999 7.6786427499999999   6.8   6.7   7.2
    ## 15: 6.9256858799999996 6.8342390100000001 6.7382893599999996  10.7  10.9  11.2
    ## 16: 8.5884571100000002 8.2959604299999992 8.2574291199999994   7.3   7.3   7.3
    ## 17: 6.3462920199999999         6.42956781 6.5918378799999999  12.1  12.2  12.2
    ## 18:         3.82316828         3.66298747 3.5033159299999999   1.9   2.0   2.1
    ## 19: 9.6452388800000008 9.3779602099999995 9.1415662799999993  12.0  12.0  12.0
    ## 20:        11.46616745 11.501344680000001        11.39050198  10.4  10.6  10.7
    ## 21: 7.9323587399999997 8.4213466599999993 7.0580444299999998  12.1  12.1  12.1
    ## 22: 11.178019519999999        11.23043633 11.322296140000001  13.3  13.6  13.7
    ## 23: 4.6217145899999998 3.4615774199999998 3.3297784300000002   6.4   6.5   6.5
    ## 24: 8.6285295499999997 8.4368677099999996 8.0846834199999993   4.3   4.4   4.5
    ## 25: 6.8894891700000001         7.04818678 6.7922754300000001  10.9  11.1  11.3
    ## 26: 3.5956599699999998 3.5109832299999999         3.53500652   4.8   4.9   5.0
    ## 27:         2.99113512 3.0856990799999999 2.8682405900000001   7.4   7.9   7.1
    ## 28: 7.7605791100000001 8.8595066100000004 8.6596641499999993    NA    NA    NA
    ## 29: 3.1427907899999998 3.2278582999999998 4.1250171699999996   5.8   5.9   6.1
    ## 30:        10.88550663 10.834609990000001        10.79634285  11.2  11.2  11.3
    ## 31: 7.5931577700000004 7.2834053000000001 8.1192512499999996   9.7   9.8   9.8
    ## 32: 3.0405108900000002 3.4223556500000001 3.1258840600000002  11.7  11.7  11.6
    ## 33: 6.6527166400000004 6.9143271400000001         7.10694933    NA    NA    NA
    ## 34: 4.0147361799999999 4.0310459099999996 5.2952661499999998   5.8   6.2   6.3
    ## 35:               <NA>               <NA>               <NA>  10.2  10.2  10.3
    ## 36: 7.6793422700000002 7.9810357099999996 8.4476556800000004   7.5   7.6   7.8
    ## 37:               <NA>               <NA>               <NA>   6.4   6.6   6.8
    ## 38: 3.8160853399999999         3.68835807         3.71246052   7.6   8.2   8.8
    ## 39: 4.1114335100000003 3.7753798999999999 4.1038827900000001   1.7   1.7   1.8
    ## 40: 5.7970871900000001         5.61569786 5.5188951499999996   7.6   8.0   8.0
    ## 41: 8.5576353100000002 7.5355224600000001 7.0132851599999997    NA    NA    NA
    ## 42: 5.0717449200000004 5.2381510699999998 5.2362384799999999   3.9   4.0   4.0
    ## 43:        10.32396889 10.253452299999999 10.059798239999999  11.6  11.8  12.0
    ## 44: 9.3146696099999993 9.2739820500000008 9.0196762100000001  11.7  11.7  11.8
    ## 45: 3.5819501900000001 3.6477367900000002 3.7555384599999999   5.2   5.2   5.2
    ## 46:        10.13146877 10.591295240000001        10.31589031  12.4  12.5  12.6
    ## 47: 2.6871192499999998 2.8551020600000001 2.9022936800000001   4.5   4.4   4.4
    ## 48:               <NA>               <NA>               <NA>    NA    NA    NA
    ## 49:         5.03110838 5.0657653800000002 4.9937844299999998   8.7   8.1   8.1
    ## 50: 4.3213686899999999 4.4068460500000004 4.4460597000000002   8.7   8.8   8.9
    ## 51: 6.3978581400000003         6.54295063 6.5617284800000002  11.6  11.7  11.8
    ## 52:         3.17197657 3.1880204700000001 2.7216007699999998   8.8   9.2   9.6
    ## 53: 4.9426484100000003 4.9937720299999997 5.1508317000000003  10.1  10.3  10.5
    ## 54: 5.2956042300000004 5.2652196900000003         5.34388065    NA    NA    NA
    ## 55: 6.8623290099999998 7.1433134100000002 6.9301710099999996   2.8   2.9   3.1
    ## 56: 8.8192958800000003 8.4718694699999997 8.2313175199999993  10.2  10.2  10.2
    ## 57: 4.1806983899999999         4.40151834 4.4177866000000003  10.5  10.1  10.2
    ## 58: 8.4982395200000003 8.4790992700000007 8.1894178400000008  11.7  11.8  11.8
    ## 59: 8.2009029400000006 8.0958175699999995 8.1131191299999994   8.9   9.0   9.1
    ## 60: 9.1268682499999993 8.9523858999999995 8.9424924899999993   8.9   9.0   9.1
    ## 61:        10.79714203        10.84046745        10.78592205  12.4  12.4  12.5
    ## 62:        11.36931515 11.708949090000001        11.89515591  12.0  12.3  12.6
    ## 63: 3.6678931700000001 3.7633202099999998 3.8320283900000001   7.0   7.0   7.1
    ## 64: 6.0395116800000004 7.0093059499999999 7.0202856100000002  10.0   9.9  10.5
    ## 65: 7.0074124299999996 6.9971704499999996 7.2576856599999999   5.8   5.9   6.1
    ## 66: 4.1169037800000003 4.2846922899999997 4.1797499699999996   6.0   6.1   6.2
    ## 67: 7.7760777499999998 7.5454115899999996 7.4333243400000004  11.2  11.2  11.2
    ## 68: 9.9043521900000009 9.8667116200000002 9.8251123400000004  12.2  12.4  12.6
    ## 69: 16.710752490000001        17.04898262 17.003614429999999  12.8  12.8  12.9
    ## 70: 9.0082111400000002         9.42438793 9.5163564699999998   8.0   8.0   8.2
    ## 71: 4.9875989000000001 4.9663839300000001 5.0767974899999997   9.8   9.9  10.1
    ## 72:               <NA>               <NA>               <NA>    NA    NA    NA
    ## 73:               <NA>               <NA>               <NA>   1.9   2.0   2.2
    ## 74: 4.4351024600000004 4.4772071799999997 4.3974895500000004   6.3   6.4   6.4
    ## 75: 7.4520664200000004 7.6476202000000004 5.8497748400000003   6.8   6.8   7.0
    ##                   HE15               HE16               HE17 scl05 scl06 scl07
    ##     scl08 scl09 scl10 scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05
    ##  1:   6.7   6.9   7.1   7.4   7.6   7.8   7.9   7.9   8.0   8.0  3113.0953
    ##  2:  10.1  10.1  10.1  10.2  10.2  10.2  10.2  10.2  10.2  10.2 40066.2569
    ##  3:   8.9   9.3   9.8   9.8   9.8   9.8   9.8   9.8   9.9   9.9  5109.8513
    ##  4:  11.0  11.1  11.1  11.2  11.3  11.4  11.5  11.6  11.7  11.7  1643.7530
    ##  5:  12.3  12.3  12.4  12.5  12.6  12.6  12.7  12.8  12.9  12.9 33999.2429
    ##  6:  10.2  10.7  10.7  10.7  10.7  10.8  10.7  10.7  10.7  10.7  1578.4024
    ##  7:  11.3  11.9  12.0  12.0  12.0  12.0  12.1  12.2  12.3  12.3  3125.8105
    ##  8:   6.7   6.8   6.9   7.1   7.3   7.4   7.4   7.6   7.8   7.8  4790.4371
    ##  9:  10.5  10.6  10.6  10.7  10.8  10.9  10.9  11.8  11.8  11.8  3899.9076
    ## 10:   1.3   1.4   1.4   1.4   1.4   1.4   1.4   1.4   1.5   1.5   457.9336
    ## 11:  12.4  12.5  12.6  12.7  12.8  12.9  13.0  13.1  13.1  13.3 36266.1871
    ## 12:   9.9   9.9   9.8   9.8   9.9   9.9  10.1  10.3  10.3  10.3  7598.5251
    ## 13:   7.0   7.1   7.3   7.4   7.5   7.5   7.6   7.7   7.8   7.8  1753.4178
    ## 14:   7.3   7.3   7.4   7.5   7.6   7.8   8.0   8.1   8.3   8.3  3414.4652
    ## 15:  11.3  11.3  11.5  11.6  11.8  12.0  11.9  11.9  12.1  12.1 24959.2592
    ## 16:   7.9   7.9   7.9   8.0   8.1   8.3   8.5   8.4   8.7   8.7  3002.1369
    ## 17:  12.3  12.4  12.5  12.5  12.5  12.6  12.6  12.7  12.6  12.7 10406.3967
    ## 18:   2.2   2.3   2.3   2.4   2.4   2.5   2.5   2.6   2.7   2.7   162.4327
    ## 19:  12.2  12.2  12.3  12.3  12.4  12.3  12.4  12.4  12.4  12.4 39040.2889
    ## 20:  10.7  10.8  10.9  10.9  11.0  11.2  11.4  11.5  11.5  11.5 34760.1878
    ## 21:  12.2  12.2  12.2  12.2  12.5  12.6  12.6  12.7  12.8  12.8  1642.7609
    ## 22:  13.7  13.8  13.8  13.9  14.0  14.0  14.0  14.1  14.1  14.1 34507.3688
    ## 23:   6.6   6.7   6.7   6.8   6.8   6.9   6.9   6.9   7.1   7.1   492.5442
    ## 24:   4.6   4.7   4.7   4.8   4.9   5.0   5.1   5.2   5.2   5.3   766.6921
    ## 25:  11.5  11.7  11.9  12.0  12.2  12.0  11.8  11.8  11.9  11.9 11200.5769
    ## 26:   5.2   5.3   5.4   5.4   5.6   5.8   6.1   6.3   6.4   6.4   714.8610
    ## 27:   7.1   7.4   7.4   7.6   7.6   7.8   7.8   7.9   8.0   8.0  1263.2873
    ## 28:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 29:   6.2   6.3   6.4   6.6   6.6   6.6   6.6   6.6   6.7   6.8  1855.5220
    ## 30:  11.4  11.4  11.5  11.8  12.0  12.2  12.5  12.5  12.7  12.8 37217.6487
    ## 31:   9.8   9.9   9.8   9.9   9.9   9.9  10.1  10.3  10.4  10.4  2183.3962
    ## 32:  11.5  11.5  11.4  11.5  11.5  11.6  11.7  11.7  11.7  11.8  3771.2790
    ## 33:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 34:   6.5   6.6   6.8   7.0   7.2   6.7   6.9   7.1   7.2   7.3 35591.0058
    ## 35:  10.3  10.4  10.6  10.6  10.7  10.7  10.8  10.8  10.9  10.9         NA
    ## 36:   7.8   7.9   7.9   8.0   8.1   8.3   8.4   8.5   8.6   8.7  4575.1055
    ## 37:   7.0   7.1   7.3   7.3   7.3   7.3   7.3   7.3   7.3   7.3  8163.0108
    ## 38:   9.4   9.6   9.8  10.1  10.1  10.1  10.1  10.2  10.2  10.2  5587.0256
    ## 39:   1.9   1.9   2.0   2.0   2.1   2.2   2.3   2.3   2.3   2.3   489.0211
    ## 40:   8.0   8.2   8.0   8.4   8.6   8.4   8.4   8.6   8.6   8.6  8277.6713
    ## 41:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 42:   4.1   4.2   4.2   4.4   4.6   4.8   5.0   5.0   5.4   5.5  2018.0257
    ## 43:  12.0  11.9  12.0  12.0  12.0  12.1  12.1  12.1  12.1  12.2 41979.0558
    ## 44:  11.8  11.9  12.0  12.0  12.1  12.1  12.2  12.4  12.5  12.5 27751.0655
    ## 45:   5.2   5.2   5.2   5.5   5.7   5.9   5.9   6.0   6.2   6.2  1268.3834
    ## 46:  12.7  12.7  12.7  12.8  12.6  12.7  12.5  12.5  12.6  12.6 66810.4785
    ## 47:   4.4   4.5   4.7   4.8   4.8   4.9   5.1   5.1   5.2   5.2   748.9226
    ## 48:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 49:   8.4   8.4   8.4   9.1   8.6   8.8   9.4   9.1   9.2   9.2  2729.4987
    ## 50:   9.0   9.0   8.9   9.0   9.1   9.1   9.2   9.3   9.3   9.3  1244.3490
    ## 51:  11.9  12.1  12.2  12.3  11.7  12.1  11.3  12.1  12.2  12.3  8021.5057
    ## 52:   9.7   9.2   8.4   8.7   9.2   9.9   9.8   9.8   9.8   9.8 51455.5942
    ## 53:  10.6  10.6  10.7  10.8  10.9  10.9  10.9  10.9  11.0  11.0  4617.9290
    ## 54:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA  5323.4631
    ## 55:   3.2   3.3   3.8   3.8   3.7   3.8   4.0   4.0   4.1   4.1   331.8112
    ## 56:  10.4  10.4  10.4  10.6  10.5  10.4  10.7  11.0  11.1  11.1  3720.4792
    ## 57:  10.5  10.5  11.2  11.2  11.3  11.4  11.4  11.5  11.5  11.5 29961.2633
    ## 58:  11.9  12.1  12.1  12.2  11.7  11.7  11.9  12.0  12.3  12.2 18098.9085
    ## 59:   9.7  10.1  10.0   9.9   9.8   9.9  10.0  10.1  10.1  10.1  5383.6565
    ## 60:   9.2   9.3   9.4   9.5   9.5   9.5   9.7   9.7   9.8   9.8 26419.2969
    ## 61:  12.2  12.2  12.3  12.4  12.4  12.2  12.3  12.4  12.4  12.4 43437.0631
    ## 62:  12.9  13.3  13.3  13.3  13.4  13.4  13.4  13.4  13.4  13.4 54952.6738
    ## 63:   7.3   7.5   7.7   7.5   7.7   7.5   7.6   7.6   7.6   7.6  2894.0627
    ## 64:  10.7  10.7  10.7  10.8  10.8  10.8  10.8  10.8  10.9  10.9 12327.2332
    ## 65:   6.3   6.4   6.7   6.9   6.8   6.8   6.9   7.0   7.1   7.2  3193.2066
    ## 66:   6.3   6.5   6.7   7.2   7.5   7.7   7.6   7.8   8.0   8.0  7456.3877
    ## 67:  11.3  11.3  11.3  11.3  11.3  11.3  11.3  11.3  11.3  11.3  1826.9314
    ## 68:  12.8  13.1  13.2  13.0  12.9  12.6  12.7  12.8  12.9  12.9 42030.2866
    ## 69:  13.2  13.2  13.2  13.3  13.3  13.2  13.3  13.3  13.4  13.4 44114.7478
    ## 70:   8.4   8.4   8.4   8.4   8.5   8.5   8.6   8.7   8.7   8.7  5226.9378
    ## 71:  10.3  10.5  10.7  10.9  11.1  11.3  11.3  11.4  11.4  11.5   546.7769
    ## 72:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 73:   2.3   2.5   2.6   2.8   3.0   3.0   3.0   3.0   3.0   3.0         NA
    ## 74:   6.5   6.5   6.6   6.7   6.7   6.8   6.9   6.9   7.0   7.0   702.7409
    ## 75:   7.0   7.2   7.3   7.3   7.9   8.0   8.2   8.2   8.2   8.2   476.5553
    ##     scl08 scl09 scl10 scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05
    ##          GDP06      GDP07      GDP08      GDP09      GDP10       GDP11
    ##  1:  3478.7109  3946.6645  4923.8432  3883.1324  4479.3417   5462.2609
    ##  2: 42675.8128 47803.6936 48718.4969 43503.1855 40852.6668  43335.3289
    ##  3:  5919.0120  7245.4483  9020.8731  8225.1372 10385.9644  12848.8642
    ##  4:  2158.1437  3139.2775  4010.8572  2994.3425  3218.3727   3525.8047
    ##  5: 36044.9228 40960.0545 49601.6567 42772.3592 52022.1256  62517.8337
    ##  6:  2473.0818  3851.4379  5574.6038  4950.2948  5842.8058   7189.6912
    ##  7:  3847.4341  4735.6576  6377.3697  5351.3554  6029.3968   6519.2302
    ##  8:  5886.4636  7348.0308  8831.0231  8597.9154 11286.2430  13245.6125
    ##  9:  4523.0508  5885.1043  7265.7355  6988.2333  6812.4063   7809.4251
    ## 10:   473.4498   535.0626   643.4046   624.1752   647.8358    751.1730
    ## 11: 40385.8700 44543.0410 46594.4510 40773.0615 47448.0132  52087.4464
    ## 12:  9464.5502 10502.3545 10751.4797 10208.9068 12808.0346  14637.2402
    ## 13:  2099.2294  2693.9701  3468.3046  3832.2364  4550.4531   5618.1323
    ## 14:  3741.0928  4714.0731  5472.5365  5193.2415  6336.7095   7335.1669
    ## 15: 26729.3234 31244.9262 35397.3637 32109.2425 31023.6383  32396.3857
    ## 16:  3328.8830  3567.8364  4249.0193  4231.6158  4633.5904   5200.5558
    ## 17: 12631.5681 16741.9392 18227.1195 14794.9711 14790.8209  17621.5480
    ## 18:   194.6874   244.2860   326.4368   380.5690   341.5541    354.4796
    ## 19: 41188.0937 48414.8451 53554.0389 47293.9928 46459.9733  51081.9977
    ## 20: 36443.6234 41508.4340 45334.1144 41575.4187 40638.3340  43790.7320
    ## 21:  1996.0571  2635.3539  3324.7359  2822.6674  3233.2959   4021.7433
    ## 22: 36323.4477 41587.2129 45427.1517 41485.9016 41531.9342  46644.7760
    ## 23:   913.3939  1081.1663  1217.0648  1077.6622  1299.3449   1549.4629
    ## 24:   792.8259   981.1113  1076.7013  1150.2111  1172.0985   1287.9541
    ## 25: 11475.8227 13918.9602 15753.4733 13046.4810 13191.6213  14216.1656
    ## 26:   806.7533  1028.3348   998.5223  1101.9608  1357.5637   1458.1035
    ## 27:  1589.8015  1860.0028  2166.8542  2261.2472  3122.3628   3643.0439
    ## 28:         NA         NA         NA         NA         NA          NA
    ## 29:  2373.2148  3182.9480  4636.6110  3853.9409  4655.4250   6036.3962
    ## 30: 35433.9890 35275.2284 39339.2976 40855.1756 44507.6764  48167.9973
    ## 31:  2513.0317  2735.3831  3455.7673  3559.6911  3736.6465   3852.7528
    ## 32:  5291.5757  6771.4148  8513.5646  7165.2232  9070.4883  11634.0019
    ## 33:         NA         NA         NA         NA         NA          NA
    ## 34: 42781.3665 45782.2766 55494.9510 37561.6727 38577.4983  48631.6913
    ## 35:         NA         NA         NA         NA         NA          NA
    ## 36:  4626.8598  5207.7960  6111.3324  7354.9536  7761.6462   7674.8354
    ## 37:  9336.3567 11300.1913 14382.5763 10275.2666 12064.7807   5554.1792
    ## 38:  6209.1245  7243.4560  8474.5868  7292.4944  9040.5663  10399.3728
    ## 39:   523.0386   596.6902   694.2777   698.8989   710.2742    837.6034
    ## 40:  9068.2944  9642.6806 10016.5713  8002.9721  9271.3982  10203.4209
    ## 41:         NA         NA         NA         NA         NA          NA
    ## 42:  2196.0122  2499.2599  2890.3601  2866.9242  2839.9252   3046.9491
    ## 43: 44863.3506 51733.4421 57644.4800 52514.0271 50950.0343  54159.3466
    ## 44: 26671.3294 32511.1267 31290.2537 28205.7328 33700.1260  38437.5432
    ## 45:  1656.4248  1883.4613  2242.8719  1891.3354  2280.4374   2487.5982
    ## 46: 74148.3201 85139.9604 96944.0956 79977.6971 87693.7901 100600.5624
    ## 47:   836.8605   908.0951   990.8466   957.9957   987.4097   1164.9761
    ## 48:         NA         NA         NA         NA         NA          NA
    ## 49:  3154.3312  3606.0704  4220.6170  4196.3128  5082.3548   5869.3231
    ## 50:  1452.4387  1744.6403  1991.2315  1905.8947  2217.4740   2450.7337
    ## 51:  9035.4105 11254.5174 13996.0252 11526.0559 12613.0110  13879.5610
    ## 52: 59530.1535 65421.7528 80234.4701 59094.4449 67403.1603  82409.5773
    ## 53:  5757.4964  8360.1663 10435.0440  8548.1187  8214.0769   9099.2175
    ## 54:  6920.1891  9101.2550 11635.2729  8562.8133 10674.9958  14311.0843
    ## 55:   367.0325   438.8336   543.7651   579.9390   610.0124    668.8690
    ## 56:  4382.6173  5848.4764  7101.0401  6169.1142  5735.4229   6809.1598
    ## 57: 33769.1542 39432.9383 40007.4693 38927.2069 47236.9602  53890.4287
    ## 58: 19672.9656 23787.6466 27483.3372 24694.2305 23509.5431  25095.1323
    ## 59:  5602.0110  6095.6224  5760.8053  5862.7973  7328.6156   8007.4128
    ## 60: 28365.3135 32549.9710 35366.2596 32042.4741 30502.7197  31636.4463
    ## 61: 46593.6022 53700.0053 56152.5523 46946.9603 52869.0443  60755.7596
    ## 62: 57579.5020 63555.2375 72487.8459 69927.4688 74605.7211  88415.6280
    ## 63:  3369.5434  3973.0170  4379.6585  4213.0063  5076.3402   5492.1213
    ## 64: 14102.4958 16539.8781 21204.1050 14514.1417 16683.3554  19034.1492
    ## 65:  3370.0339  3775.7500  4307.1559  4128.4628  4141.9764   4264.6749
    ## 66:  8102.1215  9791.6516 10940.9912  9103.7099 10742.4301  11420.7733
    ## 67:  2300.7697  3065.6113  3887.2423  2542.9954  2965.1397   3569.7581
    ## 68: 44599.6976 50566.8266 47286.9985 38713.1374 39435.8399  42038.5723
    ## 69: 46298.7314 47975.9677 48382.5584 47099.9805 48467.5158  49886.8181
    ## 70:  5887.8487  7026.5115  9091.0790  9451.9324 11992.0166  14236.6812
    ## 71:   654.2838   830.4077  1082.2860  1213.2653  1634.3121   1926.2930
    ## 72:         NA         NA         NA         NA         NA          NA
    ## 73:         NA         NA         NA         NA         NA          NA
    ## 74:  1047.9192  1124.2906  1394.0006  1159.9078  1489.4593   1672.9083
    ## 75:   447.8549   431.7872   356.6933   771.5988   948.3319   1093.6540
    ##          GDP06      GDP07      GDP08      GDP09      GDP10       GDP11
    ##           GDP12       GDP13      GDP14      GDP15      GDP16      GDP17 MMR05
    ##  1:   5591.2124   5498.7841  5494.3523  4187.5097  3945.4821  4111.2941   127
    ##  2:  38686.4613  39538.7667 41303.9294 35762.5231 37474.6654 38962.8804    NA
    ##  3:  13082.6643  13080.2547 12334.7982 13789.0604 12790.2425 14613.0418    59
    ##  4:   3681.8575   3838.1858  3986.2316  3607.2967  3591.8293  3914.5013    35
    ##  5:  68012.1479  68150.1070 62510.7912 56755.7217 49971.1315 54027.9668     5
    ##  6:   7496.2946   7875.7570  7891.3131  5500.3104  3880.7387  4147.0897    42
    ##  7:   6940.1593   7978.8726  8318.5127  5949.1063  5022.6266  5761.7471    11
    ##  8:  12370.0242  12300.3249 12112.5882  8814.0010  8710.0967  9925.3862    71
    ##  9:   7395.8498   7655.1297  7876.8665  7055.9357  7548.8550  8334.0817    15
    ## 10:    758.0007    787.4702   792.8468   653.3270   688.2497   734.9944   437
    ## 11:  52678.3901  52652.5937 50893.4467 43585.5120 42322.4848 45148.5527    11
    ## 12:  15351.5513  15842.9408 14670.9968 13574.1718 13753.5944 14999.3701    25
    ## 13:   6316.9183   7050.6463  7678.5995  8066.9426  8147.9377  8879.4387    44
    ## 14:   8050.2554   8218.3478  8114.3439  6175.8760  5870.7780  6376.7067    83
    ## 15:  28912.1569  27729.1927 27129.6261 23333.7149 24532.5191 26338.6943    12
    ## 16:   5682.0450   6056.3308  6377.0915  6124.4916  6060.0933  6213.5013    94
    ## 17:  17534.4213  19174.1003 20367.1032 17522.2302 18437.2528 20458.4607    18
    ## 18:    467.0779    499.5316   566.9265   640.5419   717.1246   768.5223   865
    ## 19:  47710.7902  49878.0432 50260.2999 42784.6984 43784.2840 46336.6633     5
    ## 20:  40874.7035  42592.9341 43011.2631 36638.1849 37037.3742 38812.1610     9
    ## 21:   4421.8182   4623.7457  4739.1883  4014.1859  4062.1699  4357.0009    39
    ## 22:  43858.3631  46285.7641 47959.9933 41086.7297 42107.5173 44552.8194     6
    ## 23:   1587.5612   2345.3929  1971.0333  1743.8510  1931.3895  2025.9324   371
    ## 24:   1337.3354   1393.9560  1402.1002  1389.1195  1265.9876  1294.2397   459
    ## 25:  12950.6865  13687.5141 14267.0122 12706.8912 13090.5067 14605.8543    15
    ## 26:   1443.8795   1449.6059  1573.8815  1605.6054  1732.5643  1981.6510   286
    ## 27:   3694.3489   3623.9116  3491.6248  3331.6951  3562.8458  3837.6517   252
    ## 28:          NA          NA         NA         NA         NA         NA    34
    ## 29:   6829.9640   7076.8772  6818.8046  4989.8031  4777.1976  5205.2883   127
    ## 30:  48603.4766  40454.4475 38109.4121 34524.4699 38761.8182 38386.5111     7
    ## 31:   3909.9076   4043.7490  4130.8791  4164.1079  4176.5889  4234.4031    62
    ## 32:  12386.7000  13890.6318 12807.2607 10510.7719  7714.8418  9247.5813    43
    ## 33:          NA          NA         NA         NA         NA         NA    15
    ## 34:  51979.1052  49388.1374 44062.3170 29869.5294 27653.0668 29759.4365    10
    ## 35:          NA          NA         NA         NA         NA         NA    NA
    ## 36:   7950.6954   7931.0805  7686.2560  7644.5487  7629.8911  7801.1787    24
    ## 37:  13025.2814  10363.7895  6466.9103  4337.9191  4035.1943  5756.6984    57
    ## 38:  10817.4429  10970.1233 11319.0798  9955.2437  9817.7385 10259.1818    31
    ## 39:    778.6193    805.0328   848.2741   751.4748   780.7186   830.0184   691
    ## 40:  10241.7279  10725.1833 10928.9168  9616.6450  8744.5158  9287.8497    54
    ## 41:          NA          NA         NA         NA         NA         NA    34
    ## 42:   2912.6583   3121.6812  3171.6992  2875.2580  2896.7200  3036.3253   131
    ## 43:  50073.0057  52184.0619 52830.1742 45175.2319 46007.8529 48675.2223    11
    ## 44:  39982.7543  42962.9882 44553.2822 38615.9952 40105.6134 42849.4263    11
    ## 45:   2723.8228   2961.5503  3098.9863  2687.4801  2176.0022  1968.5647  1080
    ## 46: 101524.1419 102913.4508 97019.1828 74355.5159 70459.1825 75496.7541     5
    ## 47:   1198.1090   1208.9043  1251.1641  1356.6678  1368.4543  1464.9933   237
    ## 48:          NA          NA         NA         NA         NA         NA    NA
    ## 49:   6528.9722   6756.7528  6672.8803  6229.1017  6204.9973  6710.5080   118
    ## 50:   2694.3055   2871.4309  2959.6485  3001.0404  3073.6536  3123.2342   156
    ## 51:  13097.2708  13696.4663 14271.3059 12578.4955 12447.4396 13864.6818     4
    ## 52:  85076.1415  85050.8663 83858.4769 63039.0635 57163.0757 59124.9324    12
    ## 53:   8507.1048   9547.8522 10043.6774  8969.1489  9548.5874 10807.7954    35
    ## 54:  15420.8745  15974.6446 14095.6487  9313.0136  8704.8984 10720.3326    42
    ## 55:    725.6277    723.2583   743.9948   751.6394   745.3428   772.3185   643
    ## 56:   6015.9452   6755.0737  6600.0568  5588.9807  5765.2008  6292.5436    12
    ## 57:  55546.4885  56967.4258 57562.5308 55646.6187 56828.2953 60913.7453    13
    ## 58:  22643.1003  23496.6025 24214.9221 20881.7669 21663.6434 23512.8173    10
    ## 59:   7501.4700   6832.4569  6433.1873  5734.6336  5272.9184  6132.4798   201
    ## 60:  28324.4293  29059.5480 29461.5503 25732.0184 26505.3432 28170.1679     5
    ## 61:  58037.8213  61126.9432 60020.3605 51545.4836 51965.1572 53791.5087     5
    ## 62:  83538.2300  85112.4644 86605.5634 82081.5972 80172.2321 80449.9945     7
    ## 63:   5860.5825   6168.2630  5951.8837  5840.0465  5994.2315  6592.9149    43
    ## 64:  19157.4170  20143.6644 20270.8594 18289.7043 16176.9474 16238.1932    76
    ## 65:   4152.6786   4222.7032  4305.4742  3861.6885  3697.9308  3481.2287    51
    ## 66:  11795.3167  12614.4803 12157.3380 11006.2497 10895.3187 10591.4744    33
    ## 67:   3855.4177   4029.7113  3104.6432  2124.6623  2187.7305  2640.6757    33
    ## 68:  42462.7716  43444.5330 47425.6077 44974.8319 41064.1334 40361.4174    11
    ## 69:  51610.6053  53117.6678 55064.7445 56839.3818 57951.5841 60062.2223    13
    ## 70:  15171.5847  16973.6742 16831.9729 15613.7643 15387.1440 17322.1474    22
    ## 71:   2137.0251   2281.4110  2492.3366  2615.0251  2567.7992  1826.5669    38
    ## 72:          NA          NA         NA         NA         NA         NA    NA
    ## 73:          NA          NA         NA         NA         NA         NA    NA
    ## 74:   1763.0727   1878.9097  1763.0626  1337.7956  1280.5784  1534.8668   421
    ## 75:   1304.9698   1430.0008  1434.8993  1445.0711  1464.5835  1548.1701   685
    ##           GDP12       GDP13      GDP14      GDP15      GDP16      GDP17 MMR05
    ##     MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ##  1:   122   119   117   117   115   116   116   115   114   114   113   112
    ##  2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  3:    57    56    53    56    51    48    47    44    42    41    40    39
    ##  4:    36    32    36    32    32    30    30    26    27    28    26    26
    ##  5:     5     5     5     5     5     6     6     6     6     6     6     6
    ##  6:    35    34    32    32    31    30    29    28    28    27    26    26
    ##  7:     9     7     6     6     5     5     4     3     3     3     3     2
    ##  8:    72    71    70    69    65    61    60    61    62    63    62    60
    ##  9:    14    13    13    12    12    12    11    10    11    10    10    10
    ## 10:   422   410   401   393   385   377   369   362   353   343   331   320
    ## 11:    11    11    12    12    11    11    11    11    11    11    10    10
    ## 12:    25    23    21    21    20    18    17    16    15    14    13    13
    ## 13:    42    40    40    37    36    34    33    32    31    30    29    29
    ## 14:    82    83    84    87    85    84    85    85    85    85    84    83
    ## 15:     9     8     9     8     8     8     7     6     8     7     6     6
    ## 16:    90    85    82    80    78    76    71    67    65    63    61    59
    ## 17:    16    17    14    14    11    13    12    10    11    10    11     9
    ## 18:   795   731   681   638   597   558   527   498   472   446   422   401
    ## 19:     4     4     4     4     4     4     4     4     3     3     3     3
    ## 20:     9     9     9     9     9     9     9     9     8     8     8     8
    ## 21:    33    36    39    43    32    32    30    30    29    27    26    25
    ## 22:     6     6     6     6     6     6     5     5     5     5     5     7
    ## 23:   359   349   342   339   339   339   336   331   325   320   314   308
    ## 24:   467   473   484   484   506   496   500   496   492   488   489   480
    ## 25:    14    15    14    13    13    13    12    12    12    12    12    12
    ## 26:   270   255   240   225   210   197   185   175   166   158   150   145
    ## 27:   249   243   239   234   228   221   214   207   199   192   184   177
    ## 28:    32    30    28    25    22    19    18    17    17    17    16    16
    ## 29:   158   138    90    75    70    67    66    75    92    83    78    79
    ## 30:     7     6     6     6     6     6     5     5     5     5     5     5
    ## 31:    58    56    55    54    53    52    52    51    49    48    47    46
    ## 32:    40    36    30    24    22    19    17    14    13    12    10    10
    ## 33:    14    15    15    16    15    14    13    13    12    12    11    11
    ## 34:    10    10    10    10    10    10    11    11    11    11    12    12
    ## 35:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 36:    24    23    23    23    23    25    25    27    28    29    29    29
    ## 37:    55    53    52    53    53    56    55    58    63    70    73    72
    ## 38:    30    30    29    29    30    30    30    30    30    30    29    29
    ## 39:   675   663   661   661   660   663   663   663   642   620   590   562
    ## 40:    51    49    49    51    46    43    41    39    38    36    34    33
    ## 41:    31    31    29    28    29    21    22    21    23    22    20    19
    ## 42:   121   113   106    99    92    86    81    78    76    74    72    70
    ## 43:     9     9     8     8     7     7     7     6     6     6     6     5
    ## 44:    11    11    10    11    11    10    10     9    10    10    10     9
    ## 45:  1040  1010   996   987   978   972   963   951   943   931   925   917
    ## 46:     4     4     4     4     4     4     3     3     3     3     3     2
    ## 47:   222   214   205   199   191   180   173   166   161   154   143   140
    ## 48:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 49:   114   112   108   106   104   102   100    98    96    94    91    88
    ## 50:   156   149   148   149   144   141   139   136   131   127   124   121
    ## 51:     4     4     4     3     3     3     3     3     2     2     2     2
    ## 52:    11    11    11    10    10    10    10    10    10     9     9     9
    ## 53:    32    30    28    28    27    24    23    21    21    21    21    19
    ## 54:    36    32    30    27    25    23    22    20    19    18    18    17
    ## 55:   541   469   427   424   373   349   329   308   291   275   260   248
    ## 56:    12    12    12    12    12    12    12    12    12    13    12    12
    ## 57:    13    12    11    10    10    10    10     9     8     9     8     8
    ## 58:     8     8     8     8     8     8     7     7     7     7     7     7
    ## 59:   201   199   191   179   171   161   143   133   128   125   122   119
    ## 60:     4     4     4     4     4     4     4     4     4     4     4     4
    ## 61:     5     5     5     5     4     5     5     5     5     4     4     4
    ## 62:     7     6     6     6     6     6     6     6     5     5     5     5
    ## 63:    42    42    43    43    42    41    39    39    38    38    37    37
    ## 64:    73    65    72    74    71    72    71    70    69    68    68    67
    ## 65:    49    47    47    46    46    46    46    46    46    46    45    43
    ## 66:    30    28    27    25    24    23    22    20    19    19    18    17
    ## 67:    31    33    33    27    25    23    24    23    24    21    20    19
    ## 68:    11    11    11    10    10     9     8     8     8     8     7     7
    ## 69:    14    14    14    15    15    15    16    16    16    18    19    19
    ## 70:    20    20    19    19    17    17    17    18    17    18    18    17
    ## 71:    37    35    34    32    31    32    32    31    30    30    29    29
    ## 72:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 73:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 74:   406   387   356   329   305   283   267   254   242   232   222   213
    ## 75:   680   671   657   632   598   557   528   509   494   480   468   458
    ##     MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ##                   LMH09               LMH14
    ##  1: upper middle income upper middle income
    ##  2:         high income         high income
    ##  3: upper middle income         high income
    ##  4: lower middle income lower middle income
    ##  5:         high income         high income
    ##  6: upper middle income upper middle income
    ##  7: upper middle income upper middle income
    ##  8: upper middle income upper middle income
    ##  9: upper middle income upper middle income
    ## 10:          low income          low income
    ## 11:         high income         high income
    ## 12: upper middle income         high income
    ## 13: lower middle income upper middle income
    ## 14: upper middle income upper middle income
    ## 15:         high income         high income
    ## 16: lower middle income upper middle income
    ## 17:         high income         high income
    ## 18:          low income          low income
    ## 19:         high income         high income
    ## 20:         high income         high income
    ## 21: lower middle income lower middle income
    ## 22:         high income         high income
    ## 23:          low income lower middle income
    ## 24:          low income          low income
    ## 25:         high income         high income
    ## 26: lower middle income lower middle income
    ## 27: lower middle income lower middle income
    ## 28: upper middle income upper middle income
    ## 29: lower middle income upper middle income
    ## 30:         high income         high income
    ## 31: lower middle income upper middle income
    ## 32: upper middle income upper middle income
    ## 33:         high income         high income
    ## 34:         high income         high income
    ## 35:                <NA>                <NA>
    ## 36: upper middle income upper middle income
    ## 37: upper middle income upper middle income
    ## 38: upper middle income upper middle income
    ## 39:          low income          low income
    ## 40: upper middle income upper middle income
    ## 41: lower middle income lower middle income
    ## 42: lower middle income lower middle income
    ## 43:         high income         high income
    ## 44:         high income         high income
    ## 45: lower middle income lower middle income
    ## 46:         high income         high income
    ## 47: lower middle income lower middle income
    ## 48:                <NA>                <NA>
    ## 49: upper middle income upper middle income
    ## 50: lower middle income lower middle income
    ## 51:         high income         high income
    ## 52:         high income         high income
    ## 53: upper middle income upper middle income
    ## 54: upper middle income         high income
    ## 55:          low income          low income
    ## 56: upper middle income upper middle income
    ## 57:         high income         high income
    ## 58:         high income         high income
    ## 59: upper middle income upper middle income
    ## 60:         high income         high income
    ## 61:         high income         high income
    ## 62:         high income         high income
    ## 63: lower middle income upper middle income
    ## 64:         high income         high income
    ## 65: lower middle income upper middle income
    ## 66: upper middle income upper middle income
    ## 67: lower middle income lower middle income
    ## 68:         high income         high income
    ## 69:         high income         high income
    ## 70: upper middle income         high income
    ## 71: lower middle income lower middle income
    ## 72:                <NA>                <NA>
    ## 73:                <NA>                <NA>
    ## 74:          low income lower middle income
    ## 75:          low income          low income
    ##                   LMH09               LMH14

\#\#\#\#cvd14fem

``` r
na.omit(edit, c("cvd14fem"), invert = TRUE) #missing cvdfem -> no missing
```

    ## Empty data.table (0 rows and 129 cols): location_name,GSNI_PERIOD,onebias,twobias,nobias,political...

\#\#\#\#cvd14

``` r
na.omit(edit, c("cvd14both"), invert = TRUE) #missing cvdboth -> no missing
```

    ## Empty data.table (0 rows and 129 cols): location_name,GSNI_PERIOD,onebias,twobias,nobias,political...

\#\#\#\#LEbirth2015

``` r
na.omit(edit, c("LEbirth2015"), invert = TRUE) #missing LE at birth (Andorra, Palestine and UK)
```

    ##          location_name GSNI_PERIOD onebias twobias nobias political economic
    ## 1:             Andorra   2005–2009   27.01    7.43  72.99     14.08     8.73
    ## 2: Palestine, State of   2010–2014   98.00   92.30   2.00     89.30    79.50
    ## 3:      United Kingdom   2005–2009   54.60   25.50  45.40     26.07    25.15
    ##    educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem  cvd09fem
    ## 1:        1.81    12.01  94.99074  94.10549  92.00298  91.82684  93.03548
    ## 2:       26.70    83.50 367.37503 362.07740 355.82036 350.58586 343.97033
    ## 3:        6.65    30.34 147.03415 138.97711 132.69706 127.54333 120.00812
    ##     cvd10fem  cvd11fem  cvd12fem  cvd13fem cvd14fem  cvd15fem  cvd16fem
    ## 1:  93.64847  94.72272  94.77537  95.38794  95.7992  96.76678  96.28366
    ## 2: 341.78855 337.69661 320.17827 314.63758 323.3969 338.52757 350.93555
    ## 3: 115.83452 112.45472 112.30358 111.23225 108.8208 108.30317 106.14944
    ##     cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ## 1:  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052  126.3548
    ## 2: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 3: 104.51284 106.93075 107.12455  224.9909  213.8383  203.5031  195.1599
    ##    cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ## 1:  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398  119.3653
    ## 2:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 3:  185.3431  178.1320  171.7848  168.7043  167.4859  163.1700  161.8943
    ##    cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ## 1:  118.6682  117.7883  116.9475  115.8910  114.4628  112.7694  110.3326
    ## 2:  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939  415.8933
    ## 3:  159.0024  157.5910  160.8536  161.2392  181.8112  172.5648  164.6845
    ##    cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ## 1:  109.3167  109.3833  109.1886  109.4555  108.8728  108.7915  108.3474
    ## 2:  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001  368.8565
    ## 3:  158.1859  149.6043  144.0946  139.3859  137.9374  136.8413  133.6982
    ##    cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ## 1:  108.1292  107.5617  106.9530  106.3957  105.7119          NA       NA
    ## 2:  380.8886  384.4684  388.1755  386.6795  381.6978          NA       NA
    ## 3:  132.9974  130.6090  129.1214  132.0264  132.3753          NA       NA
    ##    LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05  phy06
    ## 1:          NA       NA          NA       NA          NA       NA 3.2319 3.0123
    ## 2:          NA       NA          NA       NA          NA       NA     NA     NA
    ## 3:          NA       NA          NA       NA          NA       NA 2.4120 2.4590
    ##     phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14  phy15  phy16  phy17
    ## 1: 3.0109     NA 3.1479 4.0000     NA     NA     NA     NA 3.3333     NA     NA
    ## 2:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 3: 2.4844 2.5543 2.6279 2.6265 2.6603 2.6667 2.6774 2.7149 2.7465 2.7563 2.7863
    ##                  HE05               HE06               HE07               HE08
    ## 1: 5.5754260999999996 4.9350943599999999 4.9255475999999998 5.8059859300000003
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3: 8.5339870500000004 8.6973781599999995 8.8732242600000006 9.1662340199999992
    ##                  HE09               HE10               HE11               HE12
    ## 1: 6.2023372700000001         6.64963865 6.2465286300000002 6.1015033699999996
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3:        10.01600361 9.9891519500000001 9.9734649700000002 10.051768300000001
    ##                  HE13               HE14               HE15               HE16
    ## 1: 5.9878034600000003 5.9791245499999999 6.2324533500000001 6.3434934600000004
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3: 9.9785518599999996 9.9575843800000001 9.9043521900000009 9.8667116200000002
    ##                  HE17 scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12 scl13
    ## 1: 6.5443186799999999   9.8  10.1  10.1  10.1  10.1  10.1  10.2  10.2  10.2
    ## 2:               <NA>    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3: 9.8251123400000004  12.2  12.4  12.6  12.8  13.1  13.2  13.0  12.9  12.6
    ##    scl14 scl15 scl16 scl17    GDP05    GDP06    GDP07   GDP08    GDP09    GDP10
    ## 1:  10.2  10.2  10.2  10.2 40066.26 42675.81 47803.69 48718.5 43503.19 40852.67
    ## 2:    NA    NA    NA    NA       NA       NA       NA      NA       NA       NA
    ## 3:  12.7  12.8  12.9  12.9 42030.29 44599.70 50566.83 47287.0 38713.14 39435.84
    ##       GDP11    GDP12    GDP13    GDP14    GDP15    GDP16    GDP17 MMR05 MMR06
    ## 1: 43335.33 38686.46 39538.77 41303.93 35762.52 37474.67 38962.88    NA    NA
    ## 2:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ## 3: 42038.57 42462.77 43444.53 47425.61 44974.83 41064.13 40361.42    11    11
    ##    MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:    11    11    10    10     9     8     8     8     8     7     7
    ##          LMH09       LMH14
    ## 1: high income high income
    ## 2:        <NA>        <NA>
    ## 3: high income high income

``` r
edit[c("Andorra", "Palestine, State of", "United Kingdom"), c("location_name", "LEbirth2000", "LEbirth2010", "LEbirth2015", "LEbirth2019")]
```

    ##          location_name LEbirth2000 LEbirth2010 LEbirth2015 LEbirth2019
    ## 1:             Andorra          NA          NA          NA          NA
    ## 2: Palestine, State of          NA          NA          NA          NA
    ## 3:      United Kingdom          NA          NA          NA          NA

``` r
#no data points that can be used to fill LE at birth for these countries
```

Have data for UK from IHME from 2015
<https://vizhub.healthdata.org/gbd-compare/le> LE at birth for females
2015 = 84.8

``` r
edit$LEbirth2015[68] <- 82.83 #inserted UK value

edit[68,LEbirth2015]
```

    ## [1] 82.83

Have data for Andorra from IHME from 2015
<https://vizhub.healthdata.org/gbd-compare/le> LE at birth for females
2015 = 84.8

``` r
edit$LEbirth2015[2] <- 84.8 #inserted Andorra value

edit[2,LEbirth2015]
```

    ## [1] 84.8

Have data for Palestine from IHME from 2015
<https://vizhub.healthdata.org/gbd-compare/le> LE at birth for females
2015 = 76.22

``` r
edit$LEbirth2015[48] <- 76.22 #inserted Palestine value

edit[48,LEbirth2015]
```

    ## [1] 76.22

\#\#\#\#phy14

``` r
na.omit(edit, c("phy14"), invert = TRUE) #missing phy (23 rows)
```

    ##           location_name GSNI_PERIOD onebias twobias nobias political economic
    ##  1:             Algeria   2010–2014   97.83   87.00   2.17     80.08    74.08
    ##  2:             Andorra   2005–2009   27.01    7.43  72.99     14.08     8.73
    ##  3:           Argentina   2010–2014   75.41   42.49  24.59     43.35    30.43
    ##  4:              Brazil   2010–2014   89.50   52.39  10.50     43.41    36.63
    ##  5:             Ecuador   2010–2014   93.34   58.90   6.66     46.34    36.44
    ##  6:            Ethiopia   2005–2009   85.27   35.14  14.73     30.27    22.00
    ##  7:               Ghana   2010–2014   99.16   92.69   0.84     86.84    78.01
    ##  8:               Haiti   2010–2014   98.91   92.82   1.09     76.33    72.06
    ##  9:           Indonesia   2005–2009   97.44   80.36   2.56     66.47    66.40
    ## 10:          Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80    71.53
    ## 11:            Malaysia   2010–2014   98.54   88.38   1.46     79.69    74.54
    ## 12:                Mali   2005–2009   98.82   93.36   1.18     81.89    88.87
    ## 13:             Nigeria   2010–2014   99.73   94.99   0.27     85.83    83.42
    ## 14: Palestine, State of   2010–2014   98.00   92.30   2.00     89.30    79.50
    ## 15:                Peru   2010–2014   87.96   49.99  12.04     38.44    27.05
    ## 16:         Philippines   2010–2014   98.87   86.80   1.13     70.62    73.80
    ## 17:             Romania   2010–2014   85.50   60.84  14.50     48.78    55.88
    ## 18:            Thailand   2010–2014   95.47   74.50   4.53     67.30    50.86
    ## 19: Trinidad and Tobago   2010–2014   85.99   51.25  14.01     39.14    37.74
    ## 20:             Uruguay   2010–2014   74.60   36.70  25.40     28.60    34.30
    ## 21:            Viet Nam   2005–2009   92.89   69.17   7.11     59.40    62.49
    ## 22:               Yemen   2010–2014   97.80   92.10   2.20     87.40    87.20
    ## 23:              Zambia   2005–2009   96.84   80.56   3.16     66.04    55.41
    ##           location_name GSNI_PERIOD onebias twobias nobias political economic
    ##     educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem  cvd09fem
    ##  1:       37.17    86.75 500.26865 494.78751 490.17488 486.31725 483.19933
    ##  2:        1.81    12.01  94.99074  94.10549  92.00298  91.82684  93.03548
    ##  3:       17.04    52.86 169.99370 165.87751 165.37005 156.11837 153.96832
    ##  4:        9.32    77.95 192.71662 188.83657 184.09958 179.74663 176.62840
    ##  5:       23.46    84.36 161.28268 164.29742 163.35005 160.95424 157.69322
    ##  6:        8.00    80.60 261.82080 255.18410 248.43715 242.03145 236.24831
    ##  7:       30.02    90.73 325.79930 328.95890 329.44442 331.09718 332.15021
    ##  8:       59.91    88.13 526.03693 523.13684 519.11249 514.70529 510.66940
    ##  9:       19.31    90.55 369.65859 371.69064 371.74037 373.60996 373.65398
    ## 10:       41.00    81.73 546.88719 553.71417 543.25326 536.85221 507.41753
    ## 11:       43.00    94.31 278.24360 271.41947 262.56217 262.28229 263.15505
    ## 12:       47.61    84.87 320.56474 319.77363 317.97481 317.98693 317.14954
    ## 13:       46.18    92.78 277.35636 271.85405 263.52070 259.79173 257.07280
    ## 14:       26.70    83.50 367.37503 362.07740 355.82036 350.58586 343.97033
    ## 15:       14.36    79.76  97.82452  91.17324  84.14951  83.74437  93.03498
    ## 16:       39.08    91.48 293.05106 293.56564 287.95050 287.91056 287.31183
    ## 17:       20.69    63.54 458.08365 437.32047 413.66866 400.71262 397.43462
    ## 18:       29.02    84.53 148.01004 142.41904 137.40121 132.15039 125.40487
    ## 19:        5.61    72.17 244.42399 237.87706 229.26124 224.70676 212.68349
    ## 20:        9.20    51.40 181.89537 171.40400 172.51409 160.79502 150.85425
    ## 21:       20.36    70.56 256.62741 256.62311 255.97453 255.19294 254.46217
    ## 22:       45.30    81.00 493.05999 489.24403 485.10075 483.33838 478.34072
    ## 23:       23.53    89.07 301.19131 304.24329 301.25347 299.44540 297.43078
    ##     educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem  cvd09fem
    ##      cvd10fem  cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem  cvd16fem
    ##  1: 484.49190 484.48211 479.07575 472.94269 471.02644 468.44578 458.09789
    ##  2:  93.64847  94.72272  94.77537  95.38794  95.79920  96.76678  96.28366
    ##  3: 154.88168 152.74845 149.31115 147.59175 146.19661 147.86150 153.18030
    ##  4: 172.89359 169.38552 163.58428 159.43643 155.71901 153.99486 153.37633
    ##  5: 157.35397 149.29192 153.73253 152.93464 152.05617 148.90913 149.01126
    ##  6: 231.66182 230.61082 229.20585 225.91810 224.28592 223.92249 222.61808
    ##  7: 332.89254 332.14372 330.11120 328.64703 326.15716 329.48438 327.68822
    ##  8: 508.21671 512.24372 507.65492 502.18482 497.94515 495.34101 489.48808
    ##  9: 372.87334 371.44090 372.29798 370.74477 368.80629 368.52390 365.10278
    ## 10: 488.18726 483.29378 465.05092 442.79747 429.36559 435.06853 415.09331
    ## 11: 252.21083 237.09570 233.23215 226.51494 228.10326 227.29230 224.59851
    ## 12: 316.24847 314.96309 313.19762 310.27834 307.23772 309.21589 307.72590
    ## 13: 254.32157 253.99749 254.09391 253.49508 249.71126 252.45876 247.75393
    ## 14: 341.78855 337.69661 320.17827 314.63758 323.39695 338.52757 350.93555
    ## 15:  94.03452  91.93528  89.98908  87.97633  83.77122  80.26885  78.92477
    ## 16: 284.21639 281.45974 280.20698 277.95194 274.33572 275.99697 273.52319
    ## 17: 386.99741 365.99837 359.62475 340.34775 338.83849 330.12565 320.96970
    ## 18: 122.07400 115.60825 109.16780 103.96458 100.77038  97.84838  97.65457
    ## 19: 204.48233 192.87097 183.77914 188.94522 190.52675 189.97215 194.01692
    ## 20: 147.47007 150.67799 146.49314 140.61544 136.51041 136.27482 133.88372
    ## 21: 252.55867 250.17926 247.05521 243.05616 239.21285 234.94030 230.61406
    ## 22: 472.17590 471.35922 468.99902 467.16132 464.47411 466.86730 466.88438
    ## 23: 300.60435 303.67459 305.16350 306.41551 307.02384 308.97725 311.38853
    ##      cvd10fem  cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem  cvd16fem
    ##      cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ##  1: 450.74425 451.34683 447.67418  462.4656  450.5957  439.3599  428.9066
    ##  2:  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052  126.3548
    ##  3: 154.09804 150.26997 149.24294  265.4632  258.6674  258.7628  246.9013
    ##  4: 149.03982 148.29788 146.95291  269.7419  264.7693  259.8278  255.9295
    ##  5: 149.63616 148.94035 146.37173  195.4146  197.5540  198.2290  197.6351
    ##  6: 221.95432 222.81058 223.55265  276.7356  271.0027  264.5905  258.7780
    ##  7: 326.79742 324.50422 324.16728  308.0771  311.7312  310.7842  310.4548
    ##  8: 483.52984 479.46926 475.27316  431.5242  430.9686  431.2913  431.2199
    ##  9: 361.90945 358.11468 354.06925  386.6718  391.2950  397.2689  402.7375
    ## 10: 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687  761.4547
    ## 11: 222.86437 221.09851 220.62850  326.1480  318.3576  312.0718  316.5872
    ## 12: 305.13664 304.19901 301.76476  250.4399  251.6409  253.7490  256.6272
    ## 13: 245.20993 242.25928 239.99721  274.4590  267.8654  259.5376  255.2531
    ## 14: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 15:  78.51316  77.75822  77.12523  118.1332  114.0556  109.6057  107.1766
    ## 16: 271.92029 269.31716 266.26944  375.8930  378.2835  374.6201  378.4958
    ## 17: 317.98703 317.44903 319.76750  594.6991  574.2798  550.0335  547.3480
    ## 18:  96.75148  96.78591  96.50541  179.0414  171.3873  164.7609  159.7038
    ## 19: 194.33779 194.26330 193.63737  339.4050  312.2339  301.5650  311.0354
    ## 20: 134.15097 133.06846 132.74195  264.6658  253.9937  254.2766  236.7589
    ## 21: 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026  455.8597
    ## 22: 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200  566.4953
    ## 23: 308.67000 306.10250 303.60330  396.9908  395.9378  389.5186  383.8555
    ##      cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ##     cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ##  1:  419.1618  408.5190  403.3053  397.5104  389.1503  384.7680  383.2925
    ##  2:  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398  119.3653
    ##  3:  240.5962  239.1611  236.2446  233.3321  230.6517  226.4522  227.7888
    ##  4:  252.0371  248.5607  244.3169  236.3190  231.1611  225.6046  222.4439
    ##  5:  191.5033  191.5863  191.7630  187.9508  186.7919  185.9278  184.5895
    ##  6:  252.8036  245.2730  242.6073  240.1706  236.7341  234.6738  233.0782
    ##  7:  309.2160  308.0875  307.7036  305.1269  301.1806  296.2265  297.1589
    ##  8:  430.6858  431.0238  433.7781  433.2272  432.0381  430.4305  429.9405
    ##  9:  406.8324  409.3400  411.8711  416.3984  418.3803  419.7302  420.4248
    ## 10:  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342  644.2163
    ## 11:  315.2901  303.3995  295.3241  285.1897  264.2568  267.6031  268.1236
    ## 12:  259.4479  259.6777  258.0625  256.5280  255.5163  253.8127  257.2386
    ## 13:  252.9699  250.7906  250.3251  250.0944  248.4156  244.0515  246.2866
    ## 14:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 15:  117.1724  120.0147  118.2343  116.0098  113.0409  107.4914  103.9618
    ## 16:  380.0939  378.0533  378.7095  380.0489  377.3414  376.7860  376.6431
    ## 17:  543.1993  535.6756  501.2370  496.4683  468.9440  476.7701  469.2057
    ## 18:  155.0085  153.1022  147.5108  142.4819  139.5502  139.3282  140.4503
    ## 19:  288.0985  279.1031  263.7642  248.9993  253.3945  255.4942  257.6763
    ## 20:  226.8973  220.8039  222.2131  216.9713  210.2661  203.4770  201.5381
    ## 21:  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765  443.7173
    ## 22:  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702  536.2482
    ## 23:  377.2154  376.4333  380.5640  379.4144  379.6064  381.3379  385.9456
    ##     cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ##     cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ##  1:  379.8222  377.6635  373.4875  371.5185  479.0988  469.5897 460.60996
    ##  2:  118.6682  117.7883  116.9475  115.8910  114.4628  112.7694 110.33258
    ##  3:  234.6520  235.0038  228.2053  226.1503  212.7781  207.4933 207.17746
    ##  4:  222.8537  215.6675  211.9121  210.6516  227.7234  223.2779 218.36193
    ##  5:  186.8145  189.4857  189.0448  182.2878  177.6049  180.0888 179.95002
    ##  6:  230.5664  228.5969  228.0727  227.3755  269.7873  263.5363 256.88434
    ##  7:  294.3840  292.5341  288.0546  285.0981  320.4876  323.9869 323.92028
    ##  8:  426.5772  424.1333  423.1650  419.7853  481.0125  479.0757 477.03676
    ##  9:  418.4559  416.6262  414.8512  412.4565  378.9587  382.2896 385.13312
    ## 10:  618.5638  613.3843  580.3176  569.3491  634.0089  651.1682 645.59071
    ## 11:  275.7751  278.4519  279.6520  290.3302  304.0469  296.6711 289.07676
    ## 12:  257.6357  257.6290  255.5112  252.2948  284.7410  284.8986 285.02779
    ## 13:  241.7979  238.7568  235.4675  232.2572  278.5825  272.1292 263.44130
    ## 14:  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939 415.89329
    ## 15:  102.7772  102.2740  101.7696  101.1460  107.5290  102.1196  96.34764
    ## 16:  373.1642  366.5366  359.5821  354.9366  331.7122  332.2420 327.14809
    ## 17:  463.8929  457.5351  455.3877  454.2815  519.7798  499.0081 475.04244
    ## 18:  143.4768  143.6757  144.4356  144.1269  163.4339  156.8782 151.11043
    ## 19:  263.8739  264.4925  263.7471  263.2442  289.8596  274.1400 264.62657
    ## 20:  198.5235  198.8982  198.1326  197.6541  218.9647  208.2510 208.91637
    ## 21:  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238 336.52387
    ## 22:  536.1051  541.0473  547.2489  550.6548  535.1088  530.9694 525.70885
    ## 23:  390.7901  386.0968  382.3781  377.5337  348.3714  349.1627 344.28588
    ##     cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ##     cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ##  1: 452.32621  444.6443  437.7233  433.4500  426.8407  418.7057 415.16724
    ##  2: 109.31671  109.3833  109.1886  109.4555  108.8728  108.7915 108.34739
    ##  3: 196.63216  192.6343  192.5741  190.0731  186.7114  184.5299 181.88855
    ##  4: 214.14929  210.6505  206.9642  203.0646  196.2723  191.6573 187.13184
    ##  5: 178.65734  174.1443  173.9953  169.8008  170.3395  169.4053 168.52380
    ##  6: 250.72570  244.8102  238.6314  236.7101  234.7540  231.3693 229.49538
    ##  7: 324.82630  324.9608  324.9526  324.4065  322.1898  319.7047 316.18344
    ##  8: 474.64371  472.2518  471.2477  474.8282  472.1792  468.7702 465.82635
    ##  9: 388.81052  390.8450  391.6793  392.1266  394.7447  394.8640 394.48450
    ## 10: 632.76151  603.3141  581.8522  562.6225  543.9251  519.5371 513.01612
    ## 11: 291.31778  291.2486  279.5220  267.4026  260.4545  246.4554 248.03609
    ## 12: 286.46916  287.4945  287.1683  285.6945  284.0338  282.0897 279.73392
    ## 13: 259.09633  256.2984  253.4984  252.8035  252.4680  251.0900 246.82989
    ## 14: 407.15878  399.0904  394.3262  387.9774  366.8452  355.9001 368.85654
    ## 15:  94.97714  104.6293  106.4933  104.5499  102.4466   99.9540  95.08345
    ## 16: 328.81843  329.1745  326.6859  325.5405  325.4797  322.9125 320.55214
    ## 17: 466.40175  462.7888  453.2380  426.2066  420.4295  397.5164 400.02892
    ## 18: 145.88745  139.9720  137.2788  131.1732  125.2963  121.0188 119.09116
    ## 19: 266.62697  249.3102  240.6988  227.3527  215.5430  220.2906 222.12845
    ## 20: 194.57793  184.5496  180.0692  182.5667  177.8057  171.3896 165.97775
    ## 21: 337.02684  337.1512  335.4539  333.0107  329.9312  326.6030 323.18282
    ## 22: 523.71933  517.1861  508.7890  507.4918  504.3873  501.9957 497.81417
    ## 23: 340.42203  336.0299  337.2284  340.7799  340.9955  341.7355 342.88671
    ##     cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ##     cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ##  1: 412.80644  405.9023 401.39469 399.50515 396.96522        73.5     20.3
    ##  2: 108.12924  107.5617 106.95304 106.39570 105.71185          NA       NA
    ##  3: 183.42152  189.4145 190.14065 185.28922 183.93309        77.8     22.5
    ##  4: 184.72431  184.5153 178.94359 176.98645 175.66145        75.2     21.3
    ##  5: 166.17046  167.1609 168.67782 168.10363 163.65500        77.3     23.1
    ##  6: 228.51537  226.5753 225.23902 225.38551 225.38539        51.7     14.9
    ##  7: 318.52444  316.3285 315.05671 311.95072 310.60655        60.8     17.3
    ##  8: 464.27802  459.6614 455.40585 452.87085 449.11377        57.2     15.9
    ##  9: 394.73382  392.0003 389.44109 386.56637 383.26115        68.6     18.2
    ## 10: 523.15984  500.1438 493.82082 473.26712 466.31300        70.2     18.3
    ## 11: 247.15687  249.7038 250.28741 250.11953 255.54454        75.3     19.2
    ## 12: 282.44832  281.8948 280.58319 279.03154 276.18486        52.7     15.9
    ## 13: 249.11815  244.3857 241.54042 238.38554 235.65202        55.2     17.0
    ## 14: 380.88863  384.4684 388.17552 386.67955 381.69777          NA       NA
    ## 15:  91.54823   90.2800  89.85563  89.23082  88.61384        75.9     22.9
    ## 16: 321.36521  318.5080 315.14778 310.89918 307.32206        72.9     19.7
    ## 17: 391.65760  384.1841 379.99900 378.78972 379.62640        74.9     19.7
    ## 18: 117.90308  119.1497 118.67228 118.96768 118.66561        75.2     22.1
    ## 19: 222.78825  227.7660 228.23401 227.86298 227.29800        72.6     19.8
    ## 20: 164.96208  162.3258 162.77541 161.88740 161.55863        78.8     23.1
    ## 21: 319.13626  314.9546 311.23748 307.70802 303.90466        75.6     21.0
    ## 22: 500.90386  500.8572 504.82748 509.87687 513.09104        64.7     17.9
    ## 23: 346.10827  349.7302 346.15135 343.09096 339.55436        45.2     14.7
    ##     cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ##     LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05
    ##  1:        76.8     21.9       77.50     22.3        78.1     22.6 1.0242
    ##  2:          NA       NA       84.80       NA          NA       NA 3.2319
    ##  3:        78.6     22.7       79.30     23.0        79.5     23.1 3.2100
    ##  4:        77.7     22.4       78.60     23.0        79.4     23.5 1.6663
    ##  5:        77.9     22.8       79.50     23.6        80.5     24.3     NA
    ##  6:        64.5     18.3       68.60     19.1        70.5     19.4 0.0321
    ##  7:        64.5     17.9       67.40     18.6        69.2     18.9     NA
    ##  8:        35.4     12.0       63.10     16.7        64.8     17.0     NA
    ##  9:        71.0     18.4       72.50     18.8        73.3     19.1     NA
    ## 10:        73.3     19.3       75.30     20.2        77.3     21.7     NA
    ## 11:        76.5     20.1       77.10     20.7        77.1     20.6     NA
    ## 12:        59.5     17.0       61.40     17.2        63.4     17.6     NA
    ## 13:        60.8     18.0       62.80     18.5        64.1     18.9 0.2824
    ## 14:          NA       NA       76.22       NA          NA       NA     NA
    ## 15:        78.9     23.8       80.60     24.7        81.3     25.1     NA
    ## 16:        73.5     19.9       73.70     19.7        73.6     19.6 1.2289
    ## 17:        77.4     21.1       78.40     21.9        79.3     22.4 2.2126
    ## 18:        79.3     23.8       80.70     24.7        81.0     24.8 0.2930
    ## 19:        75.7     21.5       78.30     23.7        79.9     25.0     NA
    ## 20:        80.0     23.7       80.60     24.1        80.6     24.0     NA
    ## 21:        77.1     21.5       77.60     21.8        78.1     22.0     NA
    ## 22:        69.6     18.7       69.60     18.9        68.9     18.7     NA
    ## 23:        59.1     17.1       63.00     17.7        65.4     18.0 0.0545
    ##     LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05
    ##      phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13 phy14  phy15  phy16
    ##  1:     NA 1.1958     NA     NA 1.2070     NA     NA     NA    NA     NA 1.8325
    ##  2: 3.0123 3.0109     NA 3.1479 4.0000     NA     NA     NA    NA 3.3333     NA
    ##  3:     NA     NA     NA     NA 3.2100     NA     NA 3.9385    NA     NA 4.0013
    ##  4: 1.7007 1.7306 1.7802 1.8171 1.8139 1.8491     NA 1.8820    NA     NA     NA
    ##  5:     NA     NA     NA 1.5983 2.1111 1.6582     NA     NA    NA 2.0648 2.0368
    ##  6: 0.0269 0.0224 0.0251 0.0252 0.0220     NA     NA     NA    NA     NA 0.0464
    ##  7:     NA 0.0725 0.0787 0.0841 0.0938 0.0979 0.0956 0.1686    NA     NA 0.1270
    ##  8:     NA     NA     NA     NA     NA 0.1378     NA     NA    NA 0.0852     NA
    ##  9: 0.1300 0.2880     NA 0.1448 0.1395     NA 0.3075 0.3118    NA 0.2738     NA
    ## 10:     NA     NA     NA     NA     NA     NA     NA     NA    NA     NA     NA
    ## 11:     NA     NA 0.9216     NA 1.1691 1.2777 1.3320     NA    NA 1.5358     NA
    ## 12:     NA 0.0776 0.0490 0.0923 0.1021 0.1037 0.1071     NA    NA     NA 0.1395
    ## 13: 0.3481 0.3784 0.3762 0.3782 0.1836     NA     NA 0.3828    NA     NA 0.4494
    ## 14:     NA     NA     NA     NA     NA     NA     NA     NA    NA     NA     NA
    ## 15:     NA 1.6648     NA 0.9472 0.9200     NA 1.1411     NA    NA     NA 1.3048
    ## 16: 1.2361 1.2441 1.2558 1.2632 1.2717     NA     NA     NA    NA     NA     NA
    ## 17: 2.2104 2.2915 2.4133 2.4414 2.4804 2.5153 2.5887 2.6240    NA     NA 2.2565
    ## 18: 0.2875 0.2959 0.3164 0.3387 0.3906     NA     NA     NA    NA 0.4651 0.4450
    ## 19: 1.2222 1.1792     NA 1.5081 1.8086 1.8193     NA     NA    NA 2.6498     NA
    ## 20:     NA 4.1810 3.9510     NA 3.7360     NA     NA     NA    NA     NA 3.9558
    ## 21:     NA     NA     NA     NA     NA     NA     NA     NA    NA     NA     NA
    ## 22:     NA     NA     NA     NA     NA     NA     NA     NA    NA     NA     NA
    ## 23: 0.0533     NA 0.0619 0.0606 0.0614 0.1649 0.1658     NA    NA     NA 0.1628
    ##      phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13 phy14  phy15  phy16
    ##      phy17               HE05               HE06               HE07
    ##  1: 1.7879 3.2351613000000001         3.35510325 3.8214178099999998
    ##  2:     NA 5.5754260999999996 4.9350943599999999 4.9255475999999998
    ##  3: 3.9901 7.6107888199999998 7.6403284100000004 7.8345508600000002
    ##  4: 2.1652 8.0440778700000006 8.2589139899999999 8.2077617600000004
    ##  5:     NA         5.57841349 5.6872220000000002 5.8648901000000002
    ##  6: 0.0986 4.1009812400000003 4.4575729400000004 5.0012836500000004
    ##  7: 0.1359 3.9777960800000001 3.9613141999999999 4.0661277800000004
    ##  8:     NA 5.5093555500000004 5.5010376000000001 5.9015803299999998
    ##  9: 0.3767         2.58409047         2.67202187 2.8761706399999998
    ## 10:     NA               <NA>               <NA>               <NA>
    ## 11:     NA 2.8012416400000002 3.1245324600000002         3.08224082
    ## 12:     NA 5.1953811600000002 5.4565606100000004 5.2536678300000004
    ## 13:     NA 4.4659194900000001 4.2577514599999997         3.90997219
    ## 14:     NA               <NA>               <NA>               <NA>
    ## 15:     NA 4.5863194500000004 4.5380153700000001 4.4038014399999996
    ## 16: 0.6004 3.9004185200000001 3.9458506099999999 3.9195413600000002
    ## 17: 2.9807 5.5277814899999997 5.0710606599999997 5.0222334899999996
    ## 18: 0.8075 3.1597218499999999 3.0997068900000002 3.1927750100000001
    ## 19: 3.3646 4.4345116600000001 4.1543202399999997 4.1536026000000001
    ## 20: 5.0794 8.4265871000000008 8.4113969799999992 8.1404838599999998
    ## 21:     NA               <NA>               <NA>               <NA>
    ## 22:     NA               <NA>               <NA>               <NA>
    ## 23:     NA 6.8633222600000003 5.8736734400000001 4.3526496899999998
    ##      phy17               HE05               HE06               HE07
    ##                   HE08               HE09               HE10               HE11
    ##  1: 4.2018823599999999 5.3593983700000001 5.1171698599999997 5.2674808500000001
    ##  2: 5.8059859300000003 6.2023372700000001         6.64963865 6.2465286300000002
    ##  3: 8.1826972999999992 9.4559955599999999 9.4454641299999995 9.4181985899999994
    ##  4: 8.0159883500000007 8.4025468799999992 7.9491324399999996 7.7883334199999998
    ##  5: 5.8163766900000002 6.4387331000000003 7.1219563499999996 7.8652181600000004
    ##  6: 4.2806391699999997 4.6498341600000002 5.4663720099999997 4.4689779300000003
    ##  7: 4.1973075900000003 4.7066750500000003 4.6920676200000004 4.7444992099999999
    ##  8: 6.0159010899999998 6.1691598900000004 8.1455984099999998         10.2313633
    ##  9:         2.61300087 2.6849434400000001 2.9608998299999998 2.9554202599999999
    ## 10:               <NA>               <NA>               <NA>               <NA>
    ## 11: 3.0235738799999998 3.2762939900000001         3.18466234 3.3399648700000002
    ## 12: 5.0454120600000003 5.2291111900000002 4.6792340299999999 4.0157337200000001
    ## 13: 3.6958153199999999 3.5801973299999998 3.2965328700000001 3.3207793200000002
    ## 14:               <NA>               <NA>               <NA>               <NA>
    ## 15: 4.4459280999999997 4.9506058700000004 4.7206191999999998 4.6209321000000001
    ## 16: 4.0279226299999999 4.3535318399999996 4.3127627400000002 4.2060928300000002
    ## 17: 5.0227422700000002 5.2784128199999998 5.7690625200000003 4.6990156199999999
    ## 18: 3.4593396200000002          3.6193974         3.39009976 3.5682563799999998
    ## 19: 3.6575703599999998         5.48676443         5.08042908 4.7219791400000002
    ## 20: 8.8268699599999998 8.6362743399999999 8.5942153900000005 8.6352901499999994
    ## 21:               <NA>               <NA>               <NA>               <NA>
    ## 22:               <NA>               <NA>               <NA>               <NA>
    ## 23: 4.0120754200000004         4.42680454 3.7192959800000001 3.4605324300000002
    ##                   HE08               HE09               HE10               HE11
    ##                   HE12               HE13               HE14               HE15
    ##  1:         6.00050974 6.0357627899999997 6.5472140300000001 6.9784917799999997
    ##  2: 6.1015033699999996 5.9878034600000003 5.9791245499999999 6.2324533500000001
    ##  3: 9.8272666900000001 9.7809791599999993         9.67129993 10.229337689999999
    ##  4:         7.73536205 7.9772124299999998 8.3962497700000007 8.8709096899999995
    ##  5: 8.4796819699999997 8.5598344799999992 8.6212749500000001 8.5884571100000002
    ##  6: 4.5395960799999999 4.0750651400000004 4.0336551700000003         3.82316828
    ##  7: 4.1872339199999997 4.6230053900000003 4.1004481300000002 4.6217145899999998
    ##  8: 9.6682958600000006         7.23788214 7.7976360299999996 8.6285295499999997
    ##  9: 2.9028665999999999 2.9606506800000001 3.1171209800000002         2.99113512
    ## 10:               <NA>               <NA>               <NA>               <NA>
    ## 11: 3.4879069299999998 3.5224008599999999 3.7252702700000002 3.8160853399999999
    ## 12: 3.7521157299999999 3.9675071200000001 4.4815611799999999 4.1114335100000003
    ## 13: 3.3598427800000001 3.4206934000000002 3.3484041699999998 3.5819501900000001
    ## 14:               <NA>               <NA>               <NA>               <NA>
    ## 15: 4.7565517399999999 4.7019739200000004 4.9947280899999997         5.03110838
    ## 16: 4.3746528600000003          4.4638114 4.1274123200000004 4.3213686899999999
    ## 17: 4.7180356999999997 5.1926212300000003 5.0300502800000002 4.9426484100000003
    ## 18: 3.5229399199999998 3.4531335799999998 3.6844804299999998 3.6678931700000001
    ## 19: 4.8760519000000002 5.0591573700000003 5.2278909699999998 6.0395116800000004
    ## 20: 8.7641210600000008 8.8199577300000005 8.7702360200000005 9.0082111400000002
    ## 21:               <NA>               <NA>               <NA>               <NA>
    ## 22:               <NA>               <NA>               <NA>               <NA>
    ## 23: 3.9305291200000001 4.6909103400000003 3.8292422300000002 4.4351024600000004
    ##                   HE12               HE13               HE14               HE15
    ##                   HE16               HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ##  1:         6.60778189         6.38032866   6.9   7.0   7.2   6.7   6.9   7.1
    ##  2: 6.3434934600000004 6.5443186799999999   9.8  10.1  10.1  10.1  10.1  10.1
    ##  3: 9.0018968600000004 10.457043649999999   9.1   9.0   8.9   8.9   9.3   9.8
    ##  4: 9.2074222599999995 9.4693374600000002   6.3   6.4   6.5   6.7   6.8   6.9
    ##  5: 8.2959604299999992 8.2574291199999994   7.3   7.3   7.3   7.9   7.9   7.9
    ##  6:         3.66298747 3.5033159299999999   1.9   2.0   2.1   2.2   2.3   2.3
    ##  7: 3.4615774199999998 3.3297784300000002   6.4   6.5   6.5   6.6   6.7   6.7
    ##  8: 8.4368677099999996 8.0846834199999993   4.3   4.4   4.5   4.6   4.7   4.7
    ##  9: 3.0856990799999999 2.8682405900000001   7.4   7.9   7.1   7.1   7.4   7.4
    ## 10:               <NA>               <NA>  10.2  10.2  10.3  10.3  10.4  10.6
    ## 11:         3.68835807         3.71246052   7.6   8.2   8.8   9.4   9.6   9.8
    ## 12: 3.7753798999999999 4.1038827900000001   1.7   1.7   1.8   1.9   1.9   2.0
    ## 13: 3.6477367900000002 3.7555384599999999   5.2   5.2   5.2   5.2   5.2   5.2
    ## 14:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 15: 5.0657653800000002 4.9937844299999998   8.7   8.1   8.1   8.4   8.4   8.4
    ## 16: 4.4068460500000004 4.4460597000000002   8.7   8.8   8.9   9.0   9.0   8.9
    ## 17: 4.9937720299999997 5.1508317000000003  10.1  10.3  10.5  10.6  10.6  10.7
    ## 18: 3.7633202099999998 3.8320283900000001   7.0   7.0   7.1   7.3   7.5   7.7
    ## 19: 7.0093059499999999 7.0202856100000002  10.0   9.9  10.5  10.7  10.7  10.7
    ## 20:         9.42438793 9.5163564699999998   8.0   8.0   8.2   8.4   8.4   8.4
    ## 21:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 22:               <NA>               <NA>   1.9   2.0   2.2   2.3   2.5   2.6
    ## 23: 4.4772071799999997 4.3974895500000004   6.3   6.4   6.4   6.5   6.5   6.6
    ##                   HE16               HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ##     scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05      GDP06      GDP07
    ##  1:   7.4   7.6   7.8   7.9   7.9   8.0   8.0  3113.0953  3478.7109  3946.6645
    ##  2:  10.2  10.2  10.2  10.2  10.2  10.2  10.2 40066.2569 42675.8128 47803.6936
    ##  3:   9.8   9.8   9.8   9.8   9.8   9.9   9.9  5109.8513  5919.0120  7245.4483
    ##  4:   7.1   7.3   7.4   7.4   7.6   7.8   7.8  4790.4371  5886.4636  7348.0308
    ##  5:   8.0   8.1   8.3   8.5   8.4   8.7   8.7  3002.1369  3328.8830  3567.8364
    ##  6:   2.4   2.4   2.5   2.5   2.6   2.7   2.7   162.4327   194.6874   244.2860
    ##  7:   6.8   6.8   6.9   6.9   6.9   7.1   7.1   492.5442   913.3939  1081.1663
    ##  8:   4.8   4.9   5.0   5.1   5.2   5.2   5.3   766.6921   792.8259   981.1113
    ##  9:   7.6   7.6   7.8   7.8   7.9   8.0   8.0  1263.2873  1589.8015  1860.0028
    ## 10:  10.6  10.7  10.7  10.8  10.8  10.9  10.9         NA         NA         NA
    ## 11:  10.1  10.1  10.1  10.1  10.2  10.2  10.2  5587.0256  6209.1245  7243.4560
    ## 12:   2.0   2.1   2.2   2.3   2.3   2.3   2.3   489.0211   523.0386   596.6902
    ## 13:   5.5   5.7   5.9   5.9   6.0   6.2   6.2  1268.3834  1656.4248  1883.4613
    ## 14:    NA    NA    NA    NA    NA    NA    NA         NA         NA         NA
    ## 15:   9.1   8.6   8.8   9.4   9.1   9.2   9.2  2729.4987  3154.3312  3606.0704
    ## 16:   9.0   9.1   9.1   9.2   9.3   9.3   9.3  1244.3490  1452.4387  1744.6403
    ## 17:  10.8  10.9  10.9  10.9  10.9  11.0  11.0  4617.9290  5757.4964  8360.1663
    ## 18:   7.5   7.7   7.5   7.6   7.6   7.6   7.6  2894.0627  3369.5434  3973.0170
    ## 19:  10.8  10.8  10.8  10.8  10.8  10.9  10.9 12327.2332 14102.4958 16539.8781
    ## 20:   8.4   8.5   8.5   8.6   8.7   8.7   8.7  5226.9378  5887.8487  7026.5115
    ## 21:    NA    NA    NA    NA    NA    NA    NA         NA         NA         NA
    ## 22:   2.8   3.0   3.0   3.0   3.0   3.0   3.0         NA         NA         NA
    ## 23:   6.7   6.7   6.8   6.9   6.9   7.0   7.0   702.7409  1047.9192  1124.2906
    ##     scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05      GDP06      GDP07
    ##          GDP08      GDP09      GDP10      GDP11      GDP12      GDP13
    ##  1:  4923.8432  3883.1324  4479.3417  5462.2609  5591.2124  5498.7841
    ##  2: 48718.4969 43503.1855 40852.6668 43335.3289 38686.4613 39538.7667
    ##  3:  9020.8731  8225.1372 10385.9644 12848.8642 13082.6643 13080.2547
    ##  4:  8831.0231  8597.9154 11286.2430 13245.6125 12370.0242 12300.3249
    ##  5:  4249.0193  4231.6158  4633.5904  5200.5558  5682.0450  6056.3308
    ##  6:   326.4368   380.5690   341.5541   354.4796   467.0779   499.5316
    ##  7:  1217.0648  1077.6622  1299.3449  1549.4629  1587.5612  2345.3929
    ##  8:  1076.7013  1150.2111  1172.0985  1287.9541  1337.3354  1393.9560
    ##  9:  2166.8542  2261.2472  3122.3628  3643.0439  3694.3489  3623.9116
    ## 10:         NA         NA         NA         NA         NA         NA
    ## 11:  8474.5868  7292.4944  9040.5663 10399.3728 10817.4429 10970.1233
    ## 12:   694.2777   698.8989   710.2742   837.6034   778.6193   805.0328
    ## 13:  2242.8719  1891.3354  2280.4374  2487.5982  2723.8228  2961.5503
    ## 14:         NA         NA         NA         NA         NA         NA
    ## 15:  4220.6170  4196.3128  5082.3548  5869.3231  6528.9722  6756.7528
    ## 16:  1991.2315  1905.8947  2217.4740  2450.7337  2694.3055  2871.4309
    ## 17: 10435.0440  8548.1187  8214.0769  9099.2175  8507.1048  9547.8522
    ## 18:  4379.6585  4213.0063  5076.3402  5492.1213  5860.5825  6168.2630
    ## 19: 21204.1050 14514.1417 16683.3554 19034.1492 19157.4170 20143.6644
    ## 20:  9091.0790  9451.9324 11992.0166 14236.6812 15171.5847 16973.6742
    ## 21:         NA         NA         NA         NA         NA         NA
    ## 22:         NA         NA         NA         NA         NA         NA
    ## 23:  1394.0006  1159.9078  1489.4593  1672.9083  1763.0727  1878.9097
    ##          GDP08      GDP09      GDP10      GDP11      GDP12      GDP13
    ##          GDP14      GDP15      GDP16      GDP17 MMR05 MMR06 MMR07 MMR08 MMR09
    ##  1:  5494.3523  4187.5097  3945.4821  4111.2941   127   122   119   117   117
    ##  2: 41303.9294 35762.5231 37474.6654 38962.8804    NA    NA    NA    NA    NA
    ##  3: 12334.7982 13789.0604 12790.2425 14613.0418    59    57    56    53    56
    ##  4: 12112.5882  8814.0010  8710.0967  9925.3862    71    72    71    70    69
    ##  5:  6377.0915  6124.4916  6060.0933  6213.5013    94    90    85    82    80
    ##  6:   566.9265   640.5419   717.1246   768.5223   865   795   731   681   638
    ##  7:  1971.0333  1743.8510  1931.3895  2025.9324   371   359   349   342   339
    ##  8:  1402.1002  1389.1195  1265.9876  1294.2397   459   467   473   484   484
    ##  9:  3491.6248  3331.6951  3562.8458  3837.6517   252   249   243   239   234
    ## 10:         NA         NA         NA         NA    NA    NA    NA    NA    NA
    ## 11: 11319.0798  9955.2437  9817.7385 10259.1818    31    30    30    29    29
    ## 12:   848.2741   751.4748   780.7186   830.0184   691   675   663   661   661
    ## 13:  3098.9863  2687.4801  2176.0022  1968.5647  1080  1040  1010   996   987
    ## 14:         NA         NA         NA         NA    NA    NA    NA    NA    NA
    ## 15:  6672.8803  6229.1017  6204.9973  6710.5080   118   114   112   108   106
    ## 16:  2959.6485  3001.0404  3073.6536  3123.2342   156   156   149   148   149
    ## 17: 10043.6774  8969.1489  9548.5874 10807.7954    35    32    30    28    28
    ## 18:  5951.8837  5840.0465  5994.2315  6592.9149    43    42    42    43    43
    ## 19: 20270.8594 18289.7043 16176.9474 16238.1932    76    73    65    72    74
    ## 20: 16831.9729 15613.7643 15387.1440 17322.1474    22    20    20    19    19
    ## 21:         NA         NA         NA         NA    NA    NA    NA    NA    NA
    ## 22:         NA         NA         NA         NA    NA    NA    NA    NA    NA
    ## 23:  1763.0626  1337.7956  1280.5784  1534.8668   421   406   387   356   329
    ##          GDP14      GDP15      GDP16      GDP17 MMR05 MMR06 MMR07 MMR08 MMR09
    ##     MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17               LMH09
    ##  1:   115   116   116   115   114   114   113   112 upper middle income
    ##  2:    NA    NA    NA    NA    NA    NA    NA    NA         high income
    ##  3:    51    48    47    44    42    41    40    39 upper middle income
    ##  4:    65    61    60    61    62    63    62    60 upper middle income
    ##  5:    78    76    71    67    65    63    61    59 lower middle income
    ##  6:   597   558   527   498   472   446   422   401          low income
    ##  7:   339   339   336   331   325   320   314   308          low income
    ##  8:   506   496   500   496   492   488   489   480          low income
    ##  9:   228   221   214   207   199   192   184   177 lower middle income
    ## 10:    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 11:    30    30    30    30    30    30    29    29 upper middle income
    ## 12:   660   663   663   663   642   620   590   562          low income
    ## 13:   978   972   963   951   943   931   925   917 lower middle income
    ## 14:    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 15:   104   102   100    98    96    94    91    88 upper middle income
    ## 16:   144   141   139   136   131   127   124   121 lower middle income
    ## 17:    27    24    23    21    21    21    21    19 upper middle income
    ## 18:    42    41    39    39    38    38    37    37 lower middle income
    ## 19:    71    72    71    70    69    68    68    67         high income
    ## 20:    17    17    17    18    17    18    18    17 upper middle income
    ## 21:    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 22:    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 23:   305   283   267   254   242   232   222   213          low income
    ##     MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17               LMH09
    ##                   LMH14
    ##  1: upper middle income
    ##  2:         high income
    ##  3:         high income
    ##  4: upper middle income
    ##  5: upper middle income
    ##  6:          low income
    ##  7: lower middle income
    ##  8:          low income
    ##  9: lower middle income
    ## 10:                <NA>
    ## 11: upper middle income
    ## 12:          low income
    ## 13: lower middle income
    ## 14:                <NA>
    ## 15: upper middle income
    ## 16: lower middle income
    ## 17: upper middle income
    ## 18: upper middle income
    ## 19:         high income
    ## 20:         high income
    ## 21:                <NA>
    ## 22:                <NA>
    ## 23: lower middle income
    ##                   LMH14

``` r
naphy <- edit[, c("location_name", "phy09","phy10", "phy11", "phy12", "phy13", "phy14", "phy15", "phy16", "phy17")]
naphy #physican columns 09-17
```

    ##                 location_name  phy09  phy10  phy11  phy12  phy13  phy14  phy15
    ##  1:                   Algeria     NA 1.2070     NA     NA     NA     NA     NA
    ##  2:                   Andorra 3.1479 4.0000     NA     NA     NA     NA 3.3333
    ##  3:                 Argentina     NA 3.2100     NA     NA 3.9385     NA     NA
    ##  4:                   Armenia 2.7679 2.8419 2.8677 2.8968 2.9031 2.8928 2.9143
    ##  5:                 Australia 3.1085 3.3429 3.2878 3.2858 3.3530 3.4314 3.4886
    ##  6:                Azerbaijan 3.6751 3.6629 3.4376 3.4901 3.4558 3.4460     NA
    ##  7:                   Belarus 3.2845 3.2498 4.7611 4.8352 4.8534 5.0120 5.1905
    ##  8:                    Brazil 1.8171 1.8139 1.8491     NA 1.8820     NA     NA
    ##  9:                  Bulgaria 3.7401 3.7661 3.8467 3.8995 3.9630 3.9750 4.0332
    ## 10:              Burkina Faso 0.0446 0.0457 0.0386 0.0475 0.0470 0.0487 0.0657
    ## 11:                    Canada     NA 2.0384     NA 2.3962 2.4509 2.4961 2.5388
    ## 12:                     Chile 1.0294 1.4333 1.5854 1.7471 1.8806 2.0278 2.1470
    ## 13:                     China 1.3922 1.4532 1.4637 1.5367 1.6332 1.6877 1.7732
    ## 14:                  Colombia 1.5668 1.5917 1.6966 1.7400 1.8431 1.8534 1.9742
    ## 15:                    Cyprus 2.1064 2.1562 2.2484 2.2960 2.4076 2.4993 2.6236
    ## 16:                   Ecuador 1.5983 2.1111 1.6582     NA     NA     NA 2.0648
    ## 17:                   Estonia 3.2767 3.2422 3.2929 3.2822 3.3318 3.3564 3.4152
    ## 18:                  Ethiopia 0.0252 0.0220     NA     NA     NA     NA     NA
    ## 19:                   Finland 3.0861 3.2653 3.1221 3.2854 3.2966 3.3922 3.3309
    ## 20:                    France 3.3693 3.3736 3.1622 3.1745 3.1948 3.2115 3.2239
    ## 21:                   Georgia 4.2219 4.4466 4.5004 4.4868 4.5145 4.7754 5.0055
    ## 22:                   Germany 3.6638 3.7567 3.8491 3.9197 4.0087 4.0846 4.1342
    ## 23:                     Ghana 0.0841 0.0938 0.0979 0.0956 0.1686     NA     NA
    ## 24:                     Haiti     NA     NA 0.1378     NA     NA     NA 0.0852
    ## 25:                   Hungary 3.0399 2.8896 2.9811 3.1062 3.2284 3.3443 3.1178
    ## 26:                     India 0.6220 0.6616 0.7376 0.6982 0.7182 0.7247     NA
    ## 27:                 Indonesia 0.1448 0.1395     NA 0.3075 0.3118     NA 0.2738
    ## 28: Iran, Islamic Republic of     NA 0.8900     NA     NA     NA 1.5044 1.1526
    ## 29:                      Iraq     NA 0.6636     NA     NA     NA 1.0364     NA
    ## 30:                     Japan     NA 2.2059     NA 2.2740     NA 2.3413     NA
    ## 31:                    Jordan 2.1287 2.2326     NA     NA 2.1947 2.2038 2.8078
    ## 32:                Kazakhstan 3.7808 3.9278 3.9483 3.9108 3.9508 3.9800     NA
    ## 33:       Korea (Republic of) 1.9185 1.9839 2.0361 2.0798 2.1632 2.2070 2.2494
    ## 34:                    Kuwait 1.8929 2.4296 2.4560 2.4662 2.5332 2.6522 2.6463
    ## 35:                Kyrgyzstan     NA     NA     NA     NA     NA     NA     NA
    ## 36:                   Lebanon     NA 2.3385 2.6201     NA 2.2486 2.0735 2.1030
    ## 37:                     Libya 1.9578 1.9000     NA     NA     NA 2.0583 1.8943
    ## 38:                  Malaysia     NA 1.1691 1.2777 1.3320     NA     NA 1.5358
    ## 39:                      Mali 0.0923 0.1021 0.1037 0.1071     NA     NA     NA
    ## 40:                    Mexico 1.9790 2.2362 2.1004 2.1118 2.1568 2.2070 2.3246
    ## 41:      Moldova, Republic of 2.3912 2.3808 2.4142 2.4135 2.5028 2.4746 2.4836
    ## 42:                   Morocco 0.6477 0.6200     NA     NA 0.6320 0.9135     NA
    ## 43:               Netherlands 2.9057 2.9516 3.1243 3.2463 3.3058 3.3468 3.4917
    ## 44:               New Zealand 3.0477 3.0658 3.1731 3.1957 3.2303 3.2374 3.2961
    ## 45:                   Nigeria 0.3782 0.1836     NA     NA 0.3828     NA     NA
    ## 46:                    Norway 2.4756 2.4949 2.5441 2.5532 2.5991 2.6832 2.6474
    ## 47:                  Pakistan 0.7951 0.8076 0.8311 0.8590 0.8771 0.8972 0.9262
    ## 48:       Palestine, State of     NA     NA     NA     NA     NA     NA     NA
    ## 49:                      Peru 0.9472 0.9200     NA 1.1411     NA     NA     NA
    ## 50:               Philippines 1.2632 1.2717     NA     NA     NA     NA     NA
    ## 51:                    Poland 2.1593 2.1707 2.1997 2.2242 2.2340 2.3020 2.3252
    ## 52:                     Qatar 3.0975 3.7273     NA     NA     NA 1.7351     NA
    ## 53:                   Romania 2.4414 2.4804 2.5153 2.5887 2.6240     NA     NA
    ## 54:        Russian Federation 2.3924 2.3930 6.6305 4.1303 4.0705 4.0114 3.7494
    ## 55:                    Rwanda 0.0550 0.0550 0.0129     NA 0.1021 0.0909 0.1129
    ## 56:                    Serbia 2.4467 2.4820 2.5003 2.4961 2.4945 2.4611 2.4603
    ## 57:                 Singapore 1.6758 1.7187 1.7232 1.7964 1.8958 2.0125     NA
    ## 58:                  Slovenia 2.4167 2.4367 2.4965 2.5406 2.6252 2.7628 2.8148
    ## 59:              South Africa 0.7074 0.7341 0.7179 0.7254 0.7422 0.7541 0.7814
    ## 60:                     Spain 3.0395 3.0292 3.0473 3.1396 3.1425 3.1657 3.2166
    ## 61:                    Sweden 3.8065 3.8779 3.9569 4.0368 4.1259 4.2037 4.2856
    ## 62:               Switzerland 3.8476 3.8166 3.8355 3.9102 4.0303 4.1171 4.1898
    ## 63:                  Thailand 0.3387 0.3906     NA     NA     NA     NA 0.4651
    ## 64:       Trinidad and Tobago 1.5081 1.8086 1.8193     NA     NA     NA 2.6498
    ## 65:                   Tunisia     NA 1.2220     NA     NA     NA 1.2769 1.3108
    ## 66:                    Turkey 1.6635 1.7068 1.7160 1.7384 1.7619 1.7560 1.7988
    ## 67:                   Ukraine 3.4904 3.4830 3.4842 3.4910 3.4965 2.9923     NA
    ## 68:            United Kingdom 2.6279 2.6265 2.6603 2.6667 2.6774 2.7149 2.7465
    ## 69:             United States 2.4471 2.4354 2.4641 2.4985 2.5596 2.5740 2.5781
    ## 70:                   Uruguay     NA 3.7360     NA     NA     NA     NA     NA
    ## 71:                Uzbekistan 2.5706 2.5432 2.5036 2.4451 2.4044 2.3742     NA
    ## 72:                  Viet Nam     NA     NA     NA     NA     NA     NA     NA
    ## 73:                     Yemen     NA     NA     NA     NA     NA     NA     NA
    ## 74:                    Zambia 0.0606 0.0614 0.1649 0.1658     NA     NA     NA
    ## 75:                  Zimbabwe 0.1210 0.1272 0.0817 0.0807 0.0834 0.1240 0.1815
    ##                 location_name  phy09  phy10  phy11  phy12  phy13  phy14  phy15
    ##      phy16  phy17
    ##  1: 1.8325 1.7879
    ##  2:     NA     NA
    ##  3: 4.0013 3.9901
    ##  4:     NA 4.4023
    ##  5: 3.5672 3.6778
    ##  6:     NA     NA
    ##  7:     NA     NA
    ##  8:     NA 2.1652
    ##  9:     NA     NA
    ## 10: 0.0645 0.0847
    ## 11: 2.3105 2.6102
    ## 12: 2.2937 2.4411
    ## 13: 1.8647 1.9798
    ## 14: 2.0335 2.1064
    ## 15: 1.9509     NA
    ## 16: 2.0368     NA
    ## 17: 3.4546 3.4629
    ## 18: 0.0464 0.0986
    ## 19: 3.8118     NA
    ## 20: 3.2376 3.2565
    ## 21: 5.9973 6.1297
    ## 22: 4.1944 4.2488
    ## 23: 0.1270 0.1359
    ## 24:     NA     NA
    ## 25: 3.2313 3.3447
    ## 26: 0.7590 0.7779
    ## 27:     NA 0.3767
    ## 28:     NA 1.1292
    ## 29: 0.8536 0.8375
    ## 30: 2.4115     NA
    ## 31: 1.3954 2.3237
    ## 32:     NA     NA
    ## 33: 2.3037 2.3608
    ## 34:     NA     NA
    ## 35:     NA     NA
    ## 36: 2.0115 2.0255
    ## 37:     NA 2.0905
    ## 38:     NA     NA
    ## 39: 0.1395     NA
    ## 40: 2.3244 2.3827
    ## 41:     NA 3.2066
    ## 42:     NA 0.7308
    ## 43: 3.5470 3.6054
    ## 44: 3.3833 3.4728
    ## 45: 0.4494     NA
    ## 46: 2.6983 2.8342
    ## 47: 0.9620 1.0005
    ## 48:     NA     NA
    ## 49: 1.3048     NA
    ## 50:     NA 0.6004
    ## 51: 2.4146 2.3788
    ## 52: 2.6944 0.0008
    ## 53: 2.2565 2.9807
    ## 54: 4.0139     NA
    ## 55: 0.1226 0.1371
    ## 56: 3.1131     NA
    ## 57: 2.2936     NA
    ## 58: 3.0007 3.0861
    ## 59: 0.7997 0.9054
    ## 60: 3.8112 3.8723
    ## 61: 3.9840     NA
    ## 62: 4.2473 4.2957
    ## 63: 0.4450 0.8075
    ## 64:     NA 3.3646
    ## 65: 1.2834 1.3025
    ## 66: 1.8142 1.8492
    ## 67:     NA     NA
    ## 68: 2.7563 2.7863
    ## 69: 2.5881 2.6120
    ## 70: 3.9558 5.0794
    ## 71:     NA     NA
    ## 72:     NA     NA
    ## 73:     NA     NA
    ## 74: 0.1628     NA
    ## 75: 0.1788 0.1859
    ##      phy16  phy17

``` r
na.omit(naphy, c("phy14"), invert = TRUE) #able to see if can replace values
```

    ##           location_name  phy09  phy10  phy11  phy12  phy13 phy14  phy15  phy16
    ##  1:             Algeria     NA 1.2070     NA     NA     NA    NA     NA 1.8325
    ##  2:             Andorra 3.1479 4.0000     NA     NA     NA    NA 3.3333     NA
    ##  3:           Argentina     NA 3.2100     NA     NA 3.9385    NA     NA 4.0013
    ##  4:              Brazil 1.8171 1.8139 1.8491     NA 1.8820    NA     NA     NA
    ##  5:             Ecuador 1.5983 2.1111 1.6582     NA     NA    NA 2.0648 2.0368
    ##  6:            Ethiopia 0.0252 0.0220     NA     NA     NA    NA     NA 0.0464
    ##  7:               Ghana 0.0841 0.0938 0.0979 0.0956 0.1686    NA     NA 0.1270
    ##  8:               Haiti     NA     NA 0.1378     NA     NA    NA 0.0852     NA
    ##  9:           Indonesia 0.1448 0.1395     NA 0.3075 0.3118    NA 0.2738     NA
    ## 10:          Kyrgyzstan     NA     NA     NA     NA     NA    NA     NA     NA
    ## 11:            Malaysia     NA 1.1691 1.2777 1.3320     NA    NA 1.5358     NA
    ## 12:                Mali 0.0923 0.1021 0.1037 0.1071     NA    NA     NA 0.1395
    ## 13:             Nigeria 0.3782 0.1836     NA     NA 0.3828    NA     NA 0.4494
    ## 14: Palestine, State of     NA     NA     NA     NA     NA    NA     NA     NA
    ## 15:                Peru 0.9472 0.9200     NA 1.1411     NA    NA     NA 1.3048
    ## 16:         Philippines 1.2632 1.2717     NA     NA     NA    NA     NA     NA
    ## 17:             Romania 2.4414 2.4804 2.5153 2.5887 2.6240    NA     NA 2.2565
    ## 18:            Thailand 0.3387 0.3906     NA     NA     NA    NA 0.4651 0.4450
    ## 19: Trinidad and Tobago 1.5081 1.8086 1.8193     NA     NA    NA 2.6498     NA
    ## 20:             Uruguay     NA 3.7360     NA     NA     NA    NA     NA 3.9558
    ## 21:            Viet Nam     NA     NA     NA     NA     NA    NA     NA     NA
    ## 22:               Yemen     NA     NA     NA     NA     NA    NA     NA     NA
    ## 23:              Zambia 0.0606 0.0614 0.1649 0.1658     NA    NA     NA 0.1628
    ##           location_name  phy09  phy10  phy11  phy12  phy13 phy14  phy15  phy16
    ##      phy17
    ##  1: 1.7879
    ##  2:     NA
    ##  3: 3.9901
    ##  4: 2.1652
    ##  5:     NA
    ##  6: 0.0986
    ##  7: 0.1359
    ##  8:     NA
    ##  9: 0.3767
    ## 10:     NA
    ## 11:     NA
    ## 12:     NA
    ## 13:     NA
    ## 14:     NA
    ## 15:     NA
    ## 16: 0.6004
    ## 17: 2.9807
    ## 18: 0.8075
    ## 19: 3.3646
    ## 20: 5.0794
    ## 21:     NA
    ## 22:     NA
    ## 23:     NA
    ##      phy17

REPLACE

Algeria -2016 Andorra - 2015 Argentina - 2013 Brazil - 2013 Ecuador -
2015 Ethiopia - 2016 Ghana - 2013 Haiti - 2015 Indonesia - 2015
Kyrgyzsta - NA Malaysia - 2015 Mali - 2016 Nigeria - 2013 Palestine,
State of -NA Peru - 2016  
Philippines - 2017 Romania - 2013 Thailand - 2015  
Trinidad and Tobago - 2015 Uruguay - 2016 Viet Nam - NA Yemen - NA
Zambia - 2016

``` r
#need to create table with missing phy14 values in one column the can use rows_patch to insert new values

Algeriaphy <- edit["Algeria", c("location_name", "phy16")]
Andorraphy <- edit["Andorra", c("location_name", "phy15")]
Argentinaphy <- edit["Argentina", c("location_name", "phy13")]
Brazilphy <- edit["Brazil", c("location_name", "phy13")]
Ecuadorphy <- edit["Ecuador", c("location_name", "phy15")]
Ethiopiaphy <- edit["Ethiopia", c("location_name", "phy16")]
Ghanaphy <- edit["Ghana", c("location_name", "phy13")]  
Haitiphy <- edit["Haiti", c("location_name", "phy15")]
Indonesiaphy    <- edit["Indonesia", c("location_name", "phy15")]
#Kyrgyzsta - NA
Malaysiaphy <- edit["Malaysia", c("location_name", "phy15")]
Maliphy <- edit["Mali", c("location_name", "phy16")]
Nigeriaphy      <- edit["Nigeria", c("location_name", "phy13")]
#Palestine, State of        -NA
Peruphy <- edit["Peru", c("location_name", "phy16")]    
Philippinesphy <- edit["Philippines", c("location_name", "phy17")]
Romaniaphy  <- edit["Romania", c("location_name", "phy13")]
Thailandphy <- edit["Thailand", c("location_name", "phy15")]
TrinidadandTobagophy    <- edit["Trinidad and Tobago", c("location_name", "phy15")]
Uruguayphy <- edit["Uruguay", c("location_name", "phy16")]  
#Viet Nam   - NA
#Yemen  - NA
Zambiaphy <-    edit["Zambia", c("location_name", "phy16")] 


phy14edit <- rbind(Algeriaphy, Andorraphy, Argentinaphy, Brazilphy, Ecuadorphy, Ethiopiaphy, Ghanaphy, Haitiphy, Indonesiaphy, Malaysiaphy, Maliphy, Nigeriaphy, Peruphy, Philippinesphy, Romaniaphy, Thailandphy, TrinidadandTobagophy, Uruguayphy,Zambiaphy,   use.names=FALSE)
setnames(phy14edit, "phy16", "phy14edit") #new table with values to insert into the phy14 column
```

``` r
setnames(edit, "phy14", "phy14edit")

edit_phy14 <- rows_patch(edit, phy14edit)
```

    ## Matching, by = "location_name"

``` r
edit_phy14[,c("location_name", "phy14edit")]
```

    ##                 location_name phy14edit
    ##  1:                   Algeria    1.8325
    ##  2:                   Andorra    3.3333
    ##  3:                 Argentina    3.9385
    ##  4:                   Armenia    2.8928
    ##  5:                 Australia    3.4314
    ##  6:                Azerbaijan    3.4460
    ##  7:                   Belarus    5.0120
    ##  8:                    Brazil    1.8820
    ##  9:                  Bulgaria    3.9750
    ## 10:              Burkina Faso    0.0487
    ## 11:                    Canada    2.4961
    ## 12:                     Chile    2.0278
    ## 13:                     China    1.6877
    ## 14:                  Colombia    1.8534
    ## 15:                    Cyprus    2.4993
    ## 16:                   Ecuador    2.0648
    ## 17:                   Estonia    3.3564
    ## 18:                  Ethiopia    0.0464
    ## 19:                   Finland    3.3922
    ## 20:                    France    3.2115
    ## 21:                   Georgia    4.7754
    ## 22:                   Germany    4.0846
    ## 23:                     Ghana    0.1686
    ## 24:                     Haiti    0.0852
    ## 25:                   Hungary    3.3443
    ## 26:                     India    0.7247
    ## 27:                 Indonesia    0.2738
    ## 28: Iran, Islamic Republic of    1.5044
    ## 29:                      Iraq    1.0364
    ## 30:                     Japan    2.3413
    ## 31:                    Jordan    2.2038
    ## 32:                Kazakhstan    3.9800
    ## 33:       Korea (Republic of)    2.2070
    ## 34:                    Kuwait    2.6522
    ## 35:                Kyrgyzstan        NA
    ## 36:                   Lebanon    2.0735
    ## 37:                     Libya    2.0583
    ## 38:                  Malaysia    1.5358
    ## 39:                      Mali    0.1395
    ## 40:                    Mexico    2.2070
    ## 41:      Moldova, Republic of    2.4746
    ## 42:                   Morocco    0.9135
    ## 43:               Netherlands    3.3468
    ## 44:               New Zealand    3.2374
    ## 45:                   Nigeria    0.3828
    ## 46:                    Norway    2.6832
    ## 47:                  Pakistan    0.8972
    ## 48:       Palestine, State of        NA
    ## 49:                      Peru    1.3048
    ## 50:               Philippines    0.6004
    ## 51:                    Poland    2.3020
    ## 52:                     Qatar    1.7351
    ## 53:                   Romania    2.6240
    ## 54:        Russian Federation    4.0114
    ## 55:                    Rwanda    0.0909
    ## 56:                    Serbia    2.4611
    ## 57:                 Singapore    2.0125
    ## 58:                  Slovenia    2.7628
    ## 59:              South Africa    0.7541
    ## 60:                     Spain    3.1657
    ## 61:                    Sweden    4.2037
    ## 62:               Switzerland    4.1171
    ## 63:                  Thailand    0.4651
    ## 64:       Trinidad and Tobago    2.6498
    ## 65:                   Tunisia    1.2769
    ## 66:                    Turkey    1.7560
    ## 67:                   Ukraine    2.9923
    ## 68:            United Kingdom    2.7149
    ## 69:             United States    2.5740
    ## 70:                   Uruguay    3.9558
    ## 71:                Uzbekistan    2.3742
    ## 72:                  Viet Nam        NA
    ## 73:                     Yemen        NA
    ## 74:                    Zambia    0.1628
    ## 75:                  Zimbabwe    0.1240
    ##                 location_name phy14edit

``` r
na.omit(edit_phy14, "phy14edit", invert=TRUE)
```

    ##          location_name GSNI_PERIOD onebias twobias nobias political economic
    ## 1:          Kyrgyzstan   2010–2014   96.73   84.87   3.27      76.8    71.53
    ## 2: Palestine, State of   2010–2014   98.00   92.30   2.00      89.3    79.50
    ## 3:            Viet Nam   2005–2009   92.89   69.17   7.11      59.4    62.49
    ## 4:               Yemen   2010–2014   97.80   92.10   2.20      87.4    87.20
    ##    educational physical cvd05fem cvd06fem cvd07fem cvd08fem cvd09fem cvd10fem
    ## 1:       41.00    81.73 546.8872 553.7142 543.2533 536.8522 507.4175 488.1873
    ## 2:       26.70    83.50 367.3750 362.0774 355.8204 350.5859 343.9703 341.7886
    ## 3:       20.36    70.56 256.6274 256.6231 255.9745 255.1929 254.4622 252.5587
    ## 4:       45.30    81.00 493.0600 489.2440 485.1008 483.3384 478.3407 472.1759
    ##    cvd11fem cvd12fem cvd13fem cvd14fem cvd15fem cvd16fem cvd17fem cvd18fem
    ## 1: 483.2938 465.0509 442.7975 429.3656 435.0685 415.0933 408.2852 395.2001
    ## 2: 337.6966 320.1783 314.6376 323.3969 338.5276 350.9355 354.1550 349.6899
    ## 3: 250.1793 247.0552 243.0562 239.2128 234.9403 230.6141 226.8228 223.3228
    ## 4: 471.3592 468.9990 467.1613 464.4741 466.8673 466.8844 469.9199 473.8849
    ##    cvd19fem cvd05male cvd06male cvd07male cvd08male cvd09male cvd10male
    ## 1: 390.5737  748.0504  780.9492  781.9687  761.4547  734.8094  702.9963
    ## 2: 345.6669  524.2705  515.4991  498.1858  484.7670  474.6089  468.5019
    ## 3: 219.5527  448.4061  449.7884  453.6026  455.8597  457.1055  456.0361
    ## 4: 476.9648  579.9540  575.3879  568.8200  566.4953  558.3360  547.3893
    ##    cvd11male cvd12male cvd13male cvd14male cvd15male cvd16male cvd17male
    ## 1:  649.3122  632.0656  609.2108  620.6342  644.2163  618.5638  613.3843
    ## 2:  460.4963  432.5445  413.7575  433.8364  442.4321  433.5059  438.1050
    ## 3:  453.6313  450.9162  449.0089  446.5765  443.7173  440.1349  436.8855
    ## 4:  545.5135  541.5427  538.4431  532.5702  536.2482  536.1051  541.0473
    ##    cvd18male cvd19male cvd05both cvd06both cvd07both cvd08both cvd09both
    ## 1:  580.3176  569.3491  634.0089  651.1682  645.5907  632.7615  603.3141
    ## 2:  440.8897  433.9799  434.1983  427.1939  415.8933  407.1588  399.0904
    ## 3:  433.6069  429.9362  334.7621  335.3238  336.5239  337.0268  337.1512
    ## 4:  547.2489  550.6548  535.1088  530.9694  525.7088  523.7193  517.1861
    ##    cvd10both cvd11both cvd12both cvd13both cvd14both cvd15both cvd16both
    ## 1:  581.8522  562.6225  543.9251  519.5371  513.0161  523.1598  500.1438
    ## 2:  394.3262  387.9774  366.8452  355.9001  368.8565  380.8886  384.4684
    ## 3:  335.4539  333.0107  329.9312  326.6030  323.1828  319.1363  314.9546
    ## 4:  508.7890  507.4918  504.3873  501.9957  497.8142  500.9039  500.8572
    ##    cvd17both cvd18both cvd19both LEbirth2000 LE602000 LEbirth2010 LE602010
    ## 1:  493.8208  473.2671  466.3130        70.2     18.3        73.3     19.3
    ## 2:  388.1755  386.6795  381.6978          NA       NA          NA       NA
    ## 3:  311.2375  307.7080  303.9047        75.6     21.0        77.1     21.5
    ## 4:  504.8275  509.8769  513.0910        64.7     17.9        69.6     18.7
    ##    LEbirth2015 LE602015 LEbirth2019 LE602019 phy05 phy06 phy07 phy08 phy09
    ## 1:       75.30     20.2        77.3     21.7    NA    NA    NA    NA    NA
    ## 2:       76.22       NA          NA       NA    NA    NA    NA    NA    NA
    ## 3:       77.60     21.8        78.1     22.0    NA    NA    NA    NA    NA
    ## 4:       69.60     18.9        68.9     18.7    NA    NA    NA    NA    NA
    ##    phy10 phy11 phy12 phy13 phy14edit phy15 phy16 phy17 HE05 HE06 HE07 HE08 HE09
    ## 1:    NA    NA    NA    NA        NA    NA    NA    NA <NA> <NA> <NA> <NA> <NA>
    ## 2:    NA    NA    NA    NA        NA    NA    NA    NA <NA> <NA> <NA> <NA> <NA>
    ## 3:    NA    NA    NA    NA        NA    NA    NA    NA <NA> <NA> <NA> <NA> <NA>
    ## 4:    NA    NA    NA    NA        NA    NA    NA    NA <NA> <NA> <NA> <NA> <NA>
    ##    HE10 HE11 HE12 HE13 HE14 HE15 HE16 HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ## 1: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>  10.2  10.2  10.3  10.3  10.4  10.6
    ## 2: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>    NA    NA    NA    NA    NA    NA
    ## 3: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>    NA    NA    NA    NA    NA    NA
    ## 4: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>   1.9   2.0   2.2   2.3   2.5   2.6
    ##    scl11 scl12 scl13 scl14 scl15 scl16 scl17 GDP05 GDP06 GDP07 GDP08 GDP09
    ## 1:  10.6  10.7  10.7  10.8  10.8  10.9  10.9    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:   2.8   3.0   3.0   3.0   3.0   3.0   3.0    NA    NA    NA    NA    NA
    ##    GDP10 GDP11 GDP12 GDP13 GDP14 GDP15 GDP16 GDP17 MMR05 MMR06 MMR07 MMR08
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##    MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17 LMH09 LMH14
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>

``` r
#table "edit_phy14" now has column "phy14edit" which has inserted values from other years
# 4 NAs in total for phy
```

\#\#\#\#scl14

``` r
na.omit(edit_phy14, c("scl14"), invert = TRUE) # 6 missing scl 2014
```

    ##                location_name GSNI_PERIOD onebias twobias nobias political
    ## 1: Iran, Islamic Republic of   2005–2009   98.54   92.49   1.46     84.63
    ## 2:       Korea (Republic of)   2010–2014   87.07   62.91  12.93     63.68
    ## 3:      Moldova, Republic of   2005–2009   90.06   67.21   9.94     60.33
    ## 4:       Palestine, State of   2010–2014   98.00   92.30   2.00     89.30
    ## 5:        Russian Federation   2010–2014   86.83   68.56  13.17     68.43
    ## 6:                  Viet Nam   2005–2009   92.89   69.17   7.11     59.40
    ##    economic educational physical cvd05fem cvd06fem cvd07fem cvd08fem cvd09fem
    ## 1:    88.86       55.42    78.69 346.4549 333.0393 318.5381 305.9814 298.0127
    ## 2:    54.33       25.67    58.27 155.9141 143.8054 132.6713 120.8555 111.7862
    ## 3:    58.80       16.73    65.20 513.3270 491.9931 497.1151 484.5283 467.9101
    ## 4:    79.50       26.70    83.50 367.3750 362.0774 355.8204 350.5859 343.9703
    ## 5:    58.77       22.66    50.02 569.5149 527.8242 500.5882 492.7038 467.5455
    ## 6:    62.49       20.36    70.56 256.6274 256.6231 255.9745 255.1929 254.4622
    ##    cvd10fem cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem  cvd16fem
    ## 1: 290.3670 283.4457 278.28489 277.07263 275.18836 277.67423 278.51259
    ## 2: 105.3449 100.1279  95.58209  90.21631  85.58348  83.53081  81.91117
    ## 3: 459.1067 400.9103 387.36233 372.28763 377.82255 382.27748 381.40961
    ## 4: 341.7886 337.6966 320.17827 314.63758 323.39695 338.52757 350.93555
    ## 5: 459.6649 424.4358 406.44126 391.62857 386.28781 376.79609 366.11070
    ## 6: 252.5587 250.1793 247.05521 243.05616 239.21285 234.94030 230.61406
    ##     cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ## 1: 277.88105 271.04348 268.72894  400.2154  389.5281  376.5086  362.0445
    ## 2:  81.88305  82.38176  83.11824  190.0032  178.2896  167.8587  158.2836
    ## 3: 368.12912 361.05127 341.11337  702.8658  671.8424  666.6718  648.2151
    ## 4: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 5: 348.59567 349.51106 351.22730  956.8934  876.9782  830.0730  825.6957
    ## 6: 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026  455.8597
    ##    cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ## 1:  350.5229  338.1660  323.1285  311.8184  304.8604  299.1565  300.1241
    ## 2:  149.4912  143.7630  137.6913  131.7820  123.9082  117.5575  114.1778
    ## 3:  638.1296  641.1604  572.9933  565.3869  534.3178  552.1560  587.3132
    ## 4:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 5:  776.4243  770.5636  706.6609  675.9903  650.0326  646.2977  623.4263
    ## 6:  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765  443.7173
    ##    cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ## 1:  297.2979  294.9042  290.2879  288.5816  374.2741  361.9923  347.9814
    ## 2:  110.9073  110.6175  108.2689  108.8687  172.0355  160.0969  148.9444
    ## 3:  566.6405  523.8055  511.8000  498.5963  589.7769  564.1553  566.4516
    ## 4:  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939  415.8933
    ## 5:  604.2681  561.3748  549.7797  549.1739  727.6035  670.5814  635.1627
    ## 6:  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238  336.5239
    ##    cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ## 1:  334.1940  324.1761  313.9673  302.7896  294.4027  290.2060  286.3140
    ## 2:  137.6671  128.5412  122.2008  116.6089  111.6049  105.3768  100.1378
    ## 3:  552.6234  539.0321  535.4923  471.5840  460.0871  438.8196  449.0384
    ## 4:  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001  368.8565
    ## 5:  628.2161  592.7751  585.0586  537.7079  514.5833  495.2656  490.3853
    ## 6:  337.0268  337.1512  335.4539  333.0107  329.9312  326.6030  323.1828
    ##    cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ## 1: 287.96550 286.88535 285.32918 279.68642  277.7336        74.6     20.5
    ## 2:  97.49873  95.22265  95.17711  94.68896   95.4053        79.8     22.7
    ## 3: 463.27943 456.27984 432.82716 423.77893  404.9551        70.5     17.2
    ## 4: 380.88863 384.46844 388.17552 386.67955  381.6978          NA       NA
    ## 5: 475.65621 461.71309 434.85592 431.69136  432.9186        72.3     18.7
    ## 6: 319.13626 314.95461 311.23748 307.70802  303.9047        75.6     21.0
    ##    LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05  phy06
    ## 1:        78.0     22.3       78.50     22.3        79.1     22.5 0.8869 0.5361
    ## 2:        83.8     25.9       85.10     26.9        86.1     27.9 1.7529 1.8047
    ## 3:        73.1     18.2       75.00     19.5        77.1     20.9 2.3783 2.3768
    ## 4:          NA       NA       76.22       NA          NA       NA     NA     NA
    ## 5:        74.7     20.2       76.60     21.4        78.0     22.2 2.3204 2.3721
    ## 6:        77.1     21.5       77.60     21.8        78.1     22.0     NA     NA
    ##     phy07  phy08  phy09  phy10  phy11  phy12  phy13 phy14edit  phy15  phy16
    ## 1:     NA     NA     NA 0.8900     NA     NA     NA    1.5044 1.1526     NA
    ## 2: 1.8655 1.8407 1.9185 1.9839 2.0361 2.0798 2.1632    2.2070 2.2494 2.3037
    ## 3: 2.3871 2.3545 2.3912 2.3808 2.4142 2.4135 2.5028    2.4746 2.4836     NA
    ## 4:     NA     NA     NA     NA     NA     NA     NA        NA     NA     NA
    ## 5: 2.3841 2.3812 2.3924 2.3930 6.6305 4.1303 4.0705    4.0114 3.7494 4.0139
    ## 6:     NA     NA     NA     NA     NA     NA     NA        NA     NA     NA
    ##     phy17               HE05               HE06               HE07
    ## 1: 1.1292         5.30572176 5.1986260399999997         5.03977919
    ## 2: 2.3608 4.6178216900000004 4.9439678200000001 5.1149072599999998
    ## 3: 3.2066 7.9639606499999998 8.7857027100000007 9.1826009800000001
    ## 4:     NA               <NA>               <NA>               <NA>
    ## 5:     NA         4.76693487         4.76170969 4.7431106600000001
    ## 6:     NA               <NA>               <NA>               <NA>
    ##                  HE08               HE09               HE10               HE11
    ## 1: 5.2816843999999996 6.5595455200000004 6.7547311800000003 6.6072506899999999
    ## 2:         5.39896727 5.7822899799999998 5.9173440900000003         6.00844383
    ## 3: 9.1230525999999994        11.39545822 10.131128309999999 9.0967035299999992
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5: 4.8986678100000001 5.6382026700000001 4.9660777999999999 4.7900524100000004
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ##                  HE12               HE13               HE14               HE15
    ## 1: 6.6364860500000002         5.99379873 6.9135108000000001 7.7605791100000001
    ## 2:         6.13262033 6.2478942899999996 6.4743809700000003 6.6527166400000004
    ## 3: 9.1396207799999996 8.6830034299999994 8.6327571899999995 8.5576353100000002
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5: 4.9408035300000002 5.0798091899999998 5.1802287099999997 5.2956042300000004
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ##                  HE16               HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ## 1: 8.8595066100000004 8.6596641499999993    NA    NA    NA    NA    NA    NA
    ## 2: 6.9143271400000001         7.10694933    NA    NA    NA    NA    NA    NA
    ## 3: 7.5355224600000001 7.0132851599999997    NA    NA    NA    NA    NA    NA
    ## 4:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 5: 5.2652196900000003         5.34388065    NA    NA    NA    NA    NA    NA
    ## 6:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ##    scl11 scl12 scl13 scl14 scl15 scl16 scl17    GDP05    GDP06    GDP07
    ## 1:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ## 5:    NA    NA    NA    NA    NA    NA    NA 5323.463 6920.189 9101.255
    ## 6:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ##       GDP08    GDP09 GDP10    GDP11    GDP12    GDP13    GDP14    GDP15
    ## 1:       NA       NA    NA       NA       NA       NA       NA       NA
    ## 2:       NA       NA    NA       NA       NA       NA       NA       NA
    ## 3:       NA       NA    NA       NA       NA       NA       NA       NA
    ## 4:       NA       NA    NA       NA       NA       NA       NA       NA
    ## 5: 11635.27 8562.813 10675 14311.08 15420.87 15974.64 14095.65 9313.014
    ## 6:       NA       NA    NA       NA       NA       NA       NA       NA
    ##       GDP16    GDP17 MMR05 MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13
    ## 1:       NA       NA    34    32    30    28    25    22    19    18    17
    ## 2:       NA       NA    15    14    15    15    16    15    14    13    13
    ## 3:       NA       NA    34    31    31    29    28    29    21    22    21
    ## 4:       NA       NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5: 8704.898 10720.33    42    36    32    30    27    25    23    22    20
    ## 6:       NA       NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##    MMR14 MMR15 MMR16 MMR17               LMH09               LMH14
    ## 1:    17    17    16    16 upper middle income upper middle income
    ## 2:    12    12    11    11         high income         high income
    ## 3:    23    22    20    19 lower middle income lower middle income
    ## 4:    NA    NA    NA    NA                <NA>                <NA>
    ## 5:    19    18    18    17 upper middle income         high income
    ## 6:    NA    NA    NA    NA                <NA>                <NA>

``` r
nascl <- edit_phy14[, c("location_name", "scl05", "scl06", "scl07", "scl08", "scl09","scl10", "scl11", "scl12", "scl13", "scl14", "scl15", "scl16", "scl17")]
nascl #scl columns 09-17
```

    ##                 location_name scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12
    ##  1:                   Algeria   6.9   7.0   7.2   6.7   6.9   7.1   7.4   7.6
    ##  2:                   Andorra   9.8  10.1  10.1  10.1  10.1  10.1  10.2  10.2
    ##  3:                 Argentina   9.1   9.0   8.9   8.9   9.3   9.8   9.8   9.8
    ##  4:                   Armenia  10.9  10.9  11.0  11.0  11.1  11.1  11.2  11.3
    ##  5:                 Australia  11.7  11.9  12.0  12.3  12.3  12.4  12.5  12.6
    ##  6:                Azerbaijan  10.7  10.7  10.2  10.2  10.7  10.7  10.7  10.7
    ##  7:                   Belarus   9.3  10.0  10.6  11.3  11.9  12.0  12.0  12.0
    ##  8:                    Brazil   6.3   6.4   6.5   6.7   6.8   6.9   7.1   7.3
    ##  9:                  Bulgaria  10.2  10.4  10.5  10.5  10.6  10.6  10.7  10.8
    ## 10:              Burkina Faso   1.3   1.3   1.3   1.3   1.4   1.4   1.4   1.4
    ## 11:                    Canada  12.2  12.3  12.4  12.4  12.5  12.6  12.7  12.8
    ## 12:                     Chile   9.5   9.5   9.4   9.9   9.9   9.8   9.8   9.9
    ## 13:                     China   6.9   6.9   7.0   7.0   7.1   7.3   7.4   7.5
    ## 14:                  Colombia   6.8   6.7   7.2   7.3   7.3   7.4   7.5   7.6
    ## 15:                    Cyprus  10.7  10.9  11.2  11.3  11.3  11.5  11.6  11.8
    ## 16:                   Ecuador   7.3   7.3   7.3   7.9   7.9   7.9   8.0   8.1
    ## 17:                   Estonia  12.1  12.2  12.2  12.3  12.4  12.5  12.5  12.5
    ## 18:                  Ethiopia   1.9   2.0   2.1   2.2   2.3   2.3   2.4   2.4
    ## 19:                   Finland  12.0  12.0  12.0  12.2  12.2  12.3  12.3  12.4
    ## 20:                    France  10.4  10.6  10.7  10.7  10.8  10.9  10.9  11.0
    ## 21:                   Georgia  12.1  12.1  12.1  12.2  12.2  12.2  12.2  12.5
    ## 22:                   Germany  13.3  13.6  13.7  13.7  13.8  13.8  13.9  14.0
    ## 23:                     Ghana   6.4   6.5   6.5   6.6   6.7   6.7   6.8   6.8
    ## 24:                     Haiti   4.3   4.4   4.5   4.6   4.7   4.7   4.8   4.9
    ## 25:                   Hungary  10.9  11.1  11.3  11.5  11.7  11.9  12.0  12.2
    ## 26:                     India   4.8   4.9   5.0   5.2   5.3   5.4   5.4   5.6
    ## 27:                 Indonesia   7.4   7.9   7.1   7.1   7.4   7.4   7.6   7.6
    ## 28: Iran, Islamic Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 29:                      Iraq   5.8   5.9   6.1   6.2   6.3   6.4   6.6   6.6
    ## 30:                     Japan  11.2  11.2  11.3  11.4  11.4  11.5  11.8  12.0
    ## 31:                    Jordan   9.7   9.8   9.8   9.8   9.9   9.8   9.9   9.9
    ## 32:                Kazakhstan  11.7  11.7  11.6  11.5  11.5  11.4  11.5  11.5
    ## 33:       Korea (Republic of)    NA    NA    NA    NA    NA    NA    NA    NA
    ## 34:                    Kuwait   5.8   6.2   6.3   6.5   6.6   6.8   7.0   7.2
    ## 35:                Kyrgyzstan  10.2  10.2  10.3  10.3  10.4  10.6  10.6  10.7
    ## 36:                   Lebanon   7.5   7.6   7.8   7.8   7.9   7.9   8.0   8.1
    ## 37:                     Libya   6.4   6.6   6.8   7.0   7.1   7.3   7.3   7.3
    ## 38:                  Malaysia   7.6   8.2   8.8   9.4   9.6   9.8  10.1  10.1
    ## 39:                      Mali   1.7   1.7   1.8   1.9   1.9   2.0   2.0   2.1
    ## 40:                    Mexico   7.6   8.0   8.0   8.0   8.2   8.0   8.4   8.6
    ## 41:      Moldova, Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 42:                   Morocco   3.9   4.0   4.0   4.1   4.2   4.2   4.4   4.6
    ## 43:               Netherlands  11.6  11.8  12.0  12.0  11.9  12.0  12.0  12.0
    ## 44:               New Zealand  11.7  11.7  11.8  11.8  11.9  12.0  12.0  12.1
    ## 45:                   Nigeria   5.2   5.2   5.2   5.2   5.2   5.2   5.5   5.7
    ## 46:                    Norway  12.4  12.5  12.6  12.7  12.7  12.7  12.8  12.6
    ## 47:                  Pakistan   4.5   4.4   4.4   4.4   4.5   4.7   4.8   4.8
    ## 48:       Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 49:                      Peru   8.7   8.1   8.1   8.4   8.4   8.4   9.1   8.6
    ## 50:               Philippines   8.7   8.8   8.9   9.0   9.0   8.9   9.0   9.1
    ## 51:                    Poland  11.6  11.7  11.8  11.9  12.1  12.2  12.3  11.7
    ## 52:                     Qatar   8.8   9.2   9.6   9.7   9.2   8.4   8.7   9.2
    ## 53:                   Romania  10.1  10.3  10.5  10.6  10.6  10.7  10.8  10.9
    ## 54:        Russian Federation    NA    NA    NA    NA    NA    NA    NA    NA
    ## 55:                    Rwanda   2.8   2.9   3.1   3.2   3.3   3.8   3.8   3.7
    ## 56:                    Serbia  10.2  10.2  10.2  10.4  10.4  10.4  10.6  10.5
    ## 57:                 Singapore  10.5  10.1  10.2  10.5  10.5  11.2  11.2  11.3
    ## 58:                  Slovenia  11.7  11.8  11.8  11.9  12.1  12.1  12.2  11.7
    ## 59:              South Africa   8.9   9.0   9.1   9.7  10.1  10.0   9.9   9.8
    ## 60:                     Spain   8.9   9.0   9.1   9.2   9.3   9.4   9.5   9.5
    ## 61:                    Sweden  12.4  12.4  12.5  12.2  12.2  12.3  12.4  12.4
    ## 62:               Switzerland  12.0  12.3  12.6  12.9  13.3  13.3  13.3  13.4
    ## 63:                  Thailand   7.0   7.0   7.1   7.3   7.5   7.7   7.5   7.7
    ## 64:       Trinidad and Tobago  10.0   9.9  10.5  10.7  10.7  10.7  10.8  10.8
    ## 65:                   Tunisia   5.8   5.9   6.1   6.3   6.4   6.7   6.9   6.8
    ## 66:                    Turkey   6.0   6.1   6.2   6.3   6.5   6.7   7.2   7.5
    ## 67:                   Ukraine  11.2  11.2  11.2  11.3  11.3  11.3  11.3  11.3
    ## 68:            United Kingdom  12.2  12.4  12.6  12.8  13.1  13.2  13.0  12.9
    ## 69:             United States  12.8  12.8  12.9  13.2  13.2  13.2  13.3  13.3
    ## 70:                   Uruguay   8.0   8.0   8.2   8.4   8.4   8.4   8.4   8.5
    ## 71:                Uzbekistan   9.8   9.9  10.1  10.3  10.5  10.7  10.9  11.1
    ## 72:                  Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA
    ## 73:                     Yemen   1.9   2.0   2.2   2.3   2.5   2.6   2.8   3.0
    ## 74:                    Zambia   6.3   6.4   6.4   6.5   6.5   6.6   6.7   6.7
    ## 75:                  Zimbabwe   6.8   6.8   7.0   7.0   7.2   7.3   7.3   7.9
    ##                 location_name scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12
    ##     scl13 scl14 scl15 scl16 scl17
    ##  1:   7.8   7.9   7.9   8.0   8.0
    ##  2:  10.2  10.2  10.2  10.2  10.2
    ##  3:   9.8   9.8   9.8   9.9   9.9
    ##  4:  11.4  11.5  11.6  11.7  11.7
    ##  5:  12.6  12.7  12.8  12.9  12.9
    ##  6:  10.8  10.7  10.7  10.7  10.7
    ##  7:  12.0  12.1  12.2  12.3  12.3
    ##  8:   7.4   7.4   7.6   7.8   7.8
    ##  9:  10.9  10.9  11.8  11.8  11.8
    ## 10:   1.4   1.4   1.4   1.5   1.5
    ## 11:  12.9  13.0  13.1  13.1  13.3
    ## 12:   9.9  10.1  10.3  10.3  10.3
    ## 13:   7.5   7.6   7.7   7.8   7.8
    ## 14:   7.8   8.0   8.1   8.3   8.3
    ## 15:  12.0  11.9  11.9  12.1  12.1
    ## 16:   8.3   8.5   8.4   8.7   8.7
    ## 17:  12.6  12.6  12.7  12.6  12.7
    ## 18:   2.5   2.5   2.6   2.7   2.7
    ## 19:  12.3  12.4  12.4  12.4  12.4
    ## 20:  11.2  11.4  11.5  11.5  11.5
    ## 21:  12.6  12.6  12.7  12.8  12.8
    ## 22:  14.0  14.0  14.1  14.1  14.1
    ## 23:   6.9   6.9   6.9   7.1   7.1
    ## 24:   5.0   5.1   5.2   5.2   5.3
    ## 25:  12.0  11.8  11.8  11.9  11.9
    ## 26:   5.8   6.1   6.3   6.4   6.4
    ## 27:   7.8   7.8   7.9   8.0   8.0
    ## 28:    NA    NA    NA    NA    NA
    ## 29:   6.6   6.6   6.6   6.7   6.8
    ## 30:  12.2  12.5  12.5  12.7  12.8
    ## 31:   9.9  10.1  10.3  10.4  10.4
    ## 32:  11.6  11.7  11.7  11.7  11.8
    ## 33:    NA    NA    NA    NA    NA
    ## 34:   6.7   6.9   7.1   7.2   7.3
    ## 35:  10.7  10.8  10.8  10.9  10.9
    ## 36:   8.3   8.4   8.5   8.6   8.7
    ## 37:   7.3   7.3   7.3   7.3   7.3
    ## 38:  10.1  10.1  10.2  10.2  10.2
    ## 39:   2.2   2.3   2.3   2.3   2.3
    ## 40:   8.4   8.4   8.6   8.6   8.6
    ## 41:    NA    NA    NA    NA    NA
    ## 42:   4.8   5.0   5.0   5.4   5.5
    ## 43:  12.1  12.1  12.1  12.1  12.2
    ## 44:  12.1  12.2  12.4  12.5  12.5
    ## 45:   5.9   5.9   6.0   6.2   6.2
    ## 46:  12.7  12.5  12.5  12.6  12.6
    ## 47:   4.9   5.1   5.1   5.2   5.2
    ## 48:    NA    NA    NA    NA    NA
    ## 49:   8.8   9.4   9.1   9.2   9.2
    ## 50:   9.1   9.2   9.3   9.3   9.3
    ## 51:  12.1  11.3  12.1  12.2  12.3
    ## 52:   9.9   9.8   9.8   9.8   9.8
    ## 53:  10.9  10.9  10.9  11.0  11.0
    ## 54:    NA    NA    NA    NA    NA
    ## 55:   3.8   4.0   4.0   4.1   4.1
    ## 56:  10.4  10.7  11.0  11.1  11.1
    ## 57:  11.4  11.4  11.5  11.5  11.5
    ## 58:  11.7  11.9  12.0  12.3  12.2
    ## 59:   9.9  10.0  10.1  10.1  10.1
    ## 60:   9.5   9.7   9.7   9.8   9.8
    ## 61:  12.2  12.3  12.4  12.4  12.4
    ## 62:  13.4  13.4  13.4  13.4  13.4
    ## 63:   7.5   7.6   7.6   7.6   7.6
    ## 64:  10.8  10.8  10.8  10.9  10.9
    ## 65:   6.8   6.9   7.0   7.1   7.2
    ## 66:   7.7   7.6   7.8   8.0   8.0
    ## 67:  11.3  11.3  11.3  11.3  11.3
    ## 68:  12.6  12.7  12.8  12.9  12.9
    ## 69:  13.2  13.3  13.3  13.4  13.4
    ## 70:   8.5   8.6   8.7   8.7   8.7
    ## 71:  11.3  11.3  11.4  11.4  11.5
    ## 72:    NA    NA    NA    NA    NA
    ## 73:   3.0   3.0   3.0   3.0   3.0
    ## 74:   6.8   6.9   6.9   7.0   7.0
    ## 75:   8.0   8.2   8.2   8.2   8.2
    ##     scl13 scl14 scl15 scl16 scl17

``` r
na.omit(nascl, c("scl14"), invert = TRUE) #able to see if can replace values, can't replace with any , still 6 NAs
```

    ##                location_name scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12
    ## 1: Iran, Islamic Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:       Korea (Republic of)    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:      Moldova, Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:       Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:        Russian Federation    NA    NA    NA    NA    NA    NA    NA    NA
    ## 6:                  Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA
    ##    scl13 scl14 scl15 scl16 scl17
    ## 1:    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA    NA
    ## 6:    NA    NA    NA    NA    NA

SCL no values Iran, Islamic Republic of NA  
Korea (Republic of) NA  
Moldova, Republic of NA  
Palestine, State of NA Russian Federation NA Viet Nam NA

\#\#\#\#GDP14

``` r
na.omit(edit_phy14, c("GDP14"), invert = TRUE) # 7 missing GDP2014
```

    ##                location_name GSNI_PERIOD onebias twobias nobias political
    ## 1: Iran, Islamic Republic of   2005–2009   98.54   92.49   1.46     84.63
    ## 2:       Korea (Republic of)   2010–2014   87.07   62.91  12.93     63.68
    ## 3:                Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80
    ## 4:      Moldova, Republic of   2005–2009   90.06   67.21   9.94     60.33
    ## 5:       Palestine, State of   2010–2014   98.00   92.30   2.00     89.30
    ## 6:                  Viet Nam   2005–2009   92.89   69.17   7.11     59.40
    ## 7:                     Yemen   2010–2014   97.80   92.10   2.20     87.40
    ##    economic educational physical cvd05fem cvd06fem cvd07fem cvd08fem cvd09fem
    ## 1:    88.86       55.42    78.69 346.4549 333.0393 318.5381 305.9814 298.0127
    ## 2:    54.33       25.67    58.27 155.9141 143.8054 132.6713 120.8555 111.7862
    ## 3:    71.53       41.00    81.73 546.8872 553.7142 543.2533 536.8522 507.4175
    ## 4:    58.80       16.73    65.20 513.3270 491.9931 497.1151 484.5283 467.9101
    ## 5:    79.50       26.70    83.50 367.3750 362.0774 355.8204 350.5859 343.9703
    ## 6:    62.49       20.36    70.56 256.6274 256.6231 255.9745 255.1929 254.4622
    ## 7:    87.20       45.30    81.00 493.0600 489.2440 485.1008 483.3384 478.3407
    ##    cvd10fem cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem  cvd16fem
    ## 1: 290.3670 283.4457 278.28489 277.07263 275.18836 277.67423 278.51259
    ## 2: 105.3449 100.1279  95.58209  90.21631  85.58348  83.53081  81.91117
    ## 3: 488.1873 483.2938 465.05092 442.79747 429.36559 435.06853 415.09331
    ## 4: 459.1067 400.9103 387.36233 372.28763 377.82255 382.27748 381.40961
    ## 5: 341.7886 337.6966 320.17827 314.63758 323.39695 338.52757 350.93555
    ## 6: 252.5587 250.1793 247.05521 243.05616 239.21285 234.94030 230.61406
    ## 7: 472.1759 471.3592 468.99902 467.16132 464.47411 466.86730 466.88438
    ##     cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ## 1: 277.88105 271.04348 268.72894  400.2154  389.5281  376.5086  362.0445
    ## 2:  81.88305  82.38176  83.11824  190.0032  178.2896  167.8587  158.2836
    ## 3: 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687  761.4547
    ## 4: 368.12912 361.05127 341.11337  702.8658  671.8424  666.6718  648.2151
    ## 5: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 6: 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026  455.8597
    ## 7: 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200  566.4953
    ##    cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ## 1:  350.5229  338.1660  323.1285  311.8184  304.8604  299.1565  300.1241
    ## 2:  149.4912  143.7630  137.6913  131.7820  123.9082  117.5575  114.1778
    ## 3:  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342  644.2163
    ## 4:  638.1296  641.1604  572.9933  565.3869  534.3178  552.1560  587.3132
    ## 5:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 6:  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765  443.7173
    ## 7:  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702  536.2482
    ##    cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ## 1:  297.2979  294.9042  290.2879  288.5816  374.2741  361.9923  347.9814
    ## 2:  110.9073  110.6175  108.2689  108.8687  172.0355  160.0969  148.9444
    ## 3:  618.5638  613.3843  580.3176  569.3491  634.0089  651.1682  645.5907
    ## 4:  566.6405  523.8055  511.8000  498.5963  589.7769  564.1553  566.4516
    ## 5:  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939  415.8933
    ## 6:  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238  336.5239
    ## 7:  536.1051  541.0473  547.2489  550.6548  535.1088  530.9694  525.7088
    ##    cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ## 1:  334.1940  324.1761  313.9673  302.7896  294.4027  290.2060  286.3140
    ## 2:  137.6671  128.5412  122.2008  116.6089  111.6049  105.3768  100.1378
    ## 3:  632.7615  603.3141  581.8522  562.6225  543.9251  519.5371  513.0161
    ## 4:  552.6234  539.0321  535.4923  471.5840  460.0871  438.8196  449.0384
    ## 5:  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001  368.8565
    ## 6:  337.0268  337.1512  335.4539  333.0107  329.9312  326.6030  323.1828
    ## 7:  523.7193  517.1861  508.7890  507.4918  504.3873  501.9957  497.8142
    ##    cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ## 1: 287.96550 286.88535 285.32918 279.68642  277.7336        74.6     20.5
    ## 2:  97.49873  95.22265  95.17711  94.68896   95.4053        79.8     22.7
    ## 3: 523.15984 500.14376 493.82082 473.26712  466.3130        70.2     18.3
    ## 4: 463.27943 456.27984 432.82716 423.77893  404.9551        70.5     17.2
    ## 5: 380.88863 384.46844 388.17552 386.67955  381.6978          NA       NA
    ## 6: 319.13626 314.95461 311.23748 307.70802  303.9047        75.6     21.0
    ## 7: 500.90386 500.85723 504.82748 509.87687  513.0910        64.7     17.9
    ##    LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05  phy06
    ## 1:        78.0     22.3       78.50     22.3        79.1     22.5 0.8869 0.5361
    ## 2:        83.8     25.9       85.10     26.9        86.1     27.9 1.7529 1.8047
    ## 3:        73.3     19.3       75.30     20.2        77.3     21.7     NA     NA
    ## 4:        73.1     18.2       75.00     19.5        77.1     20.9 2.3783 2.3768
    ## 5:          NA       NA       76.22       NA          NA       NA     NA     NA
    ## 6:        77.1     21.5       77.60     21.8        78.1     22.0     NA     NA
    ## 7:        69.6     18.7       69.60     18.9        68.9     18.7     NA     NA
    ##     phy07  phy08  phy09  phy10  phy11  phy12  phy13 phy14edit  phy15  phy16
    ## 1:     NA     NA     NA 0.8900     NA     NA     NA    1.5044 1.1526     NA
    ## 2: 1.8655 1.8407 1.9185 1.9839 2.0361 2.0798 2.1632    2.2070 2.2494 2.3037
    ## 3:     NA     NA     NA     NA     NA     NA     NA        NA     NA     NA
    ## 4: 2.3871 2.3545 2.3912 2.3808 2.4142 2.4135 2.5028    2.4746 2.4836     NA
    ## 5:     NA     NA     NA     NA     NA     NA     NA        NA     NA     NA
    ## 6:     NA     NA     NA     NA     NA     NA     NA        NA     NA     NA
    ## 7:     NA     NA     NA     NA     NA     NA     NA        NA     NA     NA
    ##     phy17               HE05               HE06               HE07
    ## 1: 1.1292         5.30572176 5.1986260399999997         5.03977919
    ## 2: 2.3608 4.6178216900000004 4.9439678200000001 5.1149072599999998
    ## 3:     NA               <NA>               <NA>               <NA>
    ## 4: 3.2066 7.9639606499999998 8.7857027100000007 9.1826009800000001
    ## 5:     NA               <NA>               <NA>               <NA>
    ## 6:     NA               <NA>               <NA>               <NA>
    ## 7:     NA               <NA>               <NA>               <NA>
    ##                  HE08               HE09               HE10               HE11
    ## 1: 5.2816843999999996 6.5595455200000004 6.7547311800000003 6.6072506899999999
    ## 2:         5.39896727 5.7822899799999998 5.9173440900000003         6.00844383
    ## 3:               <NA>               <NA>               <NA>               <NA>
    ## 4: 9.1230525999999994        11.39545822 10.131128309999999 9.0967035299999992
    ## 5:               <NA>               <NA>               <NA>               <NA>
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ## 7:               <NA>               <NA>               <NA>               <NA>
    ##                  HE12               HE13               HE14               HE15
    ## 1: 6.6364860500000002         5.99379873 6.9135108000000001 7.7605791100000001
    ## 2:         6.13262033 6.2478942899999996 6.4743809700000003 6.6527166400000004
    ## 3:               <NA>               <NA>               <NA>               <NA>
    ## 4: 9.1396207799999996 8.6830034299999994 8.6327571899999995 8.5576353100000002
    ## 5:               <NA>               <NA>               <NA>               <NA>
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ## 7:               <NA>               <NA>               <NA>               <NA>
    ##                  HE16               HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ## 1: 8.8595066100000004 8.6596641499999993    NA    NA    NA    NA    NA    NA
    ## 2: 6.9143271400000001         7.10694933    NA    NA    NA    NA    NA    NA
    ## 3:               <NA>               <NA>  10.2  10.2  10.3  10.3  10.4  10.6
    ## 4: 7.5355224600000001 7.0132851599999997    NA    NA    NA    NA    NA    NA
    ## 5:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 6:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 7:               <NA>               <NA>   1.9   2.0   2.2   2.3   2.5   2.6
    ##    scl11 scl12 scl13 scl14 scl15 scl16 scl17 GDP05 GDP06 GDP07 GDP08 GDP09
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:  10.6  10.7  10.7  10.8  10.8  10.9  10.9    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 6:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 7:   2.8   3.0   3.0   3.0   3.0   3.0   3.0    NA    NA    NA    NA    NA
    ##    GDP10 GDP11 GDP12 GDP13 GDP14 GDP15 GDP16 GDP17 MMR05 MMR06 MMR07 MMR08
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    34    32    30    28
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    15    14    15    15
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    34    31    31    29
    ## 5:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 6:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 7:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##    MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17               LMH09
    ## 1:    25    22    19    18    17    17    17    16    16 upper middle income
    ## 2:    16    15    14    13    13    12    12    11    11         high income
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 4:    28    29    21    22    21    23    22    20    19 lower middle income
    ## 5:    NA    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 6:    NA    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 7:    NA    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ##                  LMH14
    ## 1: upper middle income
    ## 2:         high income
    ## 3:                <NA>
    ## 4: lower middle income
    ## 5:                <NA>
    ## 6:                <NA>
    ## 7:                <NA>

``` r
naGDP <- edit_phy14[, c("location_name", "GDP05", "GDP06", "GDP07", "GDP08", "GDP09","GDP10", "GDP11", "GDP12", "GDP13", "GDP14", "GDP15", "GDP16", "GDP17")]
naGDP #GDP columns 09-17
```

    ##                 location_name      GDP05      GDP06      GDP07      GDP08
    ##  1:                   Algeria  3113.0953  3478.7109  3946.6645  4923.8432
    ##  2:                   Andorra 40066.2569 42675.8128 47803.6936 48718.4969
    ##  3:                 Argentina  5109.8513  5919.0120  7245.4483  9020.8731
    ##  4:                   Armenia  1643.7530  2158.1437  3139.2775  4010.8572
    ##  5:                 Australia 33999.2429 36044.9228 40960.0545 49601.6567
    ##  6:                Azerbaijan  1578.4024  2473.0818  3851.4379  5574.6038
    ##  7:                   Belarus  3125.8105  3847.4341  4735.6576  6377.3697
    ##  8:                    Brazil  4790.4371  5886.4636  7348.0308  8831.0231
    ##  9:                  Bulgaria  3899.9076  4523.0508  5885.1043  7265.7355
    ## 10:              Burkina Faso   457.9336   473.4498   535.0626   643.4046
    ## 11:                    Canada 36266.1871 40385.8700 44543.0410 46594.4510
    ## 12:                     Chile  7598.5251  9464.5502 10502.3545 10751.4797
    ## 13:                     China  1753.4178  2099.2294  2693.9701  3468.3046
    ## 14:                  Colombia  3414.4652  3741.0928  4714.0731  5472.5365
    ## 15:                    Cyprus 24959.2592 26729.3234 31244.9262 35397.3637
    ## 16:                   Ecuador  3002.1369  3328.8830  3567.8364  4249.0193
    ## 17:                   Estonia 10406.3967 12631.5681 16741.9392 18227.1195
    ## 18:                  Ethiopia   162.4327   194.6874   244.2860   326.4368
    ## 19:                   Finland 39040.2889 41188.0937 48414.8451 53554.0389
    ## 20:                    France 34760.1878 36443.6234 41508.4340 45334.1144
    ## 21:                   Georgia  1642.7609  1996.0571  2635.3539  3324.7359
    ## 22:                   Germany 34507.3688 36323.4477 41587.2129 45427.1517
    ## 23:                     Ghana   492.5442   913.3939  1081.1663  1217.0648
    ## 24:                     Haiti   766.6921   792.8259   981.1113  1076.7013
    ## 25:                   Hungary 11200.5769 11475.8227 13918.9602 15753.4733
    ## 26:                     India   714.8610   806.7533  1028.3348   998.5223
    ## 27:                 Indonesia  1263.2873  1589.8015  1860.0028  2166.8542
    ## 28: Iran, Islamic Republic of         NA         NA         NA         NA
    ## 29:                      Iraq  1855.5220  2373.2148  3182.9480  4636.6110
    ## 30:                     Japan 37217.6487 35433.9890 35275.2284 39339.2976
    ## 31:                    Jordan  2183.3962  2513.0317  2735.3831  3455.7673
    ## 32:                Kazakhstan  3771.2790  5291.5757  6771.4148  8513.5646
    ## 33:       Korea (Republic of)         NA         NA         NA         NA
    ## 34:                    Kuwait 35591.0058 42781.3665 45782.2766 55494.9510
    ## 35:                Kyrgyzstan         NA         NA         NA         NA
    ## 36:                   Lebanon  4575.1055  4626.8598  5207.7960  6111.3324
    ## 37:                     Libya  8163.0108  9336.3567 11300.1913 14382.5763
    ## 38:                  Malaysia  5587.0256  6209.1245  7243.4560  8474.5868
    ## 39:                      Mali   489.0211   523.0386   596.6902   694.2777
    ## 40:                    Mexico  8277.6713  9068.2944  9642.6806 10016.5713
    ## 41:      Moldova, Republic of         NA         NA         NA         NA
    ## 42:                   Morocco  2018.0257  2196.0122  2499.2599  2890.3601
    ## 43:               Netherlands 41979.0558 44863.3506 51733.4421 57644.4800
    ## 44:               New Zealand 27751.0655 26671.3294 32511.1267 31290.2537
    ## 45:                   Nigeria  1268.3834  1656.4248  1883.4613  2242.8719
    ## 46:                    Norway 66810.4785 74148.3201 85139.9604 96944.0956
    ## 47:                  Pakistan   748.9226   836.8605   908.0951   990.8466
    ## 48:       Palestine, State of         NA         NA         NA         NA
    ## 49:                      Peru  2729.4987  3154.3312  3606.0704  4220.6170
    ## 50:               Philippines  1244.3490  1452.4387  1744.6403  1991.2315
    ## 51:                    Poland  8021.5057  9035.4105 11254.5174 13996.0252
    ## 52:                     Qatar 51455.5942 59530.1535 65421.7528 80234.4701
    ## 53:                   Romania  4617.9290  5757.4964  8360.1663 10435.0440
    ## 54:        Russian Federation  5323.4631  6920.1891  9101.2550 11635.2729
    ## 55:                    Rwanda   331.8112   367.0325   438.8336   543.7651
    ## 56:                    Serbia  3720.4792  4382.6173  5848.4764  7101.0401
    ## 57:                 Singapore 29961.2633 33769.1542 39432.9383 40007.4693
    ## 58:                  Slovenia 18098.9085 19672.9656 23787.6466 27483.3372
    ## 59:              South Africa  5383.6565  5602.0110  6095.6224  5760.8053
    ## 60:                     Spain 26419.2969 28365.3135 32549.9710 35366.2596
    ## 61:                    Sweden 43437.0631 46593.6022 53700.0053 56152.5523
    ## 62:               Switzerland 54952.6738 57579.5020 63555.2375 72487.8459
    ## 63:                  Thailand  2894.0627  3369.5434  3973.0170  4379.6585
    ## 64:       Trinidad and Tobago 12327.2332 14102.4958 16539.8781 21204.1050
    ## 65:                   Tunisia  3193.2066  3370.0339  3775.7500  4307.1559
    ## 66:                    Turkey  7456.3877  8102.1215  9791.6516 10940.9912
    ## 67:                   Ukraine  1826.9314  2300.7697  3065.6113  3887.2423
    ## 68:            United Kingdom 42030.2866 44599.6976 50566.8266 47286.9985
    ## 69:             United States 44114.7478 46298.7314 47975.9677 48382.5584
    ## 70:                   Uruguay  5226.9378  5887.8487  7026.5115  9091.0790
    ## 71:                Uzbekistan   546.7769   654.2838   830.4077  1082.2860
    ## 72:                  Viet Nam         NA         NA         NA         NA
    ## 73:                     Yemen         NA         NA         NA         NA
    ## 74:                    Zambia   702.7409  1047.9192  1124.2906  1394.0006
    ## 75:                  Zimbabwe   476.5553   447.8549   431.7872   356.6933
    ##                 location_name      GDP05      GDP06      GDP07      GDP08
    ##          GDP09      GDP10       GDP11       GDP12       GDP13      GDP14
    ##  1:  3883.1324  4479.3417   5462.2609   5591.2124   5498.7841  5494.3523
    ##  2: 43503.1855 40852.6668  43335.3289  38686.4613  39538.7667 41303.9294
    ##  3:  8225.1372 10385.9644  12848.8642  13082.6643  13080.2547 12334.7982
    ##  4:  2994.3425  3218.3727   3525.8047   3681.8575   3838.1858  3986.2316
    ##  5: 42772.3592 52022.1256  62517.8337  68012.1479  68150.1070 62510.7912
    ##  6:  4950.2948  5842.8058   7189.6912   7496.2946   7875.7570  7891.3131
    ##  7:  5351.3554  6029.3968   6519.2302   6940.1593   7978.8726  8318.5127
    ##  8:  8597.9154 11286.2430  13245.6125  12370.0242  12300.3249 12112.5882
    ##  9:  6988.2333  6812.4063   7809.4251   7395.8498   7655.1297  7876.8665
    ## 10:   624.1752   647.8358    751.1730    758.0007    787.4702   792.8468
    ## 11: 40773.0615 47448.0132  52087.4464  52678.3901  52652.5937 50893.4467
    ## 12: 10208.9068 12808.0346  14637.2402  15351.5513  15842.9408 14670.9968
    ## 13:  3832.2364  4550.4531   5618.1323   6316.9183   7050.6463  7678.5995
    ## 14:  5193.2415  6336.7095   7335.1669   8050.2554   8218.3478  8114.3439
    ## 15: 32109.2425 31023.6383  32396.3857  28912.1569  27729.1927 27129.6261
    ## 16:  4231.6158  4633.5904   5200.5558   5682.0450   6056.3308  6377.0915
    ## 17: 14794.9711 14790.8209  17621.5480  17534.4213  19174.1003 20367.1032
    ## 18:   380.5690   341.5541    354.4796    467.0779    499.5316   566.9265
    ## 19: 47293.9928 46459.9733  51081.9977  47710.7902  49878.0432 50260.2999
    ## 20: 41575.4187 40638.3340  43790.7320  40874.7035  42592.9341 43011.2631
    ## 21:  2822.6674  3233.2959   4021.7433   4421.8182   4623.7457  4739.1883
    ## 22: 41485.9016 41531.9342  46644.7760  43858.3631  46285.7641 47959.9933
    ## 23:  1077.6622  1299.3449   1549.4629   1587.5612   2345.3929  1971.0333
    ## 24:  1150.2111  1172.0985   1287.9541   1337.3354   1393.9560  1402.1002
    ## 25: 13046.4810 13191.6213  14216.1656  12950.6865  13687.5141 14267.0122
    ## 26:  1101.9608  1357.5637   1458.1035   1443.8795   1449.6059  1573.8815
    ## 27:  2261.2472  3122.3628   3643.0439   3694.3489   3623.9116  3491.6248
    ## 28:         NA         NA          NA          NA          NA         NA
    ## 29:  3853.9409  4655.4250   6036.3962   6829.9640   7076.8772  6818.8046
    ## 30: 40855.1756 44507.6764  48167.9973  48603.4766  40454.4475 38109.4121
    ## 31:  3559.6911  3736.6465   3852.7528   3909.9076   4043.7490  4130.8791
    ## 32:  7165.2232  9070.4883  11634.0019  12386.7000  13890.6318 12807.2607
    ## 33:         NA         NA          NA          NA          NA         NA
    ## 34: 37561.6727 38577.4983  48631.6913  51979.1052  49388.1374 44062.3170
    ## 35:         NA         NA          NA          NA          NA         NA
    ## 36:  7354.9536  7761.6462   7674.8354   7950.6954   7931.0805  7686.2560
    ## 37: 10275.2666 12064.7807   5554.1792  13025.2814  10363.7895  6466.9103
    ## 38:  7292.4944  9040.5663  10399.3728  10817.4429  10970.1233 11319.0798
    ## 39:   698.8989   710.2742    837.6034    778.6193    805.0328   848.2741
    ## 40:  8002.9721  9271.3982  10203.4209  10241.7279  10725.1833 10928.9168
    ## 41:         NA         NA          NA          NA          NA         NA
    ## 42:  2866.9242  2839.9252   3046.9491   2912.6583   3121.6812  3171.6992
    ## 43: 52514.0271 50950.0343  54159.3466  50073.0057  52184.0619 52830.1742
    ## 44: 28205.7328 33700.1260  38437.5432  39982.7543  42962.9882 44553.2822
    ## 45:  1891.3354  2280.4374   2487.5982   2723.8228   2961.5503  3098.9863
    ## 46: 79977.6971 87693.7901 100600.5624 101524.1419 102913.4508 97019.1828
    ## 47:   957.9957   987.4097   1164.9761   1198.1090   1208.9043  1251.1641
    ## 48:         NA         NA          NA          NA          NA         NA
    ## 49:  4196.3128  5082.3548   5869.3231   6528.9722   6756.7528  6672.8803
    ## 50:  1905.8947  2217.4740   2450.7337   2694.3055   2871.4309  2959.6485
    ## 51: 11526.0559 12613.0110  13879.5610  13097.2708  13696.4663 14271.3059
    ## 52: 59094.4449 67403.1603  82409.5773  85076.1415  85050.8663 83858.4769
    ## 53:  8548.1187  8214.0769   9099.2175   8507.1048   9547.8522 10043.6774
    ## 54:  8562.8133 10674.9958  14311.0843  15420.8745  15974.6446 14095.6487
    ## 55:   579.9390   610.0124    668.8690    725.6277    723.2583   743.9948
    ## 56:  6169.1142  5735.4229   6809.1598   6015.9452   6755.0737  6600.0568
    ## 57: 38927.2069 47236.9602  53890.4287  55546.4885  56967.4258 57562.5308
    ## 58: 24694.2305 23509.5431  25095.1323  22643.1003  23496.6025 24214.9221
    ## 59:  5862.7973  7328.6156   8007.4128   7501.4700   6832.4569  6433.1873
    ## 60: 32042.4741 30502.7197  31636.4463  28324.4293  29059.5480 29461.5503
    ## 61: 46946.9603 52869.0443  60755.7596  58037.8213  61126.9432 60020.3605
    ## 62: 69927.4688 74605.7211  88415.6280  83538.2300  85112.4644 86605.5634
    ## 63:  4213.0063  5076.3402   5492.1213   5860.5825   6168.2630  5951.8837
    ## 64: 14514.1417 16683.3554  19034.1492  19157.4170  20143.6644 20270.8594
    ## 65:  4128.4628  4141.9764   4264.6749   4152.6786   4222.7032  4305.4742
    ## 66:  9103.7099 10742.4301  11420.7733  11795.3167  12614.4803 12157.3380
    ## 67:  2542.9954  2965.1397   3569.7581   3855.4177   4029.7113  3104.6432
    ## 68: 38713.1374 39435.8399  42038.5723  42462.7716  43444.5330 47425.6077
    ## 69: 47099.9805 48467.5158  49886.8181  51610.6053  53117.6678 55064.7445
    ## 70:  9451.9324 11992.0166  14236.6812  15171.5847  16973.6742 16831.9729
    ## 71:  1213.2653  1634.3121   1926.2930   2137.0251   2281.4110  2492.3366
    ## 72:         NA         NA          NA          NA          NA         NA
    ## 73:         NA         NA          NA          NA          NA         NA
    ## 74:  1159.9078  1489.4593   1672.9083   1763.0727   1878.9097  1763.0626
    ## 75:   771.5988   948.3319   1093.6540   1304.9698   1430.0008  1434.8993
    ##          GDP09      GDP10       GDP11       GDP12       GDP13      GDP14
    ##          GDP15      GDP16      GDP17
    ##  1:  4187.5097  3945.4821  4111.2941
    ##  2: 35762.5231 37474.6654 38962.8804
    ##  3: 13789.0604 12790.2425 14613.0418
    ##  4:  3607.2967  3591.8293  3914.5013
    ##  5: 56755.7217 49971.1315 54027.9668
    ##  6:  5500.3104  3880.7387  4147.0897
    ##  7:  5949.1063  5022.6266  5761.7471
    ##  8:  8814.0010  8710.0967  9925.3862
    ##  9:  7055.9357  7548.8550  8334.0817
    ## 10:   653.3270   688.2497   734.9944
    ## 11: 43585.5120 42322.4848 45148.5527
    ## 12: 13574.1718 13753.5944 14999.3701
    ## 13:  8066.9426  8147.9377  8879.4387
    ## 14:  6175.8760  5870.7780  6376.7067
    ## 15: 23333.7149 24532.5191 26338.6943
    ## 16:  6124.4916  6060.0933  6213.5013
    ## 17: 17522.2302 18437.2528 20458.4607
    ## 18:   640.5419   717.1246   768.5223
    ## 19: 42784.6984 43784.2840 46336.6633
    ## 20: 36638.1849 37037.3742 38812.1610
    ## 21:  4014.1859  4062.1699  4357.0009
    ## 22: 41086.7297 42107.5173 44552.8194
    ## 23:  1743.8510  1931.3895  2025.9324
    ## 24:  1389.1195  1265.9876  1294.2397
    ## 25: 12706.8912 13090.5067 14605.8543
    ## 26:  1605.6054  1732.5643  1981.6510
    ## 27:  3331.6951  3562.8458  3837.6517
    ## 28:         NA         NA         NA
    ## 29:  4989.8031  4777.1976  5205.2883
    ## 30: 34524.4699 38761.8182 38386.5111
    ## 31:  4164.1079  4176.5889  4234.4031
    ## 32: 10510.7719  7714.8418  9247.5813
    ## 33:         NA         NA         NA
    ## 34: 29869.5294 27653.0668 29759.4365
    ## 35:         NA         NA         NA
    ## 36:  7644.5487  7629.8911  7801.1787
    ## 37:  4337.9191  4035.1943  5756.6984
    ## 38:  9955.2437  9817.7385 10259.1818
    ## 39:   751.4748   780.7186   830.0184
    ## 40:  9616.6450  8744.5158  9287.8497
    ## 41:         NA         NA         NA
    ## 42:  2875.2580  2896.7200  3036.3253
    ## 43: 45175.2319 46007.8529 48675.2223
    ## 44: 38615.9952 40105.6134 42849.4263
    ## 45:  2687.4801  2176.0022  1968.5647
    ## 46: 74355.5159 70459.1825 75496.7541
    ## 47:  1356.6678  1368.4543  1464.9933
    ## 48:         NA         NA         NA
    ## 49:  6229.1017  6204.9973  6710.5080
    ## 50:  3001.0404  3073.6536  3123.2342
    ## 51: 12578.4955 12447.4396 13864.6818
    ## 52: 63039.0635 57163.0757 59124.9324
    ## 53:  8969.1489  9548.5874 10807.7954
    ## 54:  9313.0136  8704.8984 10720.3326
    ## 55:   751.6394   745.3428   772.3185
    ## 56:  5588.9807  5765.2008  6292.5436
    ## 57: 55646.6187 56828.2953 60913.7453
    ## 58: 20881.7669 21663.6434 23512.8173
    ## 59:  5734.6336  5272.9184  6132.4798
    ## 60: 25732.0184 26505.3432 28170.1679
    ## 61: 51545.4836 51965.1572 53791.5087
    ## 62: 82081.5972 80172.2321 80449.9945
    ## 63:  5840.0465  5994.2315  6592.9149
    ## 64: 18289.7043 16176.9474 16238.1932
    ## 65:  3861.6885  3697.9308  3481.2287
    ## 66: 11006.2497 10895.3187 10591.4744
    ## 67:  2124.6623  2187.7305  2640.6757
    ## 68: 44974.8319 41064.1334 40361.4174
    ## 69: 56839.3818 57951.5841 60062.2223
    ## 70: 15613.7643 15387.1440 17322.1474
    ## 71:  2615.0251  2567.7992  1826.5669
    ## 72:         NA         NA         NA
    ## 73:         NA         NA         NA
    ## 74:  1337.7956  1280.5784  1534.8668
    ## 75:  1445.0711  1464.5835  1548.1701
    ##          GDP15      GDP16      GDP17

``` r
na.omit(naGDP, c("GDP14"), invert = TRUE) #able to see if can replace values, can't replace with any , still 7 NAs
```

    ##                location_name GDP05 GDP06 GDP07 GDP08 GDP09 GDP10 GDP11 GDP12
    ## 1: Iran, Islamic Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:       Korea (Republic of)    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:                Kyrgyzstan    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:      Moldova, Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:       Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 6:                  Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA
    ## 7:                     Yemen    NA    NA    NA    NA    NA    NA    NA    NA
    ##    GDP13 GDP14 GDP15 GDP16 GDP17
    ## 1:    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA    NA
    ## 6:    NA    NA    NA    NA    NA
    ## 7:    NA    NA    NA    NA    NA

GDP (no values) Iran, Islamic Republic of NA  
Korea (Republic of) NA  
Kyrgyzstan NA  
Moldova, Republic of NA  
Palestine, State of NA  
Viet Nam NA  
Yemen NA

\#\#\#\#MMR14

``` r
na.omit(edit_phy14, c("MMR14"), invert = TRUE) # 5 missing MMR2014
```

    ##          location_name GSNI_PERIOD onebias twobias nobias political economic
    ## 1:             Andorra   2005–2009   27.01    7.43  72.99     14.08     8.73
    ## 2:          Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80    71.53
    ## 3: Palestine, State of   2010–2014   98.00   92.30   2.00     89.30    79.50
    ## 4:            Viet Nam   2005–2009   92.89   69.17   7.11     59.40    62.49
    ## 5:               Yemen   2010–2014   97.80   92.10   2.20     87.40    87.20
    ##    educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem  cvd09fem
    ## 1:        1.81    12.01  94.99074  94.10549  92.00298  91.82684  93.03548
    ## 2:       41.00    81.73 546.88719 553.71417 543.25326 536.85221 507.41753
    ## 3:       26.70    83.50 367.37503 362.07740 355.82036 350.58586 343.97033
    ## 4:       20.36    70.56 256.62741 256.62311 255.97453 255.19294 254.46217
    ## 5:       45.30    81.00 493.05999 489.24403 485.10075 483.33838 478.34072
    ##     cvd10fem  cvd11fem  cvd12fem  cvd13fem cvd14fem  cvd15fem  cvd16fem
    ## 1:  93.64847  94.72272  94.77537  95.38794  95.7992  96.76678  96.28366
    ## 2: 488.18726 483.29378 465.05092 442.79747 429.3656 435.06853 415.09331
    ## 3: 341.78855 337.69661 320.17827 314.63758 323.3969 338.52757 350.93555
    ## 4: 252.55867 250.17926 247.05521 243.05616 239.2128 234.94030 230.61406
    ## 5: 472.17590 471.35922 468.99902 467.16132 464.4741 466.86730 466.88438
    ##     cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ## 1:  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052  126.3548
    ## 2: 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687  761.4547
    ## 3: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 4: 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026  455.8597
    ## 5: 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200  566.4953
    ##    cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ## 1:  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398  119.3653
    ## 2:  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342  644.2163
    ## 3:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 4:  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765  443.7173
    ## 5:  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702  536.2482
    ##    cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ## 1:  118.6682  117.7883  116.9475  115.8910  114.4628  112.7694  110.3326
    ## 2:  618.5638  613.3843  580.3176  569.3491  634.0089  651.1682  645.5907
    ## 3:  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939  415.8933
    ## 4:  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238  336.5239
    ## 5:  536.1051  541.0473  547.2489  550.6548  535.1088  530.9694  525.7088
    ##    cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ## 1:  109.3167  109.3833  109.1886  109.4555  108.8728  108.7915  108.3474
    ## 2:  632.7615  603.3141  581.8522  562.6225  543.9251  519.5371  513.0161
    ## 3:  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001  368.8565
    ## 4:  337.0268  337.1512  335.4539  333.0107  329.9312  326.6030  323.1828
    ## 5:  523.7193  517.1861  508.7890  507.4918  504.3873  501.9957  497.8142
    ##    cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ## 1:  108.1292  107.5617  106.9530  106.3957  105.7119          NA       NA
    ## 2:  523.1598  500.1438  493.8208  473.2671  466.3130        70.2     18.3
    ## 3:  380.8886  384.4684  388.1755  386.6795  381.6978          NA       NA
    ## 4:  319.1363  314.9546  311.2375  307.7080  303.9047        75.6     21.0
    ## 5:  500.9039  500.8572  504.8275  509.8769  513.0910        64.7     17.9
    ##    LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05  phy06
    ## 1:          NA       NA       84.80       NA          NA       NA 3.2319 3.0123
    ## 2:        73.3     19.3       75.30     20.2        77.3     21.7     NA     NA
    ## 3:          NA       NA       76.22       NA          NA       NA     NA     NA
    ## 4:        77.1     21.5       77.60     21.8        78.1     22.0     NA     NA
    ## 5:        69.6     18.7       69.60     18.9        68.9     18.7     NA     NA
    ##     phy07 phy08  phy09 phy10 phy11 phy12 phy13 phy14edit  phy15 phy16 phy17
    ## 1: 3.0109    NA 3.1479     4    NA    NA    NA    3.3333 3.3333    NA    NA
    ## 2:     NA    NA     NA    NA    NA    NA    NA        NA     NA    NA    NA
    ## 3:     NA    NA     NA    NA    NA    NA    NA        NA     NA    NA    NA
    ## 4:     NA    NA     NA    NA    NA    NA    NA        NA     NA    NA    NA
    ## 5:     NA    NA     NA    NA    NA    NA    NA        NA     NA    NA    NA
    ##                  HE05               HE06               HE07               HE08
    ## 1: 5.5754260999999996 4.9350943599999999 4.9255475999999998 5.8059859300000003
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3:               <NA>               <NA>               <NA>               <NA>
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5:               <NA>               <NA>               <NA>               <NA>
    ##                  HE09       HE10               HE11               HE12
    ## 1: 6.2023372700000001 6.64963865 6.2465286300000002 6.1015033699999996
    ## 2:               <NA>       <NA>               <NA>               <NA>
    ## 3:               <NA>       <NA>               <NA>               <NA>
    ## 4:               <NA>       <NA>               <NA>               <NA>
    ## 5:               <NA>       <NA>               <NA>               <NA>
    ##                  HE13               HE14               HE15               HE16
    ## 1: 5.9878034600000003 5.9791245499999999 6.2324533500000001 6.3434934600000004
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3:               <NA>               <NA>               <NA>               <NA>
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5:               <NA>               <NA>               <NA>               <NA>
    ##                  HE17 scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12 scl13
    ## 1: 6.5443186799999999   9.8  10.1  10.1  10.1  10.1  10.1  10.2  10.2  10.2
    ## 2:               <NA>  10.2  10.2  10.3  10.3  10.4  10.6  10.6  10.7  10.7
    ## 3:               <NA>    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:               <NA>    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:               <NA>   1.9   2.0   2.2   2.3   2.5   2.6   2.8   3.0   3.0
    ##    scl14 scl15 scl16 scl17    GDP05    GDP06    GDP07   GDP08    GDP09    GDP10
    ## 1:  10.2  10.2  10.2  10.2 40066.26 42675.81 47803.69 48718.5 43503.19 40852.67
    ## 2:  10.8  10.8  10.9  10.9       NA       NA       NA      NA       NA       NA
    ## 3:    NA    NA    NA    NA       NA       NA       NA      NA       NA       NA
    ## 4:    NA    NA    NA    NA       NA       NA       NA      NA       NA       NA
    ## 5:   3.0   3.0   3.0   3.0       NA       NA       NA      NA       NA       NA
    ##       GDP11    GDP12    GDP13    GDP14    GDP15    GDP16    GDP17 MMR05 MMR06
    ## 1: 43335.33 38686.46 39538.77 41303.93 35762.52 37474.67 38962.88    NA    NA
    ## 2:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ## 3:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ## 4:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ## 5:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ##    MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##          LMH09       LMH14
    ## 1: high income high income
    ## 2:        <NA>        <NA>
    ## 3:        <NA>        <NA>
    ## 4:        <NA>        <NA>
    ## 5:        <NA>        <NA>

``` r
naMMR <- edit_phy14[, c("location_name", "MMR05", "MMR06", "MMR07", "MMR08", "MMR09","MMR10", "MMR11", "MMR12", "MMR13", "MMR14", "MMR15", "MMR16", "MMR17")]
naMMR #MMR columns 09-17
```

    ##                 location_name MMR05 MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12
    ##  1:                   Algeria   127   122   119   117   117   115   116   116
    ##  2:                   Andorra    NA    NA    NA    NA    NA    NA    NA    NA
    ##  3:                 Argentina    59    57    56    53    56    51    48    47
    ##  4:                   Armenia    35    36    32    36    32    32    30    30
    ##  5:                 Australia     5     5     5     5     5     5     6     6
    ##  6:                Azerbaijan    42    35    34    32    32    31    30    29
    ##  7:                   Belarus    11     9     7     6     6     5     5     4
    ##  8:                    Brazil    71    72    71    70    69    65    61    60
    ##  9:                  Bulgaria    15    14    13    13    12    12    12    11
    ## 10:              Burkina Faso   437   422   410   401   393   385   377   369
    ## 11:                    Canada    11    11    11    12    12    11    11    11
    ## 12:                     Chile    25    25    23    21    21    20    18    17
    ## 13:                     China    44    42    40    40    37    36    34    33
    ## 14:                  Colombia    83    82    83    84    87    85    84    85
    ## 15:                    Cyprus    12     9     8     9     8     8     8     7
    ## 16:                   Ecuador    94    90    85    82    80    78    76    71
    ## 17:                   Estonia    18    16    17    14    14    11    13    12
    ## 18:                  Ethiopia   865   795   731   681   638   597   558   527
    ## 19:                   Finland     5     4     4     4     4     4     4     4
    ## 20:                    France     9     9     9     9     9     9     9     9
    ## 21:                   Georgia    39    33    36    39    43    32    32    30
    ## 22:                   Germany     6     6     6     6     6     6     6     5
    ## 23:                     Ghana   371   359   349   342   339   339   339   336
    ## 24:                     Haiti   459   467   473   484   484   506   496   500
    ## 25:                   Hungary    15    14    15    14    13    13    13    12
    ## 26:                     India   286   270   255   240   225   210   197   185
    ## 27:                 Indonesia   252   249   243   239   234   228   221   214
    ## 28: Iran, Islamic Republic of    34    32    30    28    25    22    19    18
    ## 29:                      Iraq   127   158   138    90    75    70    67    66
    ## 30:                     Japan     7     7     6     6     6     6     6     5
    ## 31:                    Jordan    62    58    56    55    54    53    52    52
    ## 32:                Kazakhstan    43    40    36    30    24    22    19    17
    ## 33:       Korea (Republic of)    15    14    15    15    16    15    14    13
    ## 34:                    Kuwait    10    10    10    10    10    10    10    11
    ## 35:                Kyrgyzstan    NA    NA    NA    NA    NA    NA    NA    NA
    ## 36:                   Lebanon    24    24    23    23    23    23    25    25
    ## 37:                     Libya    57    55    53    52    53    53    56    55
    ## 38:                  Malaysia    31    30    30    29    29    30    30    30
    ## 39:                      Mali   691   675   663   661   661   660   663   663
    ## 40:                    Mexico    54    51    49    49    51    46    43    41
    ## 41:      Moldova, Republic of    34    31    31    29    28    29    21    22
    ## 42:                   Morocco   131   121   113   106    99    92    86    81
    ## 43:               Netherlands    11     9     9     8     8     7     7     7
    ## 44:               New Zealand    11    11    11    10    11    11    10    10
    ## 45:                   Nigeria  1080  1040  1010   996   987   978   972   963
    ## 46:                    Norway     5     4     4     4     4     4     4     3
    ## 47:                  Pakistan   237   222   214   205   199   191   180   173
    ## 48:       Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 49:                      Peru   118   114   112   108   106   104   102   100
    ## 50:               Philippines   156   156   149   148   149   144   141   139
    ## 51:                    Poland     4     4     4     4     3     3     3     3
    ## 52:                     Qatar    12    11    11    11    10    10    10    10
    ## 53:                   Romania    35    32    30    28    28    27    24    23
    ## 54:        Russian Federation    42    36    32    30    27    25    23    22
    ## 55:                    Rwanda   643   541   469   427   424   373   349   329
    ## 56:                    Serbia    12    12    12    12    12    12    12    12
    ## 57:                 Singapore    13    13    12    11    10    10    10    10
    ## 58:                  Slovenia    10     8     8     8     8     8     8     7
    ## 59:              South Africa   201   201   199   191   179   171   161   143
    ## 60:                     Spain     5     4     4     4     4     4     4     4
    ## 61:                    Sweden     5     5     5     5     5     4     5     5
    ## 62:               Switzerland     7     7     6     6     6     6     6     6
    ## 63:                  Thailand    43    42    42    43    43    42    41    39
    ## 64:       Trinidad and Tobago    76    73    65    72    74    71    72    71
    ## 65:                   Tunisia    51    49    47    47    46    46    46    46
    ## 66:                    Turkey    33    30    28    27    25    24    23    22
    ## 67:                   Ukraine    33    31    33    33    27    25    23    24
    ## 68:            United Kingdom    11    11    11    11    10    10     9     8
    ## 69:             United States    13    14    14    14    15    15    15    16
    ## 70:                   Uruguay    22    20    20    19    19    17    17    17
    ## 71:                Uzbekistan    38    37    35    34    32    31    32    32
    ## 72:                  Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA
    ## 73:                     Yemen    NA    NA    NA    NA    NA    NA    NA    NA
    ## 74:                    Zambia   421   406   387   356   329   305   283   267
    ## 75:                  Zimbabwe   685   680   671   657   632   598   557   528
    ##                 location_name MMR05 MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12
    ##     MMR13 MMR14 MMR15 MMR16 MMR17
    ##  1:   115   114   114   113   112
    ##  2:    NA    NA    NA    NA    NA
    ##  3:    44    42    41    40    39
    ##  4:    26    27    28    26    26
    ##  5:     6     6     6     6     6
    ##  6:    28    28    27    26    26
    ##  7:     3     3     3     3     2
    ##  8:    61    62    63    62    60
    ##  9:    10    11    10    10    10
    ## 10:   362   353   343   331   320
    ## 11:    11    11    11    10    10
    ## 12:    16    15    14    13    13
    ## 13:    32    31    30    29    29
    ## 14:    85    85    85    84    83
    ## 15:     6     8     7     6     6
    ## 16:    67    65    63    61    59
    ## 17:    10    11    10    11     9
    ## 18:   498   472   446   422   401
    ## 19:     4     3     3     3     3
    ## 20:     9     8     8     8     8
    ## 21:    30    29    27    26    25
    ## 22:     5     5     5     5     7
    ## 23:   331   325   320   314   308
    ## 24:   496   492   488   489   480
    ## 25:    12    12    12    12    12
    ## 26:   175   166   158   150   145
    ## 27:   207   199   192   184   177
    ## 28:    17    17    17    16    16
    ## 29:    75    92    83    78    79
    ## 30:     5     5     5     5     5
    ## 31:    51    49    48    47    46
    ## 32:    14    13    12    10    10
    ## 33:    13    12    12    11    11
    ## 34:    11    11    11    12    12
    ## 35:    NA    NA    NA    NA    NA
    ## 36:    27    28    29    29    29
    ## 37:    58    63    70    73    72
    ## 38:    30    30    30    29    29
    ## 39:   663   642   620   590   562
    ## 40:    39    38    36    34    33
    ## 41:    21    23    22    20    19
    ## 42:    78    76    74    72    70
    ## 43:     6     6     6     6     5
    ## 44:     9    10    10    10     9
    ## 45:   951   943   931   925   917
    ## 46:     3     3     3     3     2
    ## 47:   166   161   154   143   140
    ## 48:    NA    NA    NA    NA    NA
    ## 49:    98    96    94    91    88
    ## 50:   136   131   127   124   121
    ## 51:     3     2     2     2     2
    ## 52:    10    10     9     9     9
    ## 53:    21    21    21    21    19
    ## 54:    20    19    18    18    17
    ## 55:   308   291   275   260   248
    ## 56:    12    12    13    12    12
    ## 57:     9     8     9     8     8
    ## 58:     7     7     7     7     7
    ## 59:   133   128   125   122   119
    ## 60:     4     4     4     4     4
    ## 61:     5     5     4     4     4
    ## 62:     6     5     5     5     5
    ## 63:    39    38    38    37    37
    ## 64:    70    69    68    68    67
    ## 65:    46    46    46    45    43
    ## 66:    20    19    19    18    17
    ## 67:    23    24    21    20    19
    ## 68:     8     8     8     7     7
    ## 69:    16    16    18    19    19
    ## 70:    18    17    18    18    17
    ## 71:    31    30    30    29    29
    ## 72:    NA    NA    NA    NA    NA
    ## 73:    NA    NA    NA    NA    NA
    ## 74:   254   242   232   222   213
    ## 75:   509   494   480   468   458
    ##     MMR13 MMR14 MMR15 MMR16 MMR17

``` r
na.omit(naMMR, c("MMR14"), invert = TRUE) #able to see if can replace values, can't replace any
```

    ##          location_name MMR05 MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13
    ## 1:             Andorra    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:          Kyrgyzstan    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3: Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:            Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:               Yemen    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##    MMR14 MMR15 MMR16 MMR17
    ## 1:    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA

MMR NAs (no values) Andorra NA  
Kyrgyzstan NA  
Palestine, State of NA Viet Nam NA Yemen NA

``` r
na.omit(edit_phy14, c("cvd14fem", "cvd14both", "LEbirth2015", "phy14edit", "scl14", "GDP14", "MMR14"), invert = TRUE)
```

    ##                location_name GSNI_PERIOD onebias twobias nobias political
    ## 1:                   Andorra   2005–2009   27.01    7.43  72.99     14.08
    ## 2: Iran, Islamic Republic of   2005–2009   98.54   92.49   1.46     84.63
    ## 3:       Korea (Republic of)   2010–2014   87.07   62.91  12.93     63.68
    ## 4:                Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80
    ## 5:      Moldova, Republic of   2005–2009   90.06   67.21   9.94     60.33
    ## 6:       Palestine, State of   2010–2014   98.00   92.30   2.00     89.30
    ## 7:        Russian Federation   2010–2014   86.83   68.56  13.17     68.43
    ## 8:                  Viet Nam   2005–2009   92.89   69.17   7.11     59.40
    ## 9:                     Yemen   2010–2014   97.80   92.10   2.20     87.40
    ##    economic educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem
    ## 1:     8.73        1.81    12.01  94.99074  94.10549  92.00298  91.82684
    ## 2:    88.86       55.42    78.69 346.45487 333.03926 318.53806 305.98136
    ## 3:    54.33       25.67    58.27 155.91408 143.80539 132.67130 120.85549
    ## 4:    71.53       41.00    81.73 546.88719 553.71417 543.25326 536.85221
    ## 5:    58.80       16.73    65.20 513.32695 491.99307 497.11512 484.52829
    ## 6:    79.50       26.70    83.50 367.37503 362.07740 355.82036 350.58586
    ## 7:    58.77       22.66    50.02 569.51494 527.82419 500.58817 492.70377
    ## 8:    62.49       20.36    70.56 256.62741 256.62311 255.97453 255.19294
    ## 9:    87.20       45.30    81.00 493.05999 489.24403 485.10075 483.33838
    ##     cvd09fem  cvd10fem  cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem
    ## 1:  93.03548  93.64847  94.72272  94.77537  95.38794  95.79920  96.76678
    ## 2: 298.01274 290.36702 283.44570 278.28489 277.07263 275.18836 277.67423
    ## 3: 111.78620 105.34493 100.12794  95.58209  90.21631  85.58348  83.53081
    ## 4: 507.41753 488.18726 483.29378 465.05092 442.79747 429.36559 435.06853
    ## 5: 467.91012 459.10667 400.91032 387.36233 372.28763 377.82255 382.27748
    ## 6: 343.97033 341.78855 337.69661 320.17827 314.63758 323.39695 338.52757
    ## 7: 467.54550 459.66489 424.43583 406.44126 391.62857 386.28781 376.79609
    ## 8: 254.46217 252.55867 250.17926 247.05521 243.05616 239.21285 234.94030
    ## 9: 478.34072 472.17590 471.35922 468.99902 467.16132 464.47411 466.86730
    ##     cvd16fem  cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male
    ## 1:  96.28366  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052
    ## 2: 278.51259 277.88105 271.04348 268.72894  400.2154  389.5281  376.5086
    ## 3:  81.91117  81.88305  82.38176  83.11824  190.0032  178.2896  167.8587
    ## 4: 415.09331 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687
    ## 5: 381.40961 368.12912 361.05127 341.11337  702.8658  671.8424  666.6718
    ## 6: 350.93555 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858
    ## 7: 366.11070 348.59567 349.51106 351.22730  956.8934  876.9782  830.0730
    ## 8: 230.61406 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026
    ## 9: 466.88438 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200
    ##    cvd08male cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male
    ## 1:  126.3548  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398
    ## 2:  362.0445  350.5229  338.1660  323.1285  311.8184  304.8604  299.1565
    ## 3:  158.2836  149.4912  143.7630  137.6913  131.7820  123.9082  117.5575
    ## 4:  761.4547  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342
    ## 5:  648.2151  638.1296  641.1604  572.9933  565.3869  534.3178  552.1560
    ## 6:  484.7670  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364
    ## 7:  825.6957  776.4243  770.5636  706.6609  675.9903  650.0326  646.2977
    ## 8:  455.8597  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765
    ## 9:  566.4953  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702
    ##    cvd15male cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both
    ## 1:  119.3653  118.6682  117.7883  116.9475  115.8910  114.4628  112.7694
    ## 2:  300.1241  297.2979  294.9042  290.2879  288.5816  374.2741  361.9923
    ## 3:  114.1778  110.9073  110.6175  108.2689  108.8687  172.0355  160.0969
    ## 4:  644.2163  618.5638  613.3843  580.3176  569.3491  634.0089  651.1682
    ## 5:  587.3132  566.6405  523.8055  511.8000  498.5963  589.7769  564.1553
    ## 6:  442.4321  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939
    ## 7:  623.4263  604.2681  561.3748  549.7797  549.1739  727.6035  670.5814
    ## 8:  443.7173  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238
    ## 9:  536.2482  536.1051  541.0473  547.2489  550.6548  535.1088  530.9694
    ##    cvd07both cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both
    ## 1:  110.3326  109.3167  109.3833  109.1886  109.4555  108.8728  108.7915
    ## 2:  347.9814  334.1940  324.1761  313.9673  302.7896  294.4027  290.2060
    ## 3:  148.9444  137.6671  128.5412  122.2008  116.6089  111.6049  105.3768
    ## 4:  645.5907  632.7615  603.3141  581.8522  562.6225  543.9251  519.5371
    ## 5:  566.4516  552.6234  539.0321  535.4923  471.5840  460.0871  438.8196
    ## 6:  415.8933  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001
    ## 7:  635.1627  628.2161  592.7751  585.0586  537.7079  514.5833  495.2656
    ## 8:  336.5239  337.0268  337.1512  335.4539  333.0107  329.9312  326.6030
    ## 9:  525.7088  523.7193  517.1861  508.7890  507.4918  504.3873  501.9957
    ##    cvd14both cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000
    ## 1:  108.3474 108.12924 107.56173 106.95304 106.39570  105.7119          NA
    ## 2:  286.3140 287.96550 286.88535 285.32918 279.68642  277.7336        74.6
    ## 3:  100.1378  97.49873  95.22265  95.17711  94.68896   95.4053        79.8
    ## 4:  513.0161 523.15984 500.14376 493.82082 473.26712  466.3130        70.2
    ## 5:  449.0384 463.27943 456.27984 432.82716 423.77893  404.9551        70.5
    ## 6:  368.8565 380.88863 384.46844 388.17552 386.67955  381.6978          NA
    ## 7:  490.3853 475.65621 461.71309 434.85592 431.69136  432.9186        72.3
    ## 8:  323.1828 319.13626 314.95461 311.23748 307.70802  303.9047        75.6
    ## 9:  497.8142 500.90386 500.85723 504.82748 509.87687  513.0910        64.7
    ##    LE602000 LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019
    ## 1:       NA          NA       NA       84.80       NA          NA       NA
    ## 2:     20.5        78.0     22.3       78.50     22.3        79.1     22.5
    ## 3:     22.7        83.8     25.9       85.10     26.9        86.1     27.9
    ## 4:     18.3        73.3     19.3       75.30     20.2        77.3     21.7
    ## 5:     17.2        73.1     18.2       75.00     19.5        77.1     20.9
    ## 6:       NA          NA       NA       76.22       NA          NA       NA
    ## 7:     18.7        74.7     20.2       76.60     21.4        78.0     22.2
    ## 8:     21.0        77.1     21.5       77.60     21.8        78.1     22.0
    ## 9:     17.9        69.6     18.7       69.60     18.9        68.9     18.7
    ##     phy05  phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13 phy14edit
    ## 1: 3.2319 3.0123 3.0109     NA 3.1479 4.0000     NA     NA     NA    3.3333
    ## 2: 0.8869 0.5361     NA     NA     NA 0.8900     NA     NA     NA    1.5044
    ## 3: 1.7529 1.8047 1.8655 1.8407 1.9185 1.9839 2.0361 2.0798 2.1632    2.2070
    ## 4:     NA     NA     NA     NA     NA     NA     NA     NA     NA        NA
    ## 5: 2.3783 2.3768 2.3871 2.3545 2.3912 2.3808 2.4142 2.4135 2.5028    2.4746
    ## 6:     NA     NA     NA     NA     NA     NA     NA     NA     NA        NA
    ## 7: 2.3204 2.3721 2.3841 2.3812 2.3924 2.3930 6.6305 4.1303 4.0705    4.0114
    ## 8:     NA     NA     NA     NA     NA     NA     NA     NA     NA        NA
    ## 9:     NA     NA     NA     NA     NA     NA     NA     NA     NA        NA
    ##     phy15  phy16  phy17               HE05               HE06
    ## 1: 3.3333     NA     NA 5.5754260999999996 4.9350943599999999
    ## 2: 1.1526     NA 1.1292         5.30572176 5.1986260399999997
    ## 3: 2.2494 2.3037 2.3608 4.6178216900000004 4.9439678200000001
    ## 4:     NA     NA     NA               <NA>               <NA>
    ## 5: 2.4836     NA 3.2066 7.9639606499999998 8.7857027100000007
    ## 6:     NA     NA     NA               <NA>               <NA>
    ## 7: 3.7494 4.0139     NA         4.76693487         4.76170969
    ## 8:     NA     NA     NA               <NA>               <NA>
    ## 9:     NA     NA     NA               <NA>               <NA>
    ##                  HE07               HE08               HE09               HE10
    ## 1: 4.9255475999999998 5.8059859300000003 6.2023372700000001         6.64963865
    ## 2:         5.03977919 5.2816843999999996 6.5595455200000004 6.7547311800000003
    ## 3: 5.1149072599999998         5.39896727 5.7822899799999998 5.9173440900000003
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5: 9.1826009800000001 9.1230525999999994        11.39545822 10.131128309999999
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ## 7: 4.7431106600000001 4.8986678100000001 5.6382026700000001 4.9660777999999999
    ## 8:               <NA>               <NA>               <NA>               <NA>
    ## 9:               <NA>               <NA>               <NA>               <NA>
    ##                  HE11               HE12               HE13               HE14
    ## 1: 6.2465286300000002 6.1015033699999996 5.9878034600000003 5.9791245499999999
    ## 2: 6.6072506899999999 6.6364860500000002         5.99379873 6.9135108000000001
    ## 3:         6.00844383         6.13262033 6.2478942899999996 6.4743809700000003
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5: 9.0967035299999992 9.1396207799999996 8.6830034299999994 8.6327571899999995
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ## 7: 4.7900524100000004 4.9408035300000002 5.0798091899999998 5.1802287099999997
    ## 8:               <NA>               <NA>               <NA>               <NA>
    ## 9:               <NA>               <NA>               <NA>               <NA>
    ##                  HE15               HE16               HE17 scl05 scl06 scl07
    ## 1: 6.2324533500000001 6.3434934600000004 6.5443186799999999   9.8  10.1  10.1
    ## 2: 7.7605791100000001 8.8595066100000004 8.6596641499999993    NA    NA    NA
    ## 3: 6.6527166400000004 6.9143271400000001         7.10694933    NA    NA    NA
    ## 4:               <NA>               <NA>               <NA>  10.2  10.2  10.3
    ## 5: 8.5576353100000002 7.5355224600000001 7.0132851599999997    NA    NA    NA
    ## 6:               <NA>               <NA>               <NA>    NA    NA    NA
    ## 7: 5.2956042300000004 5.2652196900000003         5.34388065    NA    NA    NA
    ## 8:               <NA>               <NA>               <NA>    NA    NA    NA
    ## 9:               <NA>               <NA>               <NA>   1.9   2.0   2.2
    ##    scl08 scl09 scl10 scl11 scl12 scl13 scl14 scl15 scl16 scl17     GDP05
    ## 1:  10.1  10.1  10.1  10.2  10.2  10.2  10.2  10.2  10.2  10.2 40066.257
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA        NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA        NA
    ## 4:  10.3  10.4  10.6  10.6  10.7  10.7  10.8  10.8  10.9  10.9        NA
    ## 5:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA        NA
    ## 6:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA        NA
    ## 7:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA  5323.463
    ## 8:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA        NA
    ## 9:   2.3   2.5   2.6   2.8   3.0   3.0   3.0   3.0   3.0   3.0        NA
    ##        GDP06     GDP07    GDP08     GDP09    GDP10    GDP11    GDP12    GDP13
    ## 1: 42675.813 47803.694 48718.50 43503.186 40852.67 43335.33 38686.46 39538.77
    ## 2:        NA        NA       NA        NA       NA       NA       NA       NA
    ## 3:        NA        NA       NA        NA       NA       NA       NA       NA
    ## 4:        NA        NA       NA        NA       NA       NA       NA       NA
    ## 5:        NA        NA       NA        NA       NA       NA       NA       NA
    ## 6:        NA        NA       NA        NA       NA       NA       NA       NA
    ## 7:  6920.189  9101.255 11635.27  8562.813 10675.00 14311.08 15420.87 15974.64
    ## 8:        NA        NA       NA        NA       NA       NA       NA       NA
    ## 9:        NA        NA       NA        NA       NA       NA       NA       NA
    ##       GDP14     GDP15     GDP16    GDP17 MMR05 MMR06 MMR07 MMR08 MMR09 MMR10
    ## 1: 41303.93 35762.523 37474.665 38962.88    NA    NA    NA    NA    NA    NA
    ## 2:       NA        NA        NA       NA    34    32    30    28    25    22
    ## 3:       NA        NA        NA       NA    15    14    15    15    16    15
    ## 4:       NA        NA        NA       NA    NA    NA    NA    NA    NA    NA
    ## 5:       NA        NA        NA       NA    34    31    31    29    28    29
    ## 6:       NA        NA        NA       NA    NA    NA    NA    NA    NA    NA
    ## 7: 14095.65  9313.014  8704.898 10720.33    42    36    32    30    27    25
    ## 8:       NA        NA        NA       NA    NA    NA    NA    NA    NA    NA
    ## 9:       NA        NA        NA       NA    NA    NA    NA    NA    NA    NA
    ##    MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17               LMH09
    ## 1:    NA    NA    NA    NA    NA    NA    NA         high income
    ## 2:    19    18    17    17    17    16    16 upper middle income
    ## 3:    14    13    13    12    12    11    11         high income
    ## 4:    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 5:    21    22    21    23    22    20    19 lower middle income
    ## 6:    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 7:    23    22    20    19    18    18    17 upper middle income
    ## 8:    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 9:    NA    NA    NA    NA    NA    NA    NA                <NA>
    ##                  LMH14
    ## 1:         high income
    ## 2: upper middle income
    ## 3:         high income
    ## 4:                <NA>
    ## 5: lower middle income
    ## 6:                <NA>
    ## 7:         high income
    ## 8:                <NA>
    ## 9:                <NA>

``` r
#now have 10 rows with one missing either outcome or confounding. 

analysis14 <- edit_phy14[, c("location_name", "GSNI_PERIOD", "twobias", "cvd14fem", "cvd14both", "LEbirth2015", "phy14edit", "scl14", "GDP14", "MMR14")]
glimpse(analysis14)
```

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

``` r
# table name = analysis14
```

\#\#\#2017

``` r
#examine NAs for 2017
edit17 <- cbind(gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh)

na.omit(edit17, c("cvd17fem", "cvd17both", "LEbirth2019", "phy17", "scl17", "GDP17", "MMR17"), invert = TRUE) #30 rows with one missing value for 2017
```

    ##                 location_name GSNI_PERIOD onebias twobias nobias political
    ##  1:                   Andorra   2005–2009   27.01    7.43  72.99     14.08
    ##  2:                Azerbaijan   2010–2014   99.14   93.82   0.86     85.13
    ##  3:                   Belarus   2010–2014   90.37   71.70   9.63     77.82
    ##  4:                  Bulgaria   2005–2009   76.84   44.40  23.16     53.67
    ##  5:                    Cyprus   2010–2014   81.05   49.44  18.95     48.14
    ##  6:                   Ecuador   2010–2014   93.34   58.90   6.66     46.34
    ##  7:                   Finland   2005–2009   51.16   22.67  48.84     24.58
    ##  8:                     Haiti   2010–2014   98.91   92.82   1.09     76.33
    ##  9: Iran, Islamic Republic of   2005–2009   98.54   92.49   1.46     84.63
    ## 10:                     Japan   2010–2014   68.81   41.67  31.19     46.87
    ## 11:                Kazakhstan   2010–2014   96.22   79.02   3.78     75.22
    ## 12:       Korea (Republic of)   2010–2014   87.07   62.91  12.93     63.68
    ## 13:                    Kuwait   2010–2014   97.77   91.56   2.23     88.10
    ## 14:                Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80
    ## 15:                  Malaysia   2010–2014   98.54   88.38   1.46     79.69
    ## 16:                      Mali   2005–2009   98.82   93.36   1.18     81.89
    ## 17:      Moldova, Republic of   2005–2009   90.06   67.21   9.94     60.33
    ## 18:                   Nigeria   2010–2014   99.73   94.99   0.27     85.83
    ## 19:       Palestine, State of   2010–2014   98.00   92.30   2.00     89.30
    ## 20:                      Peru   2010–2014   87.96   49.99  12.04     38.44
    ## 21:        Russian Federation   2010–2014   86.83   68.56  13.17     68.43
    ## 22:                    Serbia   2005–2009   82.62   48.61  17.38     47.05
    ## 23:                 Singapore   2010–2014   92.34   73.20   7.66     76.18
    ## 24:                    Sweden   2010–2014   30.01   10.75  69.99     16.05
    ## 25:                   Ukraine   2010–2014   86.53   65.40  13.47     62.63
    ## 26:            United Kingdom   2005–2009   54.60   25.50  45.40     26.07
    ## 27:                Uzbekistan   2010–2014   97.93   87.73   2.07     78.67
    ## 28:                  Viet Nam   2005–2009   92.89   69.17   7.11     59.40
    ## 29:                     Yemen   2010–2014   97.80   92.10   2.20     87.40
    ## 30:                    Zambia   2005–2009   96.84   80.56   3.16     66.04
    ##                 location_name GSNI_PERIOD onebias twobias nobias political
    ##     economic educational physical   cvd05fem   cvd06fem  cvd07fem  cvd08fem
    ##  1:     8.73        1.81    12.01   94.99074   94.10549  92.00298  91.82684
    ##  2:    91.97       30.90    72.16  653.88539  662.74433 659.53238 654.59319
    ##  3:    58.45       21.19    55.52  476.75969  456.91769 436.00228 424.59960
    ##  4:    37.13       10.95    39.42  563.51958  553.94503 534.14462 513.25045
    ##  5:    43.85       14.03    53.31  250.96373  242.99750 231.03900 217.51610
    ##  6:    36.44       23.46    84.36  161.28268  164.29742 163.35005 160.95424
    ##  7:    23.08        6.22    29.69  176.62785  169.44401 166.12157 162.55154
    ##  8:    72.06       59.91    88.13  526.03693  523.13684 519.11249 514.70529
    ##  9:    88.86       55.42    78.69  346.45487  333.03926 318.53806 305.98136
    ## 10:    41.79       16.21    26.28   80.62892   77.54224  74.58392  72.08078
    ## 11:    67.54       21.71    68.51  602.99412  598.51899 592.41855 573.33154
    ## 12:    54.33       25.67    58.27  155.91408  143.80539 132.67130 120.85549
    ## 13:    77.13       36.45    83.12  181.89122  175.94790 179.89550 194.47948
    ## 14:    71.53       41.00    81.73  546.88719  553.71417 543.25326 536.85221
    ## 15:    74.54       43.00    94.31  278.24360  271.41947 262.56217 262.28229
    ## 16:    88.87       47.61    84.87  320.56474  319.77363 317.97481 317.98693
    ## 17:    58.80       16.73    65.20  513.32695  491.99307 497.11512 484.52829
    ## 18:    83.42       46.18    92.78  277.35636  271.85405 263.52070 259.79173
    ## 19:    79.50       26.70    83.50  367.37503  362.07740 355.82036 350.58586
    ## 20:    27.05       14.36    79.76   97.82452   91.17324  84.14951  83.74437
    ## 21:    58.77       22.66    50.02  569.51494  527.82419 500.58817 492.70377
    ## 22:    35.49       13.20    66.56  561.66401  547.13295 531.03292 528.63016
    ## 23:    52.23       26.18    65.66  123.75950  119.41610 114.07452 110.44957
    ## 24:     9.16        2.61    14.13  148.45246  147.10784 144.55349 139.74976
    ## 25:    57.69       18.23    56.61  577.57784  553.20902 554.13746 550.85070
    ## 26:    25.15        6.65    30.34  147.03415  138.97711 132.69706 127.54333
    ## 27:    80.33       48.60    83.93 1025.82262 1005.30790 983.98523 997.02332
    ## 28:    62.49       20.36    70.56  256.62741  256.62311 255.97453 255.19294
    ## 29:    87.20       45.30    81.00  493.05999  489.24403 485.10075 483.33838
    ## 30:    55.41       23.53    89.07  301.19131  304.24329 301.25347 299.44540
    ##     economic educational physical   cvd05fem   cvd06fem  cvd07fem  cvd08fem
    ##      cvd09fem   cvd10fem   cvd11fem   cvd12fem   cvd13fem   cvd14fem   cvd15fem
    ##  1:  93.03548   93.64847   94.72272   94.77537   95.38794   95.79920   96.76678
    ##  2: 655.75062  661.62994  662.91437  665.11545  673.97385  688.85984  689.28400
    ##  3: 427.87579  422.21601  428.07519  392.30102  389.72174  383.05228  373.94641
    ##  4: 497.57334  495.06305  483.56995  465.16759  445.93731  456.08244  452.74524
    ##  5: 195.44933  188.74835  188.54621  184.83879  181.23758  181.02862  179.81251
    ##  6: 157.69322  157.35397  149.29192  153.73253  152.93464  152.05617  148.90913
    ##  7: 159.13643  155.55659  151.01600  149.29429  146.09508  142.89271  139.40322
    ##  8: 510.66940  508.21671  512.24372  507.65492  502.18482  497.94515  495.34101
    ##  9: 298.01274  290.36702  283.44570  278.28489  277.07263  275.18836  277.67423
    ## 10:  69.30532   68.18071   67.97138   65.88326   63.94075   62.02763   59.92309
    ## 11: 547.90401  538.84766  523.68062  506.50361  483.03342  462.02411  446.05277
    ## 12: 111.78620  105.34493  100.12794   95.58209   90.21631   85.58348   83.53081
    ## 13: 187.99981  170.86166  155.06885  143.13273  131.57058  127.63836  121.04601
    ## 14: 507.41753  488.18726  483.29378  465.05092  442.79747  429.36559  435.06853
    ## 15: 263.15505  252.21083  237.09570  233.23215  226.51494  228.10326  227.29230
    ## 16: 317.14954  316.24847  314.96309  313.19762  310.27834  307.23772  309.21589
    ## 17: 467.91012  459.10667  400.91032  387.36233  372.28763  377.82255  382.27748
    ## 18: 257.07280  254.32157  253.99749  254.09391  253.49508  249.71126  252.45876
    ## 19: 343.97033  341.78855  337.69661  320.17827  314.63758  323.39695  338.52757
    ## 20:  93.03498   94.03452   91.93528   89.98908   87.97633   83.77122   80.26885
    ## 21: 467.54550  459.66489  424.43583  406.44126  391.62857  386.28781  376.79609
    ## 22: 518.35528  493.55030  467.81093  455.97734  447.78602  454.53739  452.53237
    ## 23: 103.82613   99.15298   92.93724   91.20203   86.55430   83.43653   79.65879
    ## 24: 136.25239  132.65068  130.65338  127.62791  124.02870  120.35028  117.04866
    ## 25: 519.56979  506.60540  481.38459  473.11451  464.17229  436.92347  474.36156
    ## 26: 120.00812  115.83452  112.45472  112.30358  111.23225  108.82079  108.30317
    ## 27: 992.45481 1012.61930 1037.46364 1049.70253 1041.14657 1024.86478 1010.07674
    ## 28: 254.46217  252.55867  250.17926  247.05521  243.05616  239.21285  234.94030
    ## 29: 478.34072  472.17590  471.35922  468.99902  467.16132  464.47411  466.86730
    ## 30: 297.43078  300.60435  303.67459  305.16350  306.41551  307.02384  308.97725
    ##      cvd09fem   cvd10fem   cvd11fem   cvd12fem   cvd13fem   cvd14fem   cvd15fem
    ##      cvd16fem  cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male
    ##  1:  96.28366  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052
    ##  2: 688.89897 685.80022 648.14175 627.27975  795.4780  806.0879  797.5581
    ##  3: 375.58732 373.29793 365.27597 362.81115  841.2488  822.3199  768.8990
    ##  4: 444.74854 440.64650 452.67136 454.55472  814.3341  795.8899  767.4014
    ##  5: 178.14639 179.63631 174.81338 172.41410  340.5299  338.3851  330.8774
    ##  6: 149.01126 149.63616 148.94035 146.37173  195.4146  197.5540  198.2290
    ##  7: 136.26260 136.03946 135.91504 135.37010  292.7180  285.9363  280.4463
    ##  8: 489.48808 483.52984 479.46926 475.27316  431.5242  430.9686  431.2913
    ##  9: 278.51259 277.88105 271.04348 268.72894  400.2154  389.5281  376.5086
    ## 10:  59.05503  57.80935  58.18790  58.35637  136.7445  131.7984  128.0151
    ## 11: 433.43107 408.05109 402.99006 401.45939  929.4738  926.9272  918.2064
    ## 12:  81.91117  81.88305  82.38176  83.11824  190.0032  178.2896  167.8587
    ## 13: 120.12173 120.95883 121.22492 124.78355  282.0308  283.0347  271.4806
    ## 14: 415.09331 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687
    ## 15: 224.59851 222.86437 221.09851 220.62850  326.1480  318.3576  312.0718
    ## 16: 307.72590 305.13664 304.19901 301.76476  250.4399  251.6409  253.7490
    ## 17: 381.40961 368.12912 361.05127 341.11337  702.8658  671.8424  666.6718
    ## 18: 247.75393 245.20993 242.25928 239.99721  274.4590  267.8654  259.5376
    ## 19: 350.93555 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858
    ## 20:  78.92477  78.51316  77.75822  77.12523  118.1332  114.0556  109.6057
    ## 21: 366.11070 348.59567 349.51106 351.22730  956.8934  876.9782  830.0730
    ## 22: 439.25670 434.70288 432.93221 428.30908  605.1301  591.9054  575.6820
    ## 23:  76.29249  74.34356  73.94099  73.62812  186.6925  179.9437  176.8463
    ## 24: 116.75578 115.86243 116.73812 114.02886  236.4315  227.9933  219.7105
    ## 25: 460.24398 455.73238 466.00037 465.00257  906.8034  875.1121  894.2645
    ## 26: 106.14944 104.51284 106.93075 107.12455  224.9909  213.8383  203.5031
    ## 27: 975.22891 945.26316 908.52364 856.84712 1349.0110 1313.7526 1257.9630
    ## 28: 230.61406 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026
    ## 29: 466.88438 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200
    ## 30: 311.38853 308.67000 306.10250 303.60330  396.9908  395.9378  389.5186
    ##      cvd16fem  cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male
    ##     cvd08male cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male
    ##  1:  126.3548  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398
    ##  2:  795.9201  792.0192  792.6029  806.8414  826.6460  825.9170  835.2344
    ##  3:  771.9881  770.1596  779.5070  788.8996  710.1381  699.5572  687.9125
    ##  4:  740.1393  720.9119  704.5502  686.3721  666.1790  649.1804  660.2319
    ##  5:  312.6105  308.6625  303.2067  287.3138  275.9141  262.2377  252.0967
    ##  6:  197.6351  191.5033  191.5863  191.7630  187.9508  186.7919  185.9278
    ##  7:  271.9016  266.2553  260.7571  252.2236  244.8651  238.2376  231.7559
    ##  8:  431.2199  430.6858  431.0238  433.7781  433.2272  432.0381  430.4305
    ##  9:  362.0445  350.5229  338.1660  323.1285  311.8184  304.8604  299.1565
    ## 10:  124.7603  121.7149  119.7500  117.8518  114.2759  110.5415  106.8358
    ## 11:  877.5018  824.7192  809.5292  791.3657  766.3255  730.3873  693.7872
    ## 12:  158.2836  149.4912  143.7630  137.6913  131.7820  123.9082  117.5575
    ## 13:  295.6581  273.0090  237.9775  221.1212  224.7644  218.0751  208.6838
    ## 14:  761.4547  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342
    ## 15:  316.5872  315.2901  303.3995  295.3241  285.1897  264.2568  267.6031
    ## 16:  256.6272  259.4479  259.6777  258.0625  256.5280  255.5163  253.8127
    ## 17:  648.2151  638.1296  641.1604  572.9933  565.3869  534.3178  552.1560
    ## 18:  255.2531  252.9699  250.7906  250.3251  250.0944  248.4156  244.0515
    ## 19:  484.7670  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364
    ## 20:  107.1766  117.1724  120.0147  118.2343  116.0098  113.0409  107.4914
    ## 21:  825.6957  776.4243  770.5636  706.6609  675.9903  650.0326  646.2977
    ## 22:  567.4146  548.0041  524.2160  503.8235  494.4056  513.2331  517.1577
    ## 23:  168.2657  159.8412  154.4816  147.9723  141.9160  136.9070  131.4295
    ## 24:  211.9112  205.6428  198.6352  193.1647  187.9331  181.3070  174.5094
    ## 25:  895.7625  817.5231  795.2222  763.1596  754.7471  743.9361  691.8518
    ## 26:  195.1599  185.3431  178.1320  171.7848  168.7043  167.4859  163.1700
    ## 27: 1247.2106 1227.9049 1230.5979 1242.3562 1242.5922 1222.1098 1205.2298
    ## 28:  455.8597  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765
    ## 29:  566.4953  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702
    ## 30:  383.8555  377.2154  376.4333  380.5640  379.4144  379.6064  381.3379
    ##     cvd08male cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male
    ##     cvd15male cvd16male  cvd17male cvd18male  cvd19male cvd05both cvd06both
    ##  1:  119.3653  118.6682  117.78826  116.9475  115.89102  114.4628  112.7694
    ##  2:  833.7264  842.2704  843.82968  784.6385  760.37239  725.8544  735.0585
    ##  3:  667.1438  655.9036  654.16797  631.3911  624.05795  615.4166  593.5400
    ##  4:  653.9685  652.3623  649.20064  651.8298  649.05077  674.9428  661.4847
    ##  5:  245.5002  222.3039  198.85576  198.3850  199.87770  292.7366  286.9163
    ##  6:  184.5895  186.8145  189.48573  189.0448  182.28777  177.6049  180.0888
    ##  7:  225.2625  223.1535  223.00722  220.6973  219.35925  226.8430  219.8546
    ##  8:  429.9405  426.5772  424.13334  423.1650  419.78532  481.0125  479.0757
    ##  9:  300.1241  297.2979  294.90423  290.2879  288.58158  374.2741  361.9923
    ## 10:  104.0517  101.9864   99.50671   98.7673   98.55422  104.9456  101.2200
    ## 11:  668.8549  644.9578  595.59312  606.7502  583.04195  739.1100  735.1324
    ## 12:  114.1778  110.9073  110.61751  108.2689  108.86874  172.0355  160.0969
    ## 13:  215.4350  218.6227  221.07636  221.7215  225.49442  244.9834  243.3229
    ## 14:  644.2163  618.5638  613.38427  580.3176  569.34914  634.0089  651.1682
    ## 15:  268.1236  275.7751  278.45188  279.6520  290.33018  304.0469  296.6711
    ## 16:  257.2386  257.6357  257.62900  255.5112  252.29479  284.7410  284.8986
    ## 17:  587.3132  566.6405  523.80550  511.8000  498.59630  589.7769  564.1553
    ## 18:  246.2866  241.7979  238.75676  235.4675  232.25723  278.5825  272.1292
    ## 19:  442.4321  433.5059  438.10501  440.8897  433.97986  434.1983  427.1939
    ## 20:  103.9618  102.7772  102.27404  101.7696  101.14597  107.5290  102.1196
    ## 21:  623.4263  604.2681  561.37484  549.7797  549.17392  727.6035  670.5814
    ## 22:  508.8793  479.5325  475.06741  473.5821  468.65816  581.1130  566.8383
    ## 23:  126.8360  122.9414  119.67184  119.1105  115.46880  152.9234  147.4189
    ## 24:  171.7927  168.7297  166.76857  165.8889  165.85687  186.9808  182.9116
    ## 25:  816.2680  779.5641  770.54121  792.6953  786.12722  705.0232  676.2279
    ## 26:  161.8943  159.0024  157.59097  160.8536  161.23918  181.8112  172.5648
    ## 27: 1190.7704 1157.1051 1127.84654 1097.1030 1059.62943 1156.2007 1131.6463
    ## 28:  443.7173  440.1349  436.88548  433.6069  429.93624  334.7621  335.3238
    ## 29:  536.2482  536.1051  541.04732  547.2489  550.65478  535.1088  530.9694
    ## 30:  385.9456  390.7901  386.09677  382.3781  377.53367  348.3714  349.1627
    ##     cvd15male cvd16male  cvd17male cvd18male  cvd19male cvd05both cvd06both
    ##      cvd07both  cvd08both  cvd09both cvd10both  cvd11both  cvd12both  cvd13both
    ##  1:  110.33258  109.31671  109.38333  109.1886  109.45548  108.87280  108.79150
    ##  2:  728.35144  724.46736  722.60586  725.7938  732.17587  739.57962  744.00434
    ##  3:  562.05477  555.95422  557.54021  557.6104  565.03762  511.81059  505.55688
    ##  4:  637.59710  613.86478  596.49917  588.2128  574.25279  554.67232  536.10535
    ##  5:  276.58795  260.43612  243.58089  235.8788  228.93682  222.03099  215.59362
    ##  6:  179.95002  178.65734  174.14429  173.9953  169.80081  170.33946  169.40528
    ##  7:  216.02209  210.59758  206.37477  202.0771  195.89295  191.83921  187.40416
    ##  8:  477.03676  474.64371  472.25177  471.2477  474.82817  472.17919  468.77018
    ##  9:  347.98144  334.19398  324.17606  313.9673  302.78960  294.40271  290.20596
    ## 10:   98.02780   95.29858   92.50782   91.1429   90.36691   87.76339   85.18238
    ## 11:  727.00327  698.76475  663.07312  651.3492  633.77423  612.75404  584.05483
    ## 12:  148.94438  137.66707  128.54118  122.2008  116.60889  111.60489  105.37675
    ## 13:  237.55346  258.02380  241.09178  212.5469  195.85098  193.39730  184.59528
    ## 14:  645.59071  632.76151  603.31409  581.8522  562.62250  543.92509  519.53712
    ## 15:  289.07676  291.31778  291.24859  279.5220  267.40258  260.45450  246.45538
    ## 16:  285.02779  286.46916  287.49451  287.1683  285.69447  284.03384  282.08967
    ## 17:  566.45157  552.62338  539.03210  535.4923  471.58396  460.08707  438.81955
    ## 18:  263.44130  259.09633  256.29837  253.4984  252.80350  252.46797  251.08995
    ## 19:  415.89329  407.15878  399.09041  394.3262  387.97737  366.84516  355.90010
    ## 20:   96.34764   94.97714  104.62926  106.4933  104.54989  102.44656   99.95400
    ## 21:  635.16266  628.21607  592.77514  585.0586  537.70788  514.58328  495.26564
    ## 22:  550.81185  545.61221  531.45941  507.4926  484.72607  474.11523  477.56554
    ## 23:  143.05822  137.56764  130.09757  125.0091  118.70562  115.15732  110.43077
    ## 24:  178.18444  172.12691  167.47419  162.5909  159.25538  155.26815  150.44817
    ## 25:  684.18530  682.22877  631.59255  614.4575  586.67859  578.93263  569.92172
    ## 26:  164.68453  158.18593  149.60425  144.0946  139.38594  137.93744  136.84127
    ## 27: 1103.74692 1112.63928 1102.33911 1116.2918 1135.63642 1141.29449 1125.34402
    ## 28:  336.52387  337.02684  337.15124  335.4539  333.01066  329.93117  326.60300
    ## 29:  525.70885  523.71933  517.18606  508.7890  507.49177  504.38733  501.99569
    ## 30:  344.28588  340.42203  336.02994  337.2284  340.77995  340.99547  341.73548
    ##      cvd07both  cvd08both  cvd09both cvd10both  cvd11both  cvd12both  cvd13both
    ##      cvd14both  cvd15both  cvd16both  cvd17both cvd18both cvd19both LEbirth2000
    ##  1:  108.34739  108.12924  107.56173  106.95304 106.39570 105.71185          NA
    ##  2:  757.21682  756.21653  758.33069  756.75780 710.38982 688.07822        68.5
    ##  3:  497.23919  484.40187  481.77658  479.93207 466.89588 463.10778        74.6
    ##  4:  546.62593  541.93803  536.54177  532.73637 541.03249 541.08270        75.0
    ##  5:  212.30069  209.95313  202.90815  196.71239 192.06049 190.26941        81.0
    ##  6:  168.52380  166.17046  167.16092  168.67782 168.10363 163.65500        77.3
    ##  7:  182.96093  178.26308  175.55012  175.44476 174.60136 173.80771        80.9
    ##  8:  465.82635  464.27802  459.66137  455.40585 452.87085 449.11377        57.2
    ##  9:  286.31401  287.96550  286.88535  285.32918 279.68642 277.73359        74.6
    ## 10:   82.57175   80.18604   78.84510   77.08885  77.00931  77.00994        84.4
    ## 11:  556.36666  536.19078  519.14503  485.39932 485.31026 476.01008        69.2
    ## 12:  100.13781   97.49873   95.22265   95.17711  94.68896  95.40530        79.8
    ## 13:  177.12714  178.35299  179.50619  181.00846 181.17922 184.57616        80.1
    ## 14:  513.01612  523.15984  500.14376  493.82082 473.26712 466.31300        70.2
    ## 15:  248.03609  247.15687  249.70380  250.28741 250.11953 255.54454        75.3
    ## 16:  279.73392  282.44832  281.89484  280.58319 279.03154 276.18486        52.7
    ## 17:  449.03836  463.27943  456.27984  432.82716 423.77893 404.95508        70.5
    ## 18:  246.82989  249.11815  244.38574  241.54042 238.38554 235.65202        55.2
    ## 19:  368.85654  380.88863  384.46844  388.17552 386.67955 381.69777          NA
    ## 20:   95.08345   91.54823   90.28000   89.85563  89.23082  88.61384        75.9
    ## 21:  490.38527  475.65621  461.71309  434.85592 431.69136 432.91864        72.3
    ## 22:  482.73602  478.06378  458.04063  453.96042 453.05802 448.83636        74.3
    ## 23:  106.32256  102.22058   98.56112   95.85146  95.34721  93.68210        80.7
    ## 24:  145.54025  142.50612  141.00249  139.76469 140.20408 138.59168        81.8
    ## 25:  534.81353  609.75995  588.08212  581.24578 594.30207 591.00624        73.2
    ## 26:  133.69815  132.99741  130.60903  129.12143 132.02638 132.37531          NA
    ## 27: 1107.01301 1091.31310 1056.26572 1026.66904 992.40366 945.97305        68.2
    ## 28:  323.18282  319.13626  314.95461  311.23748 307.70802 303.90466        75.6
    ## 29:  497.81417  500.90386  500.85723  504.82748 509.87687 513.09104        64.7
    ## 30:  342.88671  346.10827  349.73020  346.15135 343.09096 339.55436        45.2
    ##      cvd14both  cvd15both  cvd16both  cvd17both cvd18both cvd19both LEbirth2000
    ##     LE602000 LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019
    ##  1:       NA          NA       NA          NA       NA          NA       NA
    ##  2:     17.9        71.8     17.6        73.4     17.9        74.1     18.3
    ##  3:     19.4        76.4     20.5        78.8     21.9        79.6     22.5
    ##  4:     19.2        77.2     20.9        78.0     21.5        78.6     22.0
    ##  5:     23.2        83.1     24.8        84.1     25.6        85.1     26.4
    ##  6:     23.1        77.9     22.8        79.5     23.6        80.5     24.3
    ##  7:     23.6        83.0     25.3        83.9     25.7        84.0     25.8
    ##  8:     15.9        35.4     12.0        63.1     16.7        64.8     17.0
    ##  9:     20.5        78.0     22.3        78.5     22.3        79.1     22.5
    ## 10:     26.6        85.8     27.7        86.4     28.1        86.9     28.6
    ## 11:     17.8        73.4     19.2        76.2     20.6        77.6     21.4
    ## 12:     22.7        83.8     25.9        85.1     26.9        86.1     27.9
    ## 13:     23.5        81.4     24.0        83.8     25.9        83.9     25.9
    ## 14:     18.3        73.3     19.3        75.3     20.2        77.3     21.7
    ## 15:     19.2        76.5     20.1        77.1     20.7        77.1     20.6
    ## 16:     15.9        59.5     17.0        61.4     17.2        63.4     17.6
    ## 17:     17.2        73.1     18.2        75.0     19.5        77.1     20.9
    ## 18:     17.0        60.8     18.0        62.8     18.5        64.1     18.9
    ## 19:       NA          NA       NA          NA       NA          NA       NA
    ## 20:     22.9        78.9     23.8        80.6     24.7        81.3     25.1
    ## 21:     18.7        74.7     20.2        76.6     21.4        78.0     22.2
    ## 22:     18.4        76.8     20.0        77.8     20.7        78.3     21.1
    ## 23:     23.1        84.0     25.8        84.9     26.7        85.5     27.2
    ## 24:     24.1        83.1     25.0        83.5     25.3        84.0     25.6
    ## 25:     18.8        75.1     19.8        77.0     21.1        77.8     21.7
    ## 26:       NA          NA       NA          NA       NA          NA       NA
    ## 27:     17.1        72.9     18.7        73.9     19.0        75.2     19.8
    ## 28:     21.0        77.1     21.5        77.6     21.8        78.1     22.0
    ## 29:     17.9        69.6     18.7        69.6     18.9        68.9     18.7
    ## 30:     14.7        59.1     17.1        63.0     17.7        65.4     18.0
    ##     LE602000 LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019
    ##      phy05  phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14
    ##  1: 3.2319 3.0123 3.0109     NA 3.1479 4.0000     NA     NA     NA     NA
    ##  2: 3.5819 3.5649 3.7124 3.6844 3.6751 3.6629 3.4376 3.4901 3.4558 3.4460
    ##  3: 3.3145 3.3795 3.5306 3.1477 3.2845 3.2498 4.7611 4.8352 4.8534 5.0120
    ##  4: 3.6651 3.6805 3.6800 3.6488 3.7401 3.7661 3.8467 3.8995 3.9630 3.9750
    ##  5: 1.8965 1.8374 2.0147 2.0507 2.1064 2.1562 2.2484 2.2960 2.4076 2.4993
    ##  6:     NA     NA     NA     NA 1.5983 2.1111 1.6582     NA     NA     NA
    ##  7: 2.9913 3.0262 3.0351 3.0618 3.0861 3.2653 3.1221 3.2854 3.2966 3.3922
    ##  8:     NA     NA     NA     NA     NA     NA 0.1378     NA     NA     NA
    ##  9: 0.8869 0.5361     NA     NA     NA 0.8900     NA     NA     NA 1.5044
    ## 10:     NA 2.0746     NA 2.1394     NA 2.2059     NA 2.2740     NA 2.3413
    ## 11: 3.6038 3.6983 3.6547 3.7161 3.7808 3.9278 3.9483 3.9108 3.9508 3.9800
    ## 12: 1.7529 1.8047 1.8655 1.8407 1.9185 1.9839 2.0361 2.0798 2.1632 2.2070
    ## 13: 1.8000 1.4543 1.8814 1.8806 1.8929 2.4296 2.4560 2.4662 2.5332 2.6522
    ## 14:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 15:     NA     NA     NA 0.9216     NA 1.1691 1.2777 1.3320     NA     NA
    ## 16:     NA     NA 0.0776 0.0490 0.0923 0.1021 0.1037 0.1071     NA     NA
    ## 17: 2.3783 2.3768 2.3871 2.3545 2.3912 2.3808 2.4142 2.4135 2.5028 2.4746
    ## 18: 0.2824 0.3481 0.3784 0.3762 0.3782 0.1836     NA     NA 0.3828     NA
    ## 19:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 20:     NA     NA 1.6648     NA 0.9472 0.9200     NA 1.1411     NA     NA
    ## 21: 2.3204 2.3721 2.3841 2.3812 2.3924 2.3930 6.6305 4.1303 4.0705 4.0114
    ## 22: 2.2609 2.2858 2.3447 2.4215 2.4467 2.4820 2.5003 2.4961 2.4945 2.4611
    ## 23: 1.5819 1.5744 1.6127 1.6418 1.6758 1.7187 1.7232 1.7964 1.8958 2.0125
    ## 24: 3.5107 3.5960 3.6760 3.7353 3.8065 3.8779 3.9569 4.0368 4.1259 4.2037
    ## 25: 3.0181 3.0838 3.0885 3.1177 3.4904 3.4830 3.4842 3.4910 3.4965 2.9923
    ## 26: 2.4120 2.4590 2.4844 2.5543 2.6279 2.6265 2.6603 2.6667 2.6774 2.7149
    ## 27: 2.6701 2.6515 2.6329 2.6048 2.5706 2.5432 2.5036 2.4451 2.4044 2.3742
    ## 28:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 29:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 30: 0.0545 0.0533     NA 0.0619 0.0606 0.0614 0.1649 0.1658     NA     NA
    ##      phy05  phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14
    ##      phy15  phy16  phy17               HE05               HE06
    ##  1: 3.3333     NA     NA 5.5754260999999996 4.9350943599999999
    ##  2:     NA     NA     NA 2.2610766899999999 2.0667839099999998
    ##  3: 5.1905     NA     NA 6.2765383699999999 5.7629585299999997
    ##  4: 4.0332     NA     NA 6.8901143100000004         6.52544594
    ##  5: 2.6236 1.9509     NA 5.4358096099999997 5.4635748900000003
    ##  6: 2.0648 2.0368     NA         5.57841349 5.6872220000000002
    ##  7: 3.3309 3.8118     NA 8.2879562399999998 8.2841176999999995
    ##  8: 0.0852     NA     NA 5.5093555500000004 5.5010376000000001
    ##  9: 1.1526     NA 1.1292         5.30572176 5.1986260399999997
    ## 10:     NA 2.4115     NA 7.7806687400000003         7.80784273
    ## 11:     NA     NA     NA 3.9017238600000002         3.39824891
    ## 12: 2.2494 2.3037 2.3608 4.6178216900000004 4.9439678200000001
    ## 13: 2.6463     NA     NA 2.3776223700000001         2.25313878
    ## 14:     NA     NA     NA               <NA>               <NA>
    ## 15: 1.5358     NA     NA 2.8012416400000002 3.1245324600000002
    ## 16:     NA 0.1395     NA 5.1953811600000002 5.4565606100000004
    ## 17: 2.4836     NA 3.2066 7.9639606499999998 8.7857027100000007
    ## 18:     NA 0.4494     NA 4.4659194900000001 4.2577514599999997
    ## 19:     NA     NA     NA               <NA>               <NA>
    ## 20:     NA 1.3048     NA 4.5863194500000004 4.5380153700000001
    ## 21: 3.7494 4.0139     NA         4.76693487         4.76170969
    ## 22: 2.4603 3.1131     NA 8.2838058500000002         8.49389839
    ## 23:     NA 2.2936     NA         3.03367925 2.9347465000000001
    ## 24: 4.2856 3.9840     NA 8.1469831500000005 8.0473642299999995
    ## 25:     NA     NA     NA 6.3461584999999996 6.4169516599999996
    ## 26: 2.7465 2.7563 2.7863 8.5339870500000004 8.6973781599999995
    ## 27:     NA     NA     NA 4.8773298299999999 4.8463411299999999
    ## 28:     NA     NA     NA               <NA>               <NA>
    ## 29:     NA     NA     NA               <NA>               <NA>
    ## 30:     NA 0.1628     NA 6.8633222600000003 5.8736734400000001
    ##      phy15  phy16  phy17               HE05               HE06
    ##                   HE07               HE08               HE09               HE10
    ##  1: 4.9255475999999998 5.8059859300000003 6.2023372700000001         6.64963865
    ##  2:         1.99021435         1.93880868 2.6137836000000001 2.4899666300000001
    ##  3: 5.8853402099999998         5.47281408 5.3903784799999999 5.6569399799999998
    ##  4: 6.1603593800000001 6.2877101900000003 6.5988688499999997         7.14063406
    ##  5: 5.3574485799999998         6.01427698 6.4653158199999998 6.5176711100000002
    ##  6: 5.8648901000000002 5.8163766900000002 6.4387331000000003 7.1219563499999996
    ##  7: 8.0835933700000009 8.3431062699999998 9.1612224599999994 9.1420278499999998
    ##  8: 5.9015803299999998 6.0159010899999998 6.1691598900000004 8.1455984099999998
    ##  9:         5.03977919 5.2816843999999996 6.5595455200000004 6.7547311800000003
    ## 10: 7.8904604899999997 8.1995143899999992 9.0582961999999991 9.1567735700000004
    ## 11: 2.7031483700000001 3.0495264500000001 3.4991376399999998 2.7364139600000001
    ## 12: 5.1149072599999998         5.39896727 5.7822899799999998 5.9173440900000003
    ## 13: 2.1331450900000002         1.93336701 3.8693599700000001 2.7570362099999999
    ## 14:               <NA>               <NA>               <NA>               <NA>
    ## 15:         3.08224082 3.0235738799999998 3.2762939900000001         3.18466234
    ## 16: 5.2536678300000004 5.0454120600000003 5.2291111900000002 4.6792340299999999
    ## 17: 9.1826009800000001 9.1230525999999994        11.39545822 10.131128309999999
    ## 18:         3.90997219 3.6958153199999999 3.5801973299999998 3.2965328700000001
    ## 19:               <NA>               <NA>               <NA>               <NA>
    ## 20: 4.4038014399999996 4.4459280999999997 4.9506058700000004 4.7206191999999998
    ## 21: 4.7431106600000001 4.8986678100000001 5.6382026700000001 4.9660777999999999
    ## 22: 9.3764915500000008 9.4824638399999994 9.3462324100000007 9.5273036999999992
    ## 23:         2.83749056 3.1948845399999999         3.39853096 3.2026536499999998
    ## 24: 7.9901738199999999 8.2066011400000001 8.7731924100000001 8.3273897199999993
    ## 25: 6.0170521700000004 5.4972801200000001 6.6077189399999998 6.8122901899999997
    ## 26: 8.8732242600000006 9.1662340199999992        10.01600361 9.9891519500000001
    ## 27: 4.6331453299999996 4.7983584400000003 4.8924522399999999 5.1175293899999996
    ## 28:               <NA>               <NA>               <NA>               <NA>
    ## 29:               <NA>               <NA>               <NA>               <NA>
    ## 30: 4.3526496899999998 4.0120754200000004         4.42680454 3.7192959800000001
    ##                   HE07               HE08               HE09               HE10
    ##                   HE11               HE12               HE13               HE14
    ##  1: 6.2465286300000002 6.1015033699999996 5.9878034600000003 5.9791245499999999
    ##  2: 2.4464180500000001         2.96481562 3.0367231399999999 3.3771374199999999
    ##  3: 4.8743853599999998 5.2453269999999996 5.6955947900000004 5.3879337300000003
    ##  4: 7.1416268299999999 7.5788183199999999 7.1707920999999999 7.7096777000000003
    ##  5: 6.4492850300000004         6.55474567 6.9451847100000004 6.9609560999999998
    ##  6: 7.8652181600000004 8.4796819699999997 8.5598344799999992 8.6212749500000001
    ##  7:         9.22303009 9.5857954000000003 9.8053464899999998 9.7811508200000006
    ##  8:         10.2313633 9.6682958600000006         7.23788214 7.7976360299999996
    ##  9: 6.6072506899999999 6.6364860500000002         5.99379873 6.9135108000000001
    ## 10: 10.616717339999999        10.79065323        10.79159355        10.83204937
    ## 11: 2.6023666900000002 3.0372598200000001 2.6628675500000001 2.9746842400000002
    ## 12:         6.00844383         6.13262033 6.2478942899999996 6.4743809700000003
    ## 13: 2.6180841899999998         2.57378602 2.5854389699999998 3.1975801000000001
    ## 14:               <NA>               <NA>               <NA>               <NA>
    ## 15: 3.3399648700000002 3.4879069299999998 3.5224008599999999 3.7252702700000002
    ## 16: 4.0157337200000001 3.7521157299999999 3.9675071200000001 4.4815611799999999
    ## 17: 9.0967035299999992 9.1396207799999996 8.6830034299999994 8.6327571899999995
    ## 18: 3.3207793200000002 3.3598427800000001 3.4206934000000002 3.3484041699999998
    ## 19:               <NA>               <NA>               <NA>               <NA>
    ## 20: 4.6209321000000001 4.7565517399999999 4.7019739200000004 4.9947280899999997
    ## 21: 4.7900524100000004 4.9408035300000002 5.0798091899999998 5.1802287099999997
    ## 22:         9.11380005 9.3282098799999993 9.3123245200000007 9.2459878900000003
    ## 23: 3.1579217900000001 3.3278367499999999 3.6856265100000001 3.8715567599999998
    ## 24: 10.421509739999999        10.73550034        10.90368462 10.948073389999999
    ## 25: 6.8161716500000002 7.1166605900000004         6.94156218 7.1896038100000004
    ## 26: 9.9734649700000002 10.051768300000001 9.9785518599999996 9.9575843800000001
    ## 27: 5.2821259500000002 5.5897636400000001 5.6617379200000002         4.67292738
    ## 28:               <NA>               <NA>               <NA>               <NA>
    ## 29:               <NA>               <NA>               <NA>               <NA>
    ## 30: 3.4605324300000002 3.9305291200000001 4.6909103400000003 3.8292422300000002
    ##                   HE11               HE12               HE13               HE14
    ##                   HE15               HE16               HE17 scl05 scl06 scl07
    ##  1: 6.2324533500000001 6.3434934600000004 6.5443186799999999   9.8  10.1  10.1
    ##  2: 4.1066360499999996 4.0365543400000004 3.7365145700000002  10.7  10.7  10.2
    ##  3: 6.0653333700000003 5.9105415299999997         5.75974846   9.3  10.0  10.6
    ##  4: 7.4133834800000002 7.4799427999999999 7.4532680500000001  10.2  10.4  10.5
    ##  5: 6.9256858799999996 6.8342390100000001 6.7382893599999996  10.7  10.9  11.2
    ##  6: 8.5884571100000002 8.2959604299999992 8.2574291199999994   7.3   7.3   7.3
    ##  7: 9.6452388800000008 9.3779602099999995 9.1415662799999993  12.0  12.0  12.0
    ##  8: 8.6285295499999997 8.4368677099999996 8.0846834199999993   4.3   4.4   4.5
    ##  9: 7.7605791100000001 8.8595066100000004 8.6596641499999993    NA    NA    NA
    ## 10:        10.88550663 10.834609990000001        10.79634285  11.2  11.2  11.3
    ## 11: 3.0405108900000002 3.4223556500000001 3.1258840600000002  11.7  11.7  11.6
    ## 12: 6.6527166400000004 6.9143271400000001         7.10694933    NA    NA    NA
    ## 13: 4.0147361799999999 4.0310459099999996 5.2952661499999998   5.8   6.2   6.3
    ## 14:               <NA>               <NA>               <NA>  10.2  10.2  10.3
    ## 15: 3.8160853399999999         3.68835807         3.71246052   7.6   8.2   8.8
    ## 16: 4.1114335100000003 3.7753798999999999 4.1038827900000001   1.7   1.7   1.8
    ## 17: 8.5576353100000002 7.5355224600000001 7.0132851599999997    NA    NA    NA
    ## 18: 3.5819501900000001 3.6477367900000002 3.7555384599999999   5.2   5.2   5.2
    ## 19:               <NA>               <NA>               <NA>    NA    NA    NA
    ## 20:         5.03110838 5.0657653800000002 4.9937844299999998   8.7   8.1   8.1
    ## 21: 5.2956042300000004 5.2652196900000003         5.34388065    NA    NA    NA
    ## 22: 8.8192958800000003 8.4718694699999997 8.2313175199999993  10.2  10.2  10.2
    ## 23: 4.1806983899999999         4.40151834 4.4177866000000003  10.5  10.1  10.2
    ## 24:        10.79714203        10.84046745        10.78592205  12.4  12.4  12.5
    ## 25: 7.7760777499999998 7.5454115899999996 7.4333243400000004  11.2  11.2  11.2
    ## 26: 9.9043521900000009 9.8667116200000002 9.8251123400000004  12.2  12.4  12.6
    ## 27: 4.9875989000000001 4.9663839300000001 5.0767974899999997   9.8   9.9  10.1
    ## 28:               <NA>               <NA>               <NA>    NA    NA    NA
    ## 29:               <NA>               <NA>               <NA>   1.9   2.0   2.2
    ## 30: 4.4351024600000004 4.4772071799999997 4.3974895500000004   6.3   6.4   6.4
    ##                   HE15               HE16               HE17 scl05 scl06 scl07
    ##     scl08 scl09 scl10 scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05
    ##  1:  10.1  10.1  10.1  10.2  10.2  10.2  10.2  10.2  10.2  10.2 40066.2569
    ##  2:  10.2  10.7  10.7  10.7  10.7  10.8  10.7  10.7  10.7  10.7  1578.4024
    ##  3:  11.3  11.9  12.0  12.0  12.0  12.0  12.1  12.2  12.3  12.3  3125.8105
    ##  4:  10.5  10.6  10.6  10.7  10.8  10.9  10.9  11.8  11.8  11.8  3899.9076
    ##  5:  11.3  11.3  11.5  11.6  11.8  12.0  11.9  11.9  12.1  12.1 24959.2592
    ##  6:   7.9   7.9   7.9   8.0   8.1   8.3   8.5   8.4   8.7   8.7  3002.1369
    ##  7:  12.2  12.2  12.3  12.3  12.4  12.3  12.4  12.4  12.4  12.4 39040.2889
    ##  8:   4.6   4.7   4.7   4.8   4.9   5.0   5.1   5.2   5.2   5.3   766.6921
    ##  9:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 10:  11.4  11.4  11.5  11.8  12.0  12.2  12.5  12.5  12.7  12.8 37217.6487
    ## 11:  11.5  11.5  11.4  11.5  11.5  11.6  11.7  11.7  11.7  11.8  3771.2790
    ## 12:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 13:   6.5   6.6   6.8   7.0   7.2   6.7   6.9   7.1   7.2   7.3 35591.0058
    ## 14:  10.3  10.4  10.6  10.6  10.7  10.7  10.8  10.8  10.9  10.9         NA
    ## 15:   9.4   9.6   9.8  10.1  10.1  10.1  10.1  10.2  10.2  10.2  5587.0256
    ## 16:   1.9   1.9   2.0   2.0   2.1   2.2   2.3   2.3   2.3   2.3   489.0211
    ## 17:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 18:   5.2   5.2   5.2   5.5   5.7   5.9   5.9   6.0   6.2   6.2  1268.3834
    ## 19:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 20:   8.4   8.4   8.4   9.1   8.6   8.8   9.4   9.1   9.2   9.2  2729.4987
    ## 21:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA  5323.4631
    ## 22:  10.4  10.4  10.4  10.6  10.5  10.4  10.7  11.0  11.1  11.1  3720.4792
    ## 23:  10.5  10.5  11.2  11.2  11.3  11.4  11.4  11.5  11.5  11.5 29961.2633
    ## 24:  12.2  12.2  12.3  12.4  12.4  12.2  12.3  12.4  12.4  12.4 43437.0631
    ## 25:  11.3  11.3  11.3  11.3  11.3  11.3  11.3  11.3  11.3  11.3  1826.9314
    ## 26:  12.8  13.1  13.2  13.0  12.9  12.6  12.7  12.8  12.9  12.9 42030.2866
    ## 27:  10.3  10.5  10.7  10.9  11.1  11.3  11.3  11.4  11.4  11.5   546.7769
    ## 28:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA         NA
    ## 29:   2.3   2.5   2.6   2.8   3.0   3.0   3.0   3.0   3.0   3.0         NA
    ## 30:   6.5   6.5   6.6   6.7   6.7   6.8   6.9   6.9   7.0   7.0   702.7409
    ##     scl08 scl09 scl10 scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05
    ##          GDP06      GDP07      GDP08      GDP09      GDP10      GDP11
    ##  1: 42675.8128 47803.6936 48718.4969 43503.1855 40852.6668 43335.3289
    ##  2:  2473.0818  3851.4379  5574.6038  4950.2948  5842.8058  7189.6912
    ##  3:  3847.4341  4735.6576  6377.3697  5351.3554  6029.3968  6519.2302
    ##  4:  4523.0508  5885.1043  7265.7355  6988.2333  6812.4063  7809.4251
    ##  5: 26729.3234 31244.9262 35397.3637 32109.2425 31023.6383 32396.3857
    ##  6:  3328.8830  3567.8364  4249.0193  4231.6158  4633.5904  5200.5558
    ##  7: 41188.0937 48414.8451 53554.0389 47293.9928 46459.9733 51081.9977
    ##  8:   792.8259   981.1113  1076.7013  1150.2111  1172.0985  1287.9541
    ##  9:         NA         NA         NA         NA         NA         NA
    ## 10: 35433.9890 35275.2284 39339.2976 40855.1756 44507.6764 48167.9973
    ## 11:  5291.5757  6771.4148  8513.5646  7165.2232  9070.4883 11634.0019
    ## 12:         NA         NA         NA         NA         NA         NA
    ## 13: 42781.3665 45782.2766 55494.9510 37561.6727 38577.4983 48631.6913
    ## 14:         NA         NA         NA         NA         NA         NA
    ## 15:  6209.1245  7243.4560  8474.5868  7292.4944  9040.5663 10399.3728
    ## 16:   523.0386   596.6902   694.2777   698.8989   710.2742   837.6034
    ## 17:         NA         NA         NA         NA         NA         NA
    ## 18:  1656.4248  1883.4613  2242.8719  1891.3354  2280.4374  2487.5982
    ## 19:         NA         NA         NA         NA         NA         NA
    ## 20:  3154.3312  3606.0704  4220.6170  4196.3128  5082.3548  5869.3231
    ## 21:  6920.1891  9101.2550 11635.2729  8562.8133 10674.9958 14311.0843
    ## 22:  4382.6173  5848.4764  7101.0401  6169.1142  5735.4229  6809.1598
    ## 23: 33769.1542 39432.9383 40007.4693 38927.2069 47236.9602 53890.4287
    ## 24: 46593.6022 53700.0053 56152.5523 46946.9603 52869.0443 60755.7596
    ## 25:  2300.7697  3065.6113  3887.2423  2542.9954  2965.1397  3569.7581
    ## 26: 44599.6976 50566.8266 47286.9985 38713.1374 39435.8399 42038.5723
    ## 27:   654.2838   830.4077  1082.2860  1213.2653  1634.3121  1926.2930
    ## 28:         NA         NA         NA         NA         NA         NA
    ## 29:         NA         NA         NA         NA         NA         NA
    ## 30:  1047.9192  1124.2906  1394.0006  1159.9078  1489.4593  1672.9083
    ##          GDP06      GDP07      GDP08      GDP09      GDP10      GDP11
    ##          GDP12      GDP13      GDP14      GDP15      GDP16      GDP17 MMR05
    ##  1: 38686.4613 39538.7667 41303.9294 35762.5231 37474.6654 38962.8804    NA
    ##  2:  7496.2946  7875.7570  7891.3131  5500.3104  3880.7387  4147.0897    42
    ##  3:  6940.1593  7978.8726  8318.5127  5949.1063  5022.6266  5761.7471    11
    ##  4:  7395.8498  7655.1297  7876.8665  7055.9357  7548.8550  8334.0817    15
    ##  5: 28912.1569 27729.1927 27129.6261 23333.7149 24532.5191 26338.6943    12
    ##  6:  5682.0450  6056.3308  6377.0915  6124.4916  6060.0933  6213.5013    94
    ##  7: 47710.7902 49878.0432 50260.2999 42784.6984 43784.2840 46336.6633     5
    ##  8:  1337.3354  1393.9560  1402.1002  1389.1195  1265.9876  1294.2397   459
    ##  9:         NA         NA         NA         NA         NA         NA    34
    ## 10: 48603.4766 40454.4475 38109.4121 34524.4699 38761.8182 38386.5111     7
    ## 11: 12386.7000 13890.6318 12807.2607 10510.7719  7714.8418  9247.5813    43
    ## 12:         NA         NA         NA         NA         NA         NA    15
    ## 13: 51979.1052 49388.1374 44062.3170 29869.5294 27653.0668 29759.4365    10
    ## 14:         NA         NA         NA         NA         NA         NA    NA
    ## 15: 10817.4429 10970.1233 11319.0798  9955.2437  9817.7385 10259.1818    31
    ## 16:   778.6193   805.0328   848.2741   751.4748   780.7186   830.0184   691
    ## 17:         NA         NA         NA         NA         NA         NA    34
    ## 18:  2723.8228  2961.5503  3098.9863  2687.4801  2176.0022  1968.5647  1080
    ## 19:         NA         NA         NA         NA         NA         NA    NA
    ## 20:  6528.9722  6756.7528  6672.8803  6229.1017  6204.9973  6710.5080   118
    ## 21: 15420.8745 15974.6446 14095.6487  9313.0136  8704.8984 10720.3326    42
    ## 22:  6015.9452  6755.0737  6600.0568  5588.9807  5765.2008  6292.5436    12
    ## 23: 55546.4885 56967.4258 57562.5308 55646.6187 56828.2953 60913.7453    13
    ## 24: 58037.8213 61126.9432 60020.3605 51545.4836 51965.1572 53791.5087     5
    ## 25:  3855.4177  4029.7113  3104.6432  2124.6623  2187.7305  2640.6757    33
    ## 26: 42462.7716 43444.5330 47425.6077 44974.8319 41064.1334 40361.4174    11
    ## 27:  2137.0251  2281.4110  2492.3366  2615.0251  2567.7992  1826.5669    38
    ## 28:         NA         NA         NA         NA         NA         NA    NA
    ## 29:         NA         NA         NA         NA         NA         NA    NA
    ## 30:  1763.0727  1878.9097  1763.0626  1337.7956  1280.5784  1534.8668   421
    ##          GDP12      GDP13      GDP14      GDP15      GDP16      GDP17 MMR05
    ##     MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ##  1:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##  2:    35    34    32    32    31    30    29    28    28    27    26    26
    ##  3:     9     7     6     6     5     5     4     3     3     3     3     2
    ##  4:    14    13    13    12    12    12    11    10    11    10    10    10
    ##  5:     9     8     9     8     8     8     7     6     8     7     6     6
    ##  6:    90    85    82    80    78    76    71    67    65    63    61    59
    ##  7:     4     4     4     4     4     4     4     4     3     3     3     3
    ##  8:   467   473   484   484   506   496   500   496   492   488   489   480
    ##  9:    32    30    28    25    22    19    18    17    17    17    16    16
    ## 10:     7     6     6     6     6     6     5     5     5     5     5     5
    ## 11:    40    36    30    24    22    19    17    14    13    12    10    10
    ## 12:    14    15    15    16    15    14    13    13    12    12    11    11
    ## 13:    10    10    10    10    10    10    11    11    11    11    12    12
    ## 14:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 15:    30    30    29    29    30    30    30    30    30    30    29    29
    ## 16:   675   663   661   661   660   663   663   663   642   620   590   562
    ## 17:    31    31    29    28    29    21    22    21    23    22    20    19
    ## 18:  1040  1010   996   987   978   972   963   951   943   931   925   917
    ## 19:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 20:   114   112   108   106   104   102   100    98    96    94    91    88
    ## 21:    36    32    30    27    25    23    22    20    19    18    18    17
    ## 22:    12    12    12    12    12    12    12    12    12    13    12    12
    ## 23:    13    12    11    10    10    10    10     9     8     9     8     8
    ## 24:     5     5     5     5     4     5     5     5     5     4     4     4
    ## 25:    31    33    33    27    25    23    24    23    24    21    20    19
    ## 26:    11    11    11    10    10     9     8     8     8     8     7     7
    ## 27:    37    35    34    32    31    32    32    31    30    30    29    29
    ## 28:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 29:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 30:   406   387   356   329   305   283   267   254   242   232   222   213
    ##     MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ##                   LMH09               LMH14
    ##  1:         high income         high income
    ##  2: upper middle income upper middle income
    ##  3: upper middle income upper middle income
    ##  4: upper middle income upper middle income
    ##  5:         high income         high income
    ##  6: lower middle income upper middle income
    ##  7:         high income         high income
    ##  8:          low income          low income
    ##  9: upper middle income upper middle income
    ## 10:         high income         high income
    ## 11: upper middle income upper middle income
    ## 12:         high income         high income
    ## 13:         high income         high income
    ## 14:                <NA>                <NA>
    ## 15: upper middle income upper middle income
    ## 16:          low income          low income
    ## 17: lower middle income lower middle income
    ## 18: lower middle income lower middle income
    ## 19:                <NA>                <NA>
    ## 20: upper middle income upper middle income
    ## 21: upper middle income         high income
    ## 22: upper middle income upper middle income
    ## 23:         high income         high income
    ## 24:         high income         high income
    ## 25: lower middle income lower middle income
    ## 26:         high income         high income
    ## 27: lower middle income lower middle income
    ## 28:                <NA>                <NA>
    ## 29:                <NA>                <NA>
    ## 30:          low income lower middle income
    ##                   LMH09               LMH14

\#\#\#\#cvd17fem

``` r
na.omit(edit17, c("cvd17fem"), invert = TRUE) #missing cvdfem -> no missing
```

    ## Empty data.table (0 rows and 129 cols): location_name,GSNI_PERIOD,onebias,twobias,nobias,political...

\#\#\#\#cvd17

``` r
na.omit(edit17, c("cvd17both"), invert = TRUE) #missing cvdboth -> no missing
```

    ## Empty data.table (0 rows and 129 cols): location_name,GSNI_PERIOD,onebias,twobias,nobias,political...

\#\#\#\#LEbirth2019

``` r
na.omit(edit17, c("LEbirth2015"), invert = TRUE) #missing LE at birth (Andorra, Palestine and UK)
```

    ##          location_name GSNI_PERIOD onebias twobias nobias political economic
    ## 1:             Andorra   2005–2009   27.01    7.43  72.99     14.08     8.73
    ## 2: Palestine, State of   2010–2014   98.00   92.30   2.00     89.30    79.50
    ## 3:      United Kingdom   2005–2009   54.60   25.50  45.40     26.07    25.15
    ##    educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem  cvd09fem
    ## 1:        1.81    12.01  94.99074  94.10549  92.00298  91.82684  93.03548
    ## 2:       26.70    83.50 367.37503 362.07740 355.82036 350.58586 343.97033
    ## 3:        6.65    30.34 147.03415 138.97711 132.69706 127.54333 120.00812
    ##     cvd10fem  cvd11fem  cvd12fem  cvd13fem cvd14fem  cvd15fem  cvd16fem
    ## 1:  93.64847  94.72272  94.77537  95.38794  95.7992  96.76678  96.28366
    ## 2: 341.78855 337.69661 320.17827 314.63758 323.3969 338.52757 350.93555
    ## 3: 115.83452 112.45472 112.30358 111.23225 108.8208 108.30317 106.14944
    ##     cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ## 1:  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052  126.3548
    ## 2: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 3: 104.51284 106.93075 107.12455  224.9909  213.8383  203.5031  195.1599
    ##    cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ## 1:  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398  119.3653
    ## 2:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 3:  185.3431  178.1320  171.7848  168.7043  167.4859  163.1700  161.8943
    ##    cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ## 1:  118.6682  117.7883  116.9475  115.8910  114.4628  112.7694  110.3326
    ## 2:  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939  415.8933
    ## 3:  159.0024  157.5910  160.8536  161.2392  181.8112  172.5648  164.6845
    ##    cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ## 1:  109.3167  109.3833  109.1886  109.4555  108.8728  108.7915  108.3474
    ## 2:  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001  368.8565
    ## 3:  158.1859  149.6043  144.0946  139.3859  137.9374  136.8413  133.6982
    ##    cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ## 1:  108.1292  107.5617  106.9530  106.3957  105.7119          NA       NA
    ## 2:  380.8886  384.4684  388.1755  386.6795  381.6978          NA       NA
    ## 3:  132.9974  130.6090  129.1214  132.0264  132.3753          NA       NA
    ##    LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05  phy06
    ## 1:          NA       NA          NA       NA          NA       NA 3.2319 3.0123
    ## 2:          NA       NA          NA       NA          NA       NA     NA     NA
    ## 3:          NA       NA          NA       NA          NA       NA 2.4120 2.4590
    ##     phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14  phy15  phy16  phy17
    ## 1: 3.0109     NA 3.1479 4.0000     NA     NA     NA     NA 3.3333     NA     NA
    ## 2:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 3: 2.4844 2.5543 2.6279 2.6265 2.6603 2.6667 2.6774 2.7149 2.7465 2.7563 2.7863
    ##                  HE05               HE06               HE07               HE08
    ## 1: 5.5754260999999996 4.9350943599999999 4.9255475999999998 5.8059859300000003
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3: 8.5339870500000004 8.6973781599999995 8.8732242600000006 9.1662340199999992
    ##                  HE09               HE10               HE11               HE12
    ## 1: 6.2023372700000001         6.64963865 6.2465286300000002 6.1015033699999996
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3:        10.01600361 9.9891519500000001 9.9734649700000002 10.051768300000001
    ##                  HE13               HE14               HE15               HE16
    ## 1: 5.9878034600000003 5.9791245499999999 6.2324533500000001 6.3434934600000004
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3: 9.9785518599999996 9.9575843800000001 9.9043521900000009 9.8667116200000002
    ##                  HE17 scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12 scl13
    ## 1: 6.5443186799999999   9.8  10.1  10.1  10.1  10.1  10.1  10.2  10.2  10.2
    ## 2:               <NA>    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3: 9.8251123400000004  12.2  12.4  12.6  12.8  13.1  13.2  13.0  12.9  12.6
    ##    scl14 scl15 scl16 scl17    GDP05    GDP06    GDP07   GDP08    GDP09    GDP10
    ## 1:  10.2  10.2  10.2  10.2 40066.26 42675.81 47803.69 48718.5 43503.19 40852.67
    ## 2:    NA    NA    NA    NA       NA       NA       NA      NA       NA       NA
    ## 3:  12.7  12.8  12.9  12.9 42030.29 44599.70 50566.83 47287.0 38713.14 39435.84
    ##       GDP11    GDP12    GDP13    GDP14    GDP15    GDP16    GDP17 MMR05 MMR06
    ## 1: 43335.33 38686.46 39538.77 41303.93 35762.52 37474.67 38962.88    NA    NA
    ## 2:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ## 3: 42038.57 42462.77 43444.53 47425.61 44974.83 41064.13 40361.42    11    11
    ##    MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:    11    11    10    10     9     8     8     8     8     7     7
    ##          LMH09       LMH14
    ## 1: high income high income
    ## 2:        <NA>        <NA>
    ## 3: high income high income

``` r
edit17[c("Andorra", "Palestine, State of", "United Kingdom"), c("location_name", "LEbirth2000", "LEbirth2010", "LEbirth2015", "LEbirth2019")]
```

    ##          location_name LEbirth2000 LEbirth2010 LEbirth2015 LEbirth2019
    ## 1:             Andorra          NA          NA          NA          NA
    ## 2: Palestine, State of          NA          NA          NA          NA
    ## 3:      United Kingdom          NA          NA          NA          NA

``` r
#no data points that can be used to fill LE at birth for these countries
```

Can add in data from IMHE <https://vizhub.healthdata.org/gbd-compare/le>
LE at birth for females 2019 UK\[68\] = 82.88 Andorra\[2\]= 84.95
Palestine\[48\] = 76.47

``` r
edit17$LEbirth2019[68] <- 82.88
edit17$LEbirth2019[2] <- 84.95
edit17$LEbirth2019[48] <- 76.47

edit17[c("Andorra", "Palestine, State of", "United Kingdom"), c("location_name", "LEbirth2000", "LEbirth2010", "LEbirth2015", "LEbirth2019")]
```

    ##          location_name LEbirth2000 LEbirth2010 LEbirth2015 LEbirth2019
    ## 1:             Andorra          NA          NA          NA       84.95
    ## 2: Palestine, State of          NA          NA          NA       76.47
    ## 3:      United Kingdom          NA          NA          NA       82.88

\#\#\#\#phy17

``` r
na.omit(edit17, c("phy17"), invert = TRUE) #missing phy (26 rows)
```

    ##           location_name GSNI_PERIOD onebias twobias nobias political economic
    ##  1:             Andorra   2005–2009   27.01    7.43  72.99     14.08     8.73
    ##  2:          Azerbaijan   2010–2014   99.14   93.82   0.86     85.13    91.97
    ##  3:             Belarus   2010–2014   90.37   71.70   9.63     77.82    58.45
    ##  4:            Bulgaria   2005–2009   76.84   44.40  23.16     53.67    37.13
    ##  5:              Cyprus   2010–2014   81.05   49.44  18.95     48.14    43.85
    ##  6:             Ecuador   2010–2014   93.34   58.90   6.66     46.34    36.44
    ##  7:             Finland   2005–2009   51.16   22.67  48.84     24.58    23.08
    ##  8:               Haiti   2010–2014   98.91   92.82   1.09     76.33    72.06
    ##  9:               Japan   2010–2014   68.81   41.67  31.19     46.87    41.79
    ## 10:          Kazakhstan   2010–2014   96.22   79.02   3.78     75.22    67.54
    ## 11:              Kuwait   2010–2014   97.77   91.56   2.23     88.10    77.13
    ## 12:          Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80    71.53
    ## 13:            Malaysia   2010–2014   98.54   88.38   1.46     79.69    74.54
    ## 14:                Mali   2005–2009   98.82   93.36   1.18     81.89    88.87
    ## 15:             Nigeria   2010–2014   99.73   94.99   0.27     85.83    83.42
    ## 16: Palestine, State of   2010–2014   98.00   92.30   2.00     89.30    79.50
    ## 17:                Peru   2010–2014   87.96   49.99  12.04     38.44    27.05
    ## 18:  Russian Federation   2010–2014   86.83   68.56  13.17     68.43    58.77
    ## 19:              Serbia   2005–2009   82.62   48.61  17.38     47.05    35.49
    ## 20:           Singapore   2010–2014   92.34   73.20   7.66     76.18    52.23
    ## 21:              Sweden   2010–2014   30.01   10.75  69.99     16.05     9.16
    ## 22:             Ukraine   2010–2014   86.53   65.40  13.47     62.63    57.69
    ## 23:          Uzbekistan   2010–2014   97.93   87.73   2.07     78.67    80.33
    ## 24:            Viet Nam   2005–2009   92.89   69.17   7.11     59.40    62.49
    ## 25:               Yemen   2010–2014   97.80   92.10   2.20     87.40    87.20
    ## 26:              Zambia   2005–2009   96.84   80.56   3.16     66.04    55.41
    ##           location_name GSNI_PERIOD onebias twobias nobias political economic
    ##     educational physical   cvd05fem   cvd06fem  cvd07fem  cvd08fem  cvd09fem
    ##  1:        1.81    12.01   94.99074   94.10549  92.00298  91.82684  93.03548
    ##  2:       30.90    72.16  653.88539  662.74433 659.53238 654.59319 655.75062
    ##  3:       21.19    55.52  476.75969  456.91769 436.00228 424.59960 427.87579
    ##  4:       10.95    39.42  563.51958  553.94503 534.14462 513.25045 497.57334
    ##  5:       14.03    53.31  250.96373  242.99750 231.03900 217.51610 195.44933
    ##  6:       23.46    84.36  161.28268  164.29742 163.35005 160.95424 157.69322
    ##  7:        6.22    29.69  176.62785  169.44401 166.12157 162.55154 159.13643
    ##  8:       59.91    88.13  526.03693  523.13684 519.11249 514.70529 510.66940
    ##  9:       16.21    26.28   80.62892   77.54224  74.58392  72.08078  69.30532
    ## 10:       21.71    68.51  602.99412  598.51899 592.41855 573.33154 547.90401
    ## 11:       36.45    83.12  181.89122  175.94790 179.89550 194.47948 187.99981
    ## 12:       41.00    81.73  546.88719  553.71417 543.25326 536.85221 507.41753
    ## 13:       43.00    94.31  278.24360  271.41947 262.56217 262.28229 263.15505
    ## 14:       47.61    84.87  320.56474  319.77363 317.97481 317.98693 317.14954
    ## 15:       46.18    92.78  277.35636  271.85405 263.52070 259.79173 257.07280
    ## 16:       26.70    83.50  367.37503  362.07740 355.82036 350.58586 343.97033
    ## 17:       14.36    79.76   97.82452   91.17324  84.14951  83.74437  93.03498
    ## 18:       22.66    50.02  569.51494  527.82419 500.58817 492.70377 467.54550
    ## 19:       13.20    66.56  561.66401  547.13295 531.03292 528.63016 518.35528
    ## 20:       26.18    65.66  123.75950  119.41610 114.07452 110.44957 103.82613
    ## 21:        2.61    14.13  148.45246  147.10784 144.55349 139.74976 136.25239
    ## 22:       18.23    56.61  577.57784  553.20902 554.13746 550.85070 519.56979
    ## 23:       48.60    83.93 1025.82262 1005.30790 983.98523 997.02332 992.45481
    ## 24:       20.36    70.56  256.62741  256.62311 255.97453 255.19294 254.46217
    ## 25:       45.30    81.00  493.05999  489.24403 485.10075 483.33838 478.34072
    ## 26:       23.53    89.07  301.19131  304.24329 301.25347 299.44540 297.43078
    ##     educational physical   cvd05fem   cvd06fem  cvd07fem  cvd08fem  cvd09fem
    ##       cvd10fem   cvd11fem   cvd12fem   cvd13fem   cvd14fem   cvd15fem  cvd16fem
    ##  1:   93.64847   94.72272   94.77537   95.38794   95.79920   96.76678  96.28366
    ##  2:  661.62994  662.91437  665.11545  673.97385  688.85984  689.28400 688.89897
    ##  3:  422.21601  428.07519  392.30102  389.72174  383.05228  373.94641 375.58732
    ##  4:  495.06305  483.56995  465.16759  445.93731  456.08244  452.74524 444.74854
    ##  5:  188.74835  188.54621  184.83879  181.23758  181.02862  179.81251 178.14639
    ##  6:  157.35397  149.29192  153.73253  152.93464  152.05617  148.90913 149.01126
    ##  7:  155.55659  151.01600  149.29429  146.09508  142.89271  139.40322 136.26260
    ##  8:  508.21671  512.24372  507.65492  502.18482  497.94515  495.34101 489.48808
    ##  9:   68.18071   67.97138   65.88326   63.94075   62.02763   59.92309  59.05503
    ## 10:  538.84766  523.68062  506.50361  483.03342  462.02411  446.05277 433.43107
    ## 11:  170.86166  155.06885  143.13273  131.57058  127.63836  121.04601 120.12173
    ## 12:  488.18726  483.29378  465.05092  442.79747  429.36559  435.06853 415.09331
    ## 13:  252.21083  237.09570  233.23215  226.51494  228.10326  227.29230 224.59851
    ## 14:  316.24847  314.96309  313.19762  310.27834  307.23772  309.21589 307.72590
    ## 15:  254.32157  253.99749  254.09391  253.49508  249.71126  252.45876 247.75393
    ## 16:  341.78855  337.69661  320.17827  314.63758  323.39695  338.52757 350.93555
    ## 17:   94.03452   91.93528   89.98908   87.97633   83.77122   80.26885  78.92477
    ## 18:  459.66489  424.43583  406.44126  391.62857  386.28781  376.79609 366.11070
    ## 19:  493.55030  467.81093  455.97734  447.78602  454.53739  452.53237 439.25670
    ## 20:   99.15298   92.93724   91.20203   86.55430   83.43653   79.65879  76.29249
    ## 21:  132.65068  130.65338  127.62791  124.02870  120.35028  117.04866 116.75578
    ## 22:  506.60540  481.38459  473.11451  464.17229  436.92347  474.36156 460.24398
    ## 23: 1012.61930 1037.46364 1049.70253 1041.14657 1024.86478 1010.07674 975.22891
    ## 24:  252.55867  250.17926  247.05521  243.05616  239.21285  234.94030 230.61406
    ## 25:  472.17590  471.35922  468.99902  467.16132  464.47411  466.86730 466.88438
    ## 26:  300.60435  303.67459  305.16350  306.41551  307.02384  308.97725 311.38853
    ##       cvd10fem   cvd11fem   cvd12fem   cvd13fem   cvd14fem   cvd15fem  cvd16fem
    ##      cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ##  1:  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052  126.3548
    ##  2: 685.80022 648.14175 627.27975  795.4780  806.0879  797.5581  795.9201
    ##  3: 373.29793 365.27597 362.81115  841.2488  822.3199  768.8990  771.9881
    ##  4: 440.64650 452.67136 454.55472  814.3341  795.8899  767.4014  740.1393
    ##  5: 179.63631 174.81338 172.41410  340.5299  338.3851  330.8774  312.6105
    ##  6: 149.63616 148.94035 146.37173  195.4146  197.5540  198.2290  197.6351
    ##  7: 136.03946 135.91504 135.37010  292.7180  285.9363  280.4463  271.9016
    ##  8: 483.52984 479.46926 475.27316  431.5242  430.9686  431.2913  431.2199
    ##  9:  57.80935  58.18790  58.35637  136.7445  131.7984  128.0151  124.7603
    ## 10: 408.05109 402.99006 401.45939  929.4738  926.9272  918.2064  877.5018
    ## 11: 120.95883 121.22492 124.78355  282.0308  283.0347  271.4806  295.6581
    ## 12: 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687  761.4547
    ## 13: 222.86437 221.09851 220.62850  326.1480  318.3576  312.0718  316.5872
    ## 14: 305.13664 304.19901 301.76476  250.4399  251.6409  253.7490  256.6272
    ## 15: 245.20993 242.25928 239.99721  274.4590  267.8654  259.5376  255.2531
    ## 16: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 17:  78.51316  77.75822  77.12523  118.1332  114.0556  109.6057  107.1766
    ## 18: 348.59567 349.51106 351.22730  956.8934  876.9782  830.0730  825.6957
    ## 19: 434.70288 432.93221 428.30908  605.1301  591.9054  575.6820  567.4146
    ## 20:  74.34356  73.94099  73.62812  186.6925  179.9437  176.8463  168.2657
    ## 21: 115.86243 116.73812 114.02886  236.4315  227.9933  219.7105  211.9112
    ## 22: 455.73238 466.00037 465.00257  906.8034  875.1121  894.2645  895.7625
    ## 23: 945.26316 908.52364 856.84712 1349.0110 1313.7526 1257.9630 1247.2106
    ## 24: 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026  455.8597
    ## 25: 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200  566.4953
    ## 26: 308.67000 306.10250 303.60330  396.9908  395.9378  389.5186  383.8555
    ##      cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ##     cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ##  1:  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398  119.3653
    ##  2:  792.0192  792.6029  806.8414  826.6460  825.9170  835.2344  833.7264
    ##  3:  770.1596  779.5070  788.8996  710.1381  699.5572  687.9125  667.1438
    ##  4:  720.9119  704.5502  686.3721  666.1790  649.1804  660.2319  653.9685
    ##  5:  308.6625  303.2067  287.3138  275.9141  262.2377  252.0967  245.5002
    ##  6:  191.5033  191.5863  191.7630  187.9508  186.7919  185.9278  184.5895
    ##  7:  266.2553  260.7571  252.2236  244.8651  238.2376  231.7559  225.2625
    ##  8:  430.6858  431.0238  433.7781  433.2272  432.0381  430.4305  429.9405
    ##  9:  121.7149  119.7500  117.8518  114.2759  110.5415  106.8358  104.0517
    ## 10:  824.7192  809.5292  791.3657  766.3255  730.3873  693.7872  668.8549
    ## 11:  273.0090  237.9775  221.1212  224.7644  218.0751  208.6838  215.4350
    ## 12:  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342  644.2163
    ## 13:  315.2901  303.3995  295.3241  285.1897  264.2568  267.6031  268.1236
    ## 14:  259.4479  259.6777  258.0625  256.5280  255.5163  253.8127  257.2386
    ## 15:  252.9699  250.7906  250.3251  250.0944  248.4156  244.0515  246.2866
    ## 16:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 17:  117.1724  120.0147  118.2343  116.0098  113.0409  107.4914  103.9618
    ## 18:  776.4243  770.5636  706.6609  675.9903  650.0326  646.2977  623.4263
    ## 19:  548.0041  524.2160  503.8235  494.4056  513.2331  517.1577  508.8793
    ## 20:  159.8412  154.4816  147.9723  141.9160  136.9070  131.4295  126.8360
    ## 21:  205.6428  198.6352  193.1647  187.9331  181.3070  174.5094  171.7927
    ## 22:  817.5231  795.2222  763.1596  754.7471  743.9361  691.8518  816.2680
    ## 23: 1227.9049 1230.5979 1242.3562 1242.5922 1222.1098 1205.2298 1190.7704
    ## 24:  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765  443.7173
    ## 25:  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702  536.2482
    ## 26:  377.2154  376.4333  380.5640  379.4144  379.6064  381.3379  385.9456
    ##     cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ##     cvd16male  cvd17male cvd18male  cvd19male cvd05both cvd06both  cvd07both
    ##  1:  118.6682  117.78826  116.9475  115.89102  114.4628  112.7694  110.33258
    ##  2:  842.2704  843.82968  784.6385  760.37239  725.8544  735.0585  728.35144
    ##  3:  655.9036  654.16797  631.3911  624.05795  615.4166  593.5400  562.05477
    ##  4:  652.3623  649.20064  651.8298  649.05077  674.9428  661.4847  637.59710
    ##  5:  222.3039  198.85576  198.3850  199.87770  292.7366  286.9163  276.58795
    ##  6:  186.8145  189.48573  189.0448  182.28777  177.6049  180.0888  179.95002
    ##  7:  223.1535  223.00722  220.6973  219.35925  226.8430  219.8546  216.02209
    ##  8:  426.5772  424.13334  423.1650  419.78532  481.0125  479.0757  477.03676
    ##  9:  101.9864   99.50671   98.7673   98.55422  104.9456  101.2200   98.02780
    ## 10:  644.9578  595.59312  606.7502  583.04195  739.1100  735.1324  727.00327
    ## 11:  218.6227  221.07636  221.7215  225.49442  244.9834  243.3229  237.55346
    ## 12:  618.5638  613.38427  580.3176  569.34914  634.0089  651.1682  645.59071
    ## 13:  275.7751  278.45188  279.6520  290.33018  304.0469  296.6711  289.07676
    ## 14:  257.6357  257.62900  255.5112  252.29479  284.7410  284.8986  285.02779
    ## 15:  241.7979  238.75676  235.4675  232.25723  278.5825  272.1292  263.44130
    ## 16:  433.5059  438.10501  440.8897  433.97986  434.1983  427.1939  415.89329
    ## 17:  102.7772  102.27404  101.7696  101.14597  107.5290  102.1196   96.34764
    ## 18:  604.2681  561.37484  549.7797  549.17392  727.6035  670.5814  635.16266
    ## 19:  479.5325  475.06741  473.5821  468.65816  581.1130  566.8383  550.81185
    ## 20:  122.9414  119.67184  119.1105  115.46880  152.9234  147.4189  143.05822
    ## 21:  168.7297  166.76857  165.8889  165.85687  186.9808  182.9116  178.18444
    ## 22:  779.5641  770.54121  792.6953  786.12722  705.0232  676.2279  684.18530
    ## 23: 1157.1051 1127.84654 1097.1030 1059.62943 1156.2007 1131.6463 1103.74692
    ## 24:  440.1349  436.88548  433.6069  429.93624  334.7621  335.3238  336.52387
    ## 25:  536.1051  541.04732  547.2489  550.65478  535.1088  530.9694  525.70885
    ## 26:  390.7901  386.09677  382.3781  377.53367  348.3714  349.1627  344.28588
    ##     cvd16male  cvd17male cvd18male  cvd19male cvd05both cvd06both  cvd07both
    ##      cvd08both  cvd09both cvd10both  cvd11both  cvd12both  cvd13both  cvd14both
    ##  1:  109.31671  109.38333  109.1886  109.45548  108.87280  108.79150  108.34739
    ##  2:  724.46736  722.60586  725.7938  732.17587  739.57962  744.00434  757.21682
    ##  3:  555.95422  557.54021  557.6104  565.03762  511.81059  505.55688  497.23919
    ##  4:  613.86478  596.49917  588.2128  574.25279  554.67232  536.10535  546.62593
    ##  5:  260.43612  243.58089  235.8788  228.93682  222.03099  215.59362  212.30069
    ##  6:  178.65734  174.14429  173.9953  169.80081  170.33946  169.40528  168.52380
    ##  7:  210.59758  206.37477  202.0771  195.89295  191.83921  187.40416  182.96093
    ##  8:  474.64371  472.25177  471.2477  474.82817  472.17919  468.77018  465.82635
    ##  9:   95.29858   92.50782   91.1429   90.36691   87.76339   85.18238   82.57175
    ## 10:  698.76475  663.07312  651.3492  633.77423  612.75404  584.05483  556.36666
    ## 11:  258.02380  241.09178  212.5469  195.85098  193.39730  184.59528  177.12714
    ## 12:  632.76151  603.31409  581.8522  562.62250  543.92509  519.53712  513.01612
    ## 13:  291.31778  291.24859  279.5220  267.40258  260.45450  246.45538  248.03609
    ## 14:  286.46916  287.49451  287.1683  285.69447  284.03384  282.08967  279.73392
    ## 15:  259.09633  256.29837  253.4984  252.80350  252.46797  251.08995  246.82989
    ## 16:  407.15878  399.09041  394.3262  387.97737  366.84516  355.90010  368.85654
    ## 17:   94.97714  104.62926  106.4933  104.54989  102.44656   99.95400   95.08345
    ## 18:  628.21607  592.77514  585.0586  537.70788  514.58328  495.26564  490.38527
    ## 19:  545.61221  531.45941  507.4926  484.72607  474.11523  477.56554  482.73602
    ## 20:  137.56764  130.09757  125.0091  118.70562  115.15732  110.43077  106.32256
    ## 21:  172.12691  167.47419  162.5909  159.25538  155.26815  150.44817  145.54025
    ## 22:  682.22877  631.59255  614.4575  586.67859  578.93263  569.92172  534.81353
    ## 23: 1112.63928 1102.33911 1116.2918 1135.63642 1141.29449 1125.34402 1107.01301
    ## 24:  337.02684  337.15124  335.4539  333.01066  329.93117  326.60300  323.18282
    ## 25:  523.71933  517.18606  508.7890  507.49177  504.38733  501.99569  497.81417
    ## 26:  340.42203  336.02994  337.2284  340.77995  340.99547  341.73548  342.88671
    ##      cvd08both  cvd09both cvd10both  cvd11both  cvd12both  cvd13both  cvd14both
    ##      cvd15both  cvd16both  cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ##  1:  108.12924  107.56173  106.95304 106.39570 105.71185          NA       NA
    ##  2:  756.21653  758.33069  756.75780 710.38982 688.07822        68.5     17.9
    ##  3:  484.40187  481.77658  479.93207 466.89588 463.10778        74.6     19.4
    ##  4:  541.93803  536.54177  532.73637 541.03249 541.08270        75.0     19.2
    ##  5:  209.95313  202.90815  196.71239 192.06049 190.26941        81.0     23.2
    ##  6:  166.17046  167.16092  168.67782 168.10363 163.65500        77.3     23.1
    ##  7:  178.26308  175.55012  175.44476 174.60136 173.80771        80.9     23.6
    ##  8:  464.27802  459.66137  455.40585 452.87085 449.11377        57.2     15.9
    ##  9:   80.18604   78.84510   77.08885  77.00931  77.00994        84.4     26.6
    ## 10:  536.19078  519.14503  485.39932 485.31026 476.01008        69.2     17.8
    ## 11:  178.35299  179.50619  181.00846 181.17922 184.57616        80.1     23.5
    ## 12:  523.15984  500.14376  493.82082 473.26712 466.31300        70.2     18.3
    ## 13:  247.15687  249.70380  250.28741 250.11953 255.54454        75.3     19.2
    ## 14:  282.44832  281.89484  280.58319 279.03154 276.18486        52.7     15.9
    ## 15:  249.11815  244.38574  241.54042 238.38554 235.65202        55.2     17.0
    ## 16:  380.88863  384.46844  388.17552 386.67955 381.69777          NA       NA
    ## 17:   91.54823   90.28000   89.85563  89.23082  88.61384        75.9     22.9
    ## 18:  475.65621  461.71309  434.85592 431.69136 432.91864        72.3     18.7
    ## 19:  478.06378  458.04063  453.96042 453.05802 448.83636        74.3     18.4
    ## 20:  102.22058   98.56112   95.85146  95.34721  93.68210        80.7     23.1
    ## 21:  142.50612  141.00249  139.76469 140.20408 138.59168        81.8     24.1
    ## 22:  609.75995  588.08212  581.24578 594.30207 591.00624        73.2     18.8
    ## 23: 1091.31310 1056.26572 1026.66904 992.40366 945.97305        68.2     17.1
    ## 24:  319.13626  314.95461  311.23748 307.70802 303.90466        75.6     21.0
    ## 25:  500.90386  500.85723  504.82748 509.87687 513.09104        64.7     17.9
    ## 26:  346.10827  349.73020  346.15135 343.09096 339.55436        45.2     14.7
    ##      cvd15both  cvd16both  cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ##     LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05
    ##  1:          NA       NA          NA       NA       84.95       NA 3.2319
    ##  2:        71.8     17.6        73.4     17.9       74.10     18.3 3.5819
    ##  3:        76.4     20.5        78.8     21.9       79.60     22.5 3.3145
    ##  4:        77.2     20.9        78.0     21.5       78.60     22.0 3.6651
    ##  5:        83.1     24.8        84.1     25.6       85.10     26.4 1.8965
    ##  6:        77.9     22.8        79.5     23.6       80.50     24.3     NA
    ##  7:        83.0     25.3        83.9     25.7       84.00     25.8 2.9913
    ##  8:        35.4     12.0        63.1     16.7       64.80     17.0     NA
    ##  9:        85.8     27.7        86.4     28.1       86.90     28.6     NA
    ## 10:        73.4     19.2        76.2     20.6       77.60     21.4 3.6038
    ## 11:        81.4     24.0        83.8     25.9       83.90     25.9 1.8000
    ## 12:        73.3     19.3        75.3     20.2       77.30     21.7     NA
    ## 13:        76.5     20.1        77.1     20.7       77.10     20.6     NA
    ## 14:        59.5     17.0        61.4     17.2       63.40     17.6     NA
    ## 15:        60.8     18.0        62.8     18.5       64.10     18.9 0.2824
    ## 16:          NA       NA          NA       NA       76.47       NA     NA
    ## 17:        78.9     23.8        80.6     24.7       81.30     25.1     NA
    ## 18:        74.7     20.2        76.6     21.4       78.00     22.2 2.3204
    ## 19:        76.8     20.0        77.8     20.7       78.30     21.1 2.2609
    ## 20:        84.0     25.8        84.9     26.7       85.50     27.2 1.5819
    ## 21:        83.1     25.0        83.5     25.3       84.00     25.6 3.5107
    ## 22:        75.1     19.8        77.0     21.1       77.80     21.7 3.0181
    ## 23:        72.9     18.7        73.9     19.0       75.20     19.8 2.6701
    ## 24:        77.1     21.5        77.6     21.8       78.10     22.0     NA
    ## 25:        69.6     18.7        69.6     18.9       68.90     18.7     NA
    ## 26:        59.1     17.1        63.0     17.7       65.40     18.0 0.0545
    ##     LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05
    ##      phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14  phy15
    ##  1: 3.0123 3.0109     NA 3.1479 4.0000     NA     NA     NA     NA 3.3333
    ##  2: 3.5649 3.7124 3.6844 3.6751 3.6629 3.4376 3.4901 3.4558 3.4460     NA
    ##  3: 3.3795 3.5306 3.1477 3.2845 3.2498 4.7611 4.8352 4.8534 5.0120 5.1905
    ##  4: 3.6805 3.6800 3.6488 3.7401 3.7661 3.8467 3.8995 3.9630 3.9750 4.0332
    ##  5: 1.8374 2.0147 2.0507 2.1064 2.1562 2.2484 2.2960 2.4076 2.4993 2.6236
    ##  6:     NA     NA     NA 1.5983 2.1111 1.6582     NA     NA     NA 2.0648
    ##  7: 3.0262 3.0351 3.0618 3.0861 3.2653 3.1221 3.2854 3.2966 3.3922 3.3309
    ##  8:     NA     NA     NA     NA     NA 0.1378     NA     NA     NA 0.0852
    ##  9: 2.0746     NA 2.1394     NA 2.2059     NA 2.2740     NA 2.3413     NA
    ## 10: 3.6983 3.6547 3.7161 3.7808 3.9278 3.9483 3.9108 3.9508 3.9800     NA
    ## 11: 1.4543 1.8814 1.8806 1.8929 2.4296 2.4560 2.4662 2.5332 2.6522 2.6463
    ## 12:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 13:     NA     NA 0.9216     NA 1.1691 1.2777 1.3320     NA     NA 1.5358
    ## 14:     NA 0.0776 0.0490 0.0923 0.1021 0.1037 0.1071     NA     NA     NA
    ## 15: 0.3481 0.3784 0.3762 0.3782 0.1836     NA     NA 0.3828     NA     NA
    ## 16:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 17:     NA 1.6648     NA 0.9472 0.9200     NA 1.1411     NA     NA     NA
    ## 18: 2.3721 2.3841 2.3812 2.3924 2.3930 6.6305 4.1303 4.0705 4.0114 3.7494
    ## 19: 2.2858 2.3447 2.4215 2.4467 2.4820 2.5003 2.4961 2.4945 2.4611 2.4603
    ## 20: 1.5744 1.6127 1.6418 1.6758 1.7187 1.7232 1.7964 1.8958 2.0125     NA
    ## 21: 3.5960 3.6760 3.7353 3.8065 3.8779 3.9569 4.0368 4.1259 4.2037 4.2856
    ## 22: 3.0838 3.0885 3.1177 3.4904 3.4830 3.4842 3.4910 3.4965 2.9923     NA
    ## 23: 2.6515 2.6329 2.6048 2.5706 2.5432 2.5036 2.4451 2.4044 2.3742     NA
    ## 24:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 25:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 26: 0.0533     NA 0.0619 0.0606 0.0614 0.1649 0.1658     NA     NA     NA
    ##      phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14  phy15
    ##      phy16 phy17               HE05               HE06               HE07
    ##  1:     NA    NA 5.5754260999999996 4.9350943599999999 4.9255475999999998
    ##  2:     NA    NA 2.2610766899999999 2.0667839099999998         1.99021435
    ##  3:     NA    NA 6.2765383699999999 5.7629585299999997 5.8853402099999998
    ##  4:     NA    NA 6.8901143100000004         6.52544594 6.1603593800000001
    ##  5: 1.9509    NA 5.4358096099999997 5.4635748900000003 5.3574485799999998
    ##  6: 2.0368    NA         5.57841349 5.6872220000000002 5.8648901000000002
    ##  7: 3.8118    NA 8.2879562399999998 8.2841176999999995 8.0835933700000009
    ##  8:     NA    NA 5.5093555500000004 5.5010376000000001 5.9015803299999998
    ##  9: 2.4115    NA 7.7806687400000003         7.80784273 7.8904604899999997
    ## 10:     NA    NA 3.9017238600000002         3.39824891 2.7031483700000001
    ## 11:     NA    NA 2.3776223700000001         2.25313878 2.1331450900000002
    ## 12:     NA    NA               <NA>               <NA>               <NA>
    ## 13:     NA    NA 2.8012416400000002 3.1245324600000002         3.08224082
    ## 14: 0.1395    NA 5.1953811600000002 5.4565606100000004 5.2536678300000004
    ## 15: 0.4494    NA 4.4659194900000001 4.2577514599999997         3.90997219
    ## 16:     NA    NA               <NA>               <NA>               <NA>
    ## 17: 1.3048    NA 4.5863194500000004 4.5380153700000001 4.4038014399999996
    ## 18: 4.0139    NA         4.76693487         4.76170969 4.7431106600000001
    ## 19: 3.1131    NA 8.2838058500000002         8.49389839 9.3764915500000008
    ## 20: 2.2936    NA         3.03367925 2.9347465000000001         2.83749056
    ## 21: 3.9840    NA 8.1469831500000005 8.0473642299999995 7.9901738199999999
    ## 22:     NA    NA 6.3461584999999996 6.4169516599999996 6.0170521700000004
    ## 23:     NA    NA 4.8773298299999999 4.8463411299999999 4.6331453299999996
    ## 24:     NA    NA               <NA>               <NA>               <NA>
    ## 25:     NA    NA               <NA>               <NA>               <NA>
    ## 26: 0.1628    NA 6.8633222600000003 5.8736734400000001 4.3526496899999998
    ##      phy16 phy17               HE05               HE06               HE07
    ##                   HE08               HE09               HE10               HE11
    ##  1: 5.8059859300000003 6.2023372700000001         6.64963865 6.2465286300000002
    ##  2:         1.93880868 2.6137836000000001 2.4899666300000001 2.4464180500000001
    ##  3:         5.47281408 5.3903784799999999 5.6569399799999998 4.8743853599999998
    ##  4: 6.2877101900000003 6.5988688499999997         7.14063406 7.1416268299999999
    ##  5:         6.01427698 6.4653158199999998 6.5176711100000002 6.4492850300000004
    ##  6: 5.8163766900000002 6.4387331000000003 7.1219563499999996 7.8652181600000004
    ##  7: 8.3431062699999998 9.1612224599999994 9.1420278499999998         9.22303009
    ##  8: 6.0159010899999998 6.1691598900000004 8.1455984099999998         10.2313633
    ##  9: 8.1995143899999992 9.0582961999999991 9.1567735700000004 10.616717339999999
    ## 10: 3.0495264500000001 3.4991376399999998 2.7364139600000001 2.6023666900000002
    ## 11:         1.93336701 3.8693599700000001 2.7570362099999999 2.6180841899999998
    ## 12:               <NA>               <NA>               <NA>               <NA>
    ## 13: 3.0235738799999998 3.2762939900000001         3.18466234 3.3399648700000002
    ## 14: 5.0454120600000003 5.2291111900000002 4.6792340299999999 4.0157337200000001
    ## 15: 3.6958153199999999 3.5801973299999998 3.2965328700000001 3.3207793200000002
    ## 16:               <NA>               <NA>               <NA>               <NA>
    ## 17: 4.4459280999999997 4.9506058700000004 4.7206191999999998 4.6209321000000001
    ## 18: 4.8986678100000001 5.6382026700000001 4.9660777999999999 4.7900524100000004
    ## 19: 9.4824638399999994 9.3462324100000007 9.5273036999999992         9.11380005
    ## 20: 3.1948845399999999         3.39853096 3.2026536499999998 3.1579217900000001
    ## 21: 8.2066011400000001 8.7731924100000001 8.3273897199999993 10.421509739999999
    ## 22: 5.4972801200000001 6.6077189399999998 6.8122901899999997 6.8161716500000002
    ## 23: 4.7983584400000003 4.8924522399999999 5.1175293899999996 5.2821259500000002
    ## 24:               <NA>               <NA>               <NA>               <NA>
    ## 25:               <NA>               <NA>               <NA>               <NA>
    ## 26: 4.0120754200000004         4.42680454 3.7192959800000001 3.4605324300000002
    ##                   HE08               HE09               HE10               HE11
    ##                   HE12               HE13               HE14               HE15
    ##  1: 6.1015033699999996 5.9878034600000003 5.9791245499999999 6.2324533500000001
    ##  2:         2.96481562 3.0367231399999999 3.3771374199999999 4.1066360499999996
    ##  3: 5.2453269999999996 5.6955947900000004 5.3879337300000003 6.0653333700000003
    ##  4: 7.5788183199999999 7.1707920999999999 7.7096777000000003 7.4133834800000002
    ##  5:         6.55474567 6.9451847100000004 6.9609560999999998 6.9256858799999996
    ##  6: 8.4796819699999997 8.5598344799999992 8.6212749500000001 8.5884571100000002
    ##  7: 9.5857954000000003 9.8053464899999998 9.7811508200000006 9.6452388800000008
    ##  8: 9.6682958600000006         7.23788214 7.7976360299999996 8.6285295499999997
    ##  9:        10.79065323        10.79159355        10.83204937        10.88550663
    ## 10: 3.0372598200000001 2.6628675500000001 2.9746842400000002 3.0405108900000002
    ## 11:         2.57378602 2.5854389699999998 3.1975801000000001 4.0147361799999999
    ## 12:               <NA>               <NA>               <NA>               <NA>
    ## 13: 3.4879069299999998 3.5224008599999999 3.7252702700000002 3.8160853399999999
    ## 14: 3.7521157299999999 3.9675071200000001 4.4815611799999999 4.1114335100000003
    ## 15: 3.3598427800000001 3.4206934000000002 3.3484041699999998 3.5819501900000001
    ## 16:               <NA>               <NA>               <NA>               <NA>
    ## 17: 4.7565517399999999 4.7019739200000004 4.9947280899999997         5.03110838
    ## 18: 4.9408035300000002 5.0798091899999998 5.1802287099999997 5.2956042300000004
    ## 19: 9.3282098799999993 9.3123245200000007 9.2459878900000003 8.8192958800000003
    ## 20: 3.3278367499999999 3.6856265100000001 3.8715567599999998 4.1806983899999999
    ## 21:        10.73550034        10.90368462 10.948073389999999        10.79714203
    ## 22: 7.1166605900000004         6.94156218 7.1896038100000004 7.7760777499999998
    ## 23: 5.5897636400000001 5.6617379200000002         4.67292738 4.9875989000000001
    ## 24:               <NA>               <NA>               <NA>               <NA>
    ## 25:               <NA>               <NA>               <NA>               <NA>
    ## 26: 3.9305291200000001 4.6909103400000003 3.8292422300000002 4.4351024600000004
    ##                   HE12               HE13               HE14               HE15
    ##                   HE16               HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ##  1: 6.3434934600000004 6.5443186799999999   9.8  10.1  10.1  10.1  10.1  10.1
    ##  2: 4.0365543400000004 3.7365145700000002  10.7  10.7  10.2  10.2  10.7  10.7
    ##  3: 5.9105415299999997         5.75974846   9.3  10.0  10.6  11.3  11.9  12.0
    ##  4: 7.4799427999999999 7.4532680500000001  10.2  10.4  10.5  10.5  10.6  10.6
    ##  5: 6.8342390100000001 6.7382893599999996  10.7  10.9  11.2  11.3  11.3  11.5
    ##  6: 8.2959604299999992 8.2574291199999994   7.3   7.3   7.3   7.9   7.9   7.9
    ##  7: 9.3779602099999995 9.1415662799999993  12.0  12.0  12.0  12.2  12.2  12.3
    ##  8: 8.4368677099999996 8.0846834199999993   4.3   4.4   4.5   4.6   4.7   4.7
    ##  9: 10.834609990000001        10.79634285  11.2  11.2  11.3  11.4  11.4  11.5
    ## 10: 3.4223556500000001 3.1258840600000002  11.7  11.7  11.6  11.5  11.5  11.4
    ## 11: 4.0310459099999996 5.2952661499999998   5.8   6.2   6.3   6.5   6.6   6.8
    ## 12:               <NA>               <NA>  10.2  10.2  10.3  10.3  10.4  10.6
    ## 13:         3.68835807         3.71246052   7.6   8.2   8.8   9.4   9.6   9.8
    ## 14: 3.7753798999999999 4.1038827900000001   1.7   1.7   1.8   1.9   1.9   2.0
    ## 15: 3.6477367900000002 3.7555384599999999   5.2   5.2   5.2   5.2   5.2   5.2
    ## 16:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 17: 5.0657653800000002 4.9937844299999998   8.7   8.1   8.1   8.4   8.4   8.4
    ## 18: 5.2652196900000003         5.34388065    NA    NA    NA    NA    NA    NA
    ## 19: 8.4718694699999997 8.2313175199999993  10.2  10.2  10.2  10.4  10.4  10.4
    ## 20:         4.40151834 4.4177866000000003  10.5  10.1  10.2  10.5  10.5  11.2
    ## 21:        10.84046745        10.78592205  12.4  12.4  12.5  12.2  12.2  12.3
    ## 22: 7.5454115899999996 7.4333243400000004  11.2  11.2  11.2  11.3  11.3  11.3
    ## 23: 4.9663839300000001 5.0767974899999997   9.8   9.9  10.1  10.3  10.5  10.7
    ## 24:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 25:               <NA>               <NA>   1.9   2.0   2.2   2.3   2.5   2.6
    ## 26: 4.4772071799999997 4.3974895500000004   6.3   6.4   6.4   6.5   6.5   6.6
    ##                   HE16               HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ##     scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05      GDP06      GDP07
    ##  1:  10.2  10.2  10.2  10.2  10.2  10.2  10.2 40066.2569 42675.8128 47803.6936
    ##  2:  10.7  10.7  10.8  10.7  10.7  10.7  10.7  1578.4024  2473.0818  3851.4379
    ##  3:  12.0  12.0  12.0  12.1  12.2  12.3  12.3  3125.8105  3847.4341  4735.6576
    ##  4:  10.7  10.8  10.9  10.9  11.8  11.8  11.8  3899.9076  4523.0508  5885.1043
    ##  5:  11.6  11.8  12.0  11.9  11.9  12.1  12.1 24959.2592 26729.3234 31244.9262
    ##  6:   8.0   8.1   8.3   8.5   8.4   8.7   8.7  3002.1369  3328.8830  3567.8364
    ##  7:  12.3  12.4  12.3  12.4  12.4  12.4  12.4 39040.2889 41188.0937 48414.8451
    ##  8:   4.8   4.9   5.0   5.1   5.2   5.2   5.3   766.6921   792.8259   981.1113
    ##  9:  11.8  12.0  12.2  12.5  12.5  12.7  12.8 37217.6487 35433.9890 35275.2284
    ## 10:  11.5  11.5  11.6  11.7  11.7  11.7  11.8  3771.2790  5291.5757  6771.4148
    ## 11:   7.0   7.2   6.7   6.9   7.1   7.2   7.3 35591.0058 42781.3665 45782.2766
    ## 12:  10.6  10.7  10.7  10.8  10.8  10.9  10.9         NA         NA         NA
    ## 13:  10.1  10.1  10.1  10.1  10.2  10.2  10.2  5587.0256  6209.1245  7243.4560
    ## 14:   2.0   2.1   2.2   2.3   2.3   2.3   2.3   489.0211   523.0386   596.6902
    ## 15:   5.5   5.7   5.9   5.9   6.0   6.2   6.2  1268.3834  1656.4248  1883.4613
    ## 16:    NA    NA    NA    NA    NA    NA    NA         NA         NA         NA
    ## 17:   9.1   8.6   8.8   9.4   9.1   9.2   9.2  2729.4987  3154.3312  3606.0704
    ## 18:    NA    NA    NA    NA    NA    NA    NA  5323.4631  6920.1891  9101.2550
    ## 19:  10.6  10.5  10.4  10.7  11.0  11.1  11.1  3720.4792  4382.6173  5848.4764
    ## 20:  11.2  11.3  11.4  11.4  11.5  11.5  11.5 29961.2633 33769.1542 39432.9383
    ## 21:  12.4  12.4  12.2  12.3  12.4  12.4  12.4 43437.0631 46593.6022 53700.0053
    ## 22:  11.3  11.3  11.3  11.3  11.3  11.3  11.3  1826.9314  2300.7697  3065.6113
    ## 23:  10.9  11.1  11.3  11.3  11.4  11.4  11.5   546.7769   654.2838   830.4077
    ## 24:    NA    NA    NA    NA    NA    NA    NA         NA         NA         NA
    ## 25:   2.8   3.0   3.0   3.0   3.0   3.0   3.0         NA         NA         NA
    ## 26:   6.7   6.7   6.8   6.9   6.9   7.0   7.0   702.7409  1047.9192  1124.2906
    ##     scl11 scl12 scl13 scl14 scl15 scl16 scl17      GDP05      GDP06      GDP07
    ##          GDP08      GDP09      GDP10      GDP11      GDP12      GDP13
    ##  1: 48718.4969 43503.1855 40852.6668 43335.3289 38686.4613 39538.7667
    ##  2:  5574.6038  4950.2948  5842.8058  7189.6912  7496.2946  7875.7570
    ##  3:  6377.3697  5351.3554  6029.3968  6519.2302  6940.1593  7978.8726
    ##  4:  7265.7355  6988.2333  6812.4063  7809.4251  7395.8498  7655.1297
    ##  5: 35397.3637 32109.2425 31023.6383 32396.3857 28912.1569 27729.1927
    ##  6:  4249.0193  4231.6158  4633.5904  5200.5558  5682.0450  6056.3308
    ##  7: 53554.0389 47293.9928 46459.9733 51081.9977 47710.7902 49878.0432
    ##  8:  1076.7013  1150.2111  1172.0985  1287.9541  1337.3354  1393.9560
    ##  9: 39339.2976 40855.1756 44507.6764 48167.9973 48603.4766 40454.4475
    ## 10:  8513.5646  7165.2232  9070.4883 11634.0019 12386.7000 13890.6318
    ## 11: 55494.9510 37561.6727 38577.4983 48631.6913 51979.1052 49388.1374
    ## 12:         NA         NA         NA         NA         NA         NA
    ## 13:  8474.5868  7292.4944  9040.5663 10399.3728 10817.4429 10970.1233
    ## 14:   694.2777   698.8989   710.2742   837.6034   778.6193   805.0328
    ## 15:  2242.8719  1891.3354  2280.4374  2487.5982  2723.8228  2961.5503
    ## 16:         NA         NA         NA         NA         NA         NA
    ## 17:  4220.6170  4196.3128  5082.3548  5869.3231  6528.9722  6756.7528
    ## 18: 11635.2729  8562.8133 10674.9958 14311.0843 15420.8745 15974.6446
    ## 19:  7101.0401  6169.1142  5735.4229  6809.1598  6015.9452  6755.0737
    ## 20: 40007.4693 38927.2069 47236.9602 53890.4287 55546.4885 56967.4258
    ## 21: 56152.5523 46946.9603 52869.0443 60755.7596 58037.8213 61126.9432
    ## 22:  3887.2423  2542.9954  2965.1397  3569.7581  3855.4177  4029.7113
    ## 23:  1082.2860  1213.2653  1634.3121  1926.2930  2137.0251  2281.4110
    ## 24:         NA         NA         NA         NA         NA         NA
    ## 25:         NA         NA         NA         NA         NA         NA
    ## 26:  1394.0006  1159.9078  1489.4593  1672.9083  1763.0727  1878.9097
    ##          GDP08      GDP09      GDP10      GDP11      GDP12      GDP13
    ##          GDP14      GDP15      GDP16      GDP17 MMR05 MMR06 MMR07 MMR08 MMR09
    ##  1: 41303.9294 35762.5231 37474.6654 38962.8804    NA    NA    NA    NA    NA
    ##  2:  7891.3131  5500.3104  3880.7387  4147.0897    42    35    34    32    32
    ##  3:  8318.5127  5949.1063  5022.6266  5761.7471    11     9     7     6     6
    ##  4:  7876.8665  7055.9357  7548.8550  8334.0817    15    14    13    13    12
    ##  5: 27129.6261 23333.7149 24532.5191 26338.6943    12     9     8     9     8
    ##  6:  6377.0915  6124.4916  6060.0933  6213.5013    94    90    85    82    80
    ##  7: 50260.2999 42784.6984 43784.2840 46336.6633     5     4     4     4     4
    ##  8:  1402.1002  1389.1195  1265.9876  1294.2397   459   467   473   484   484
    ##  9: 38109.4121 34524.4699 38761.8182 38386.5111     7     7     6     6     6
    ## 10: 12807.2607 10510.7719  7714.8418  9247.5813    43    40    36    30    24
    ## 11: 44062.3170 29869.5294 27653.0668 29759.4365    10    10    10    10    10
    ## 12:         NA         NA         NA         NA    NA    NA    NA    NA    NA
    ## 13: 11319.0798  9955.2437  9817.7385 10259.1818    31    30    30    29    29
    ## 14:   848.2741   751.4748   780.7186   830.0184   691   675   663   661   661
    ## 15:  3098.9863  2687.4801  2176.0022  1968.5647  1080  1040  1010   996   987
    ## 16:         NA         NA         NA         NA    NA    NA    NA    NA    NA
    ## 17:  6672.8803  6229.1017  6204.9973  6710.5080   118   114   112   108   106
    ## 18: 14095.6487  9313.0136  8704.8984 10720.3326    42    36    32    30    27
    ## 19:  6600.0568  5588.9807  5765.2008  6292.5436    12    12    12    12    12
    ## 20: 57562.5308 55646.6187 56828.2953 60913.7453    13    13    12    11    10
    ## 21: 60020.3605 51545.4836 51965.1572 53791.5087     5     5     5     5     5
    ## 22:  3104.6432  2124.6623  2187.7305  2640.6757    33    31    33    33    27
    ## 23:  2492.3366  2615.0251  2567.7992  1826.5669    38    37    35    34    32
    ## 24:         NA         NA         NA         NA    NA    NA    NA    NA    NA
    ## 25:         NA         NA         NA         NA    NA    NA    NA    NA    NA
    ## 26:  1763.0626  1337.7956  1280.5784  1534.8668   421   406   387   356   329
    ##          GDP14      GDP15      GDP16      GDP17 MMR05 MMR06 MMR07 MMR08 MMR09
    ##     MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17               LMH09
    ##  1:    NA    NA    NA    NA    NA    NA    NA    NA         high income
    ##  2:    31    30    29    28    28    27    26    26 upper middle income
    ##  3:     5     5     4     3     3     3     3     2 upper middle income
    ##  4:    12    12    11    10    11    10    10    10 upper middle income
    ##  5:     8     8     7     6     8     7     6     6         high income
    ##  6:    78    76    71    67    65    63    61    59 lower middle income
    ##  7:     4     4     4     4     3     3     3     3         high income
    ##  8:   506   496   500   496   492   488   489   480          low income
    ##  9:     6     6     5     5     5     5     5     5         high income
    ## 10:    22    19    17    14    13    12    10    10 upper middle income
    ## 11:    10    10    11    11    11    11    12    12         high income
    ## 12:    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 13:    30    30    30    30    30    30    29    29 upper middle income
    ## 14:   660   663   663   663   642   620   590   562          low income
    ## 15:   978   972   963   951   943   931   925   917 lower middle income
    ## 16:    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 17:   104   102   100    98    96    94    91    88 upper middle income
    ## 18:    25    23    22    20    19    18    18    17 upper middle income
    ## 19:    12    12    12    12    12    13    12    12 upper middle income
    ## 20:    10    10    10     9     8     9     8     8         high income
    ## 21:     4     5     5     5     5     4     4     4         high income
    ## 22:    25    23    24    23    24    21    20    19 lower middle income
    ## 23:    31    32    32    31    30    30    29    29 lower middle income
    ## 24:    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 25:    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 26:   305   283   267   254   242   232   222   213          low income
    ##     MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17               LMH09
    ##                   LMH14
    ##  1:         high income
    ##  2: upper middle income
    ##  3: upper middle income
    ##  4: upper middle income
    ##  5:         high income
    ##  6: upper middle income
    ##  7:         high income
    ##  8:          low income
    ##  9:         high income
    ## 10: upper middle income
    ## 11:         high income
    ## 12:                <NA>
    ## 13: upper middle income
    ## 14:          low income
    ## 15: lower middle income
    ## 16:                <NA>
    ## 17: upper middle income
    ## 18:         high income
    ## 19: upper middle income
    ## 20:         high income
    ## 21:         high income
    ## 22: lower middle income
    ## 23: lower middle income
    ## 24:                <NA>
    ## 25:                <NA>
    ## 26: lower middle income
    ##                   LMH14

``` r
naphy17 <- edit17[, c("location_name", "phy09","phy10", "phy11", "phy12", "phy13", "phy14", "phy15", "phy16", "phy17")]
naphy17 #physican columns 09-17
```

    ##                 location_name  phy09  phy10  phy11  phy12  phy13  phy14  phy15
    ##  1:                   Algeria     NA 1.2070     NA     NA     NA     NA     NA
    ##  2:                   Andorra 3.1479 4.0000     NA     NA     NA     NA 3.3333
    ##  3:                 Argentina     NA 3.2100     NA     NA 3.9385     NA     NA
    ##  4:                   Armenia 2.7679 2.8419 2.8677 2.8968 2.9031 2.8928 2.9143
    ##  5:                 Australia 3.1085 3.3429 3.2878 3.2858 3.3530 3.4314 3.4886
    ##  6:                Azerbaijan 3.6751 3.6629 3.4376 3.4901 3.4558 3.4460     NA
    ##  7:                   Belarus 3.2845 3.2498 4.7611 4.8352 4.8534 5.0120 5.1905
    ##  8:                    Brazil 1.8171 1.8139 1.8491     NA 1.8820     NA     NA
    ##  9:                  Bulgaria 3.7401 3.7661 3.8467 3.8995 3.9630 3.9750 4.0332
    ## 10:              Burkina Faso 0.0446 0.0457 0.0386 0.0475 0.0470 0.0487 0.0657
    ## 11:                    Canada     NA 2.0384     NA 2.3962 2.4509 2.4961 2.5388
    ## 12:                     Chile 1.0294 1.4333 1.5854 1.7471 1.8806 2.0278 2.1470
    ## 13:                     China 1.3922 1.4532 1.4637 1.5367 1.6332 1.6877 1.7732
    ## 14:                  Colombia 1.5668 1.5917 1.6966 1.7400 1.8431 1.8534 1.9742
    ## 15:                    Cyprus 2.1064 2.1562 2.2484 2.2960 2.4076 2.4993 2.6236
    ## 16:                   Ecuador 1.5983 2.1111 1.6582     NA     NA     NA 2.0648
    ## 17:                   Estonia 3.2767 3.2422 3.2929 3.2822 3.3318 3.3564 3.4152
    ## 18:                  Ethiopia 0.0252 0.0220     NA     NA     NA     NA     NA
    ## 19:                   Finland 3.0861 3.2653 3.1221 3.2854 3.2966 3.3922 3.3309
    ## 20:                    France 3.3693 3.3736 3.1622 3.1745 3.1948 3.2115 3.2239
    ## 21:                   Georgia 4.2219 4.4466 4.5004 4.4868 4.5145 4.7754 5.0055
    ## 22:                   Germany 3.6638 3.7567 3.8491 3.9197 4.0087 4.0846 4.1342
    ## 23:                     Ghana 0.0841 0.0938 0.0979 0.0956 0.1686     NA     NA
    ## 24:                     Haiti     NA     NA 0.1378     NA     NA     NA 0.0852
    ## 25:                   Hungary 3.0399 2.8896 2.9811 3.1062 3.2284 3.3443 3.1178
    ## 26:                     India 0.6220 0.6616 0.7376 0.6982 0.7182 0.7247     NA
    ## 27:                 Indonesia 0.1448 0.1395     NA 0.3075 0.3118     NA 0.2738
    ## 28: Iran, Islamic Republic of     NA 0.8900     NA     NA     NA 1.5044 1.1526
    ## 29:                      Iraq     NA 0.6636     NA     NA     NA 1.0364     NA
    ## 30:                     Japan     NA 2.2059     NA 2.2740     NA 2.3413     NA
    ## 31:                    Jordan 2.1287 2.2326     NA     NA 2.1947 2.2038 2.8078
    ## 32:                Kazakhstan 3.7808 3.9278 3.9483 3.9108 3.9508 3.9800     NA
    ## 33:       Korea (Republic of) 1.9185 1.9839 2.0361 2.0798 2.1632 2.2070 2.2494
    ## 34:                    Kuwait 1.8929 2.4296 2.4560 2.4662 2.5332 2.6522 2.6463
    ## 35:                Kyrgyzstan     NA     NA     NA     NA     NA     NA     NA
    ## 36:                   Lebanon     NA 2.3385 2.6201     NA 2.2486 2.0735 2.1030
    ## 37:                     Libya 1.9578 1.9000     NA     NA     NA 2.0583 1.8943
    ## 38:                  Malaysia     NA 1.1691 1.2777 1.3320     NA     NA 1.5358
    ## 39:                      Mali 0.0923 0.1021 0.1037 0.1071     NA     NA     NA
    ## 40:                    Mexico 1.9790 2.2362 2.1004 2.1118 2.1568 2.2070 2.3246
    ## 41:      Moldova, Republic of 2.3912 2.3808 2.4142 2.4135 2.5028 2.4746 2.4836
    ## 42:                   Morocco 0.6477 0.6200     NA     NA 0.6320 0.9135     NA
    ## 43:               Netherlands 2.9057 2.9516 3.1243 3.2463 3.3058 3.3468 3.4917
    ## 44:               New Zealand 3.0477 3.0658 3.1731 3.1957 3.2303 3.2374 3.2961
    ## 45:                   Nigeria 0.3782 0.1836     NA     NA 0.3828     NA     NA
    ## 46:                    Norway 2.4756 2.4949 2.5441 2.5532 2.5991 2.6832 2.6474
    ## 47:                  Pakistan 0.7951 0.8076 0.8311 0.8590 0.8771 0.8972 0.9262
    ## 48:       Palestine, State of     NA     NA     NA     NA     NA     NA     NA
    ## 49:                      Peru 0.9472 0.9200     NA 1.1411     NA     NA     NA
    ## 50:               Philippines 1.2632 1.2717     NA     NA     NA     NA     NA
    ## 51:                    Poland 2.1593 2.1707 2.1997 2.2242 2.2340 2.3020 2.3252
    ## 52:                     Qatar 3.0975 3.7273     NA     NA     NA 1.7351     NA
    ## 53:                   Romania 2.4414 2.4804 2.5153 2.5887 2.6240     NA     NA
    ## 54:        Russian Federation 2.3924 2.3930 6.6305 4.1303 4.0705 4.0114 3.7494
    ## 55:                    Rwanda 0.0550 0.0550 0.0129     NA 0.1021 0.0909 0.1129
    ## 56:                    Serbia 2.4467 2.4820 2.5003 2.4961 2.4945 2.4611 2.4603
    ## 57:                 Singapore 1.6758 1.7187 1.7232 1.7964 1.8958 2.0125     NA
    ## 58:                  Slovenia 2.4167 2.4367 2.4965 2.5406 2.6252 2.7628 2.8148
    ## 59:              South Africa 0.7074 0.7341 0.7179 0.7254 0.7422 0.7541 0.7814
    ## 60:                     Spain 3.0395 3.0292 3.0473 3.1396 3.1425 3.1657 3.2166
    ## 61:                    Sweden 3.8065 3.8779 3.9569 4.0368 4.1259 4.2037 4.2856
    ## 62:               Switzerland 3.8476 3.8166 3.8355 3.9102 4.0303 4.1171 4.1898
    ## 63:                  Thailand 0.3387 0.3906     NA     NA     NA     NA 0.4651
    ## 64:       Trinidad and Tobago 1.5081 1.8086 1.8193     NA     NA     NA 2.6498
    ## 65:                   Tunisia     NA 1.2220     NA     NA     NA 1.2769 1.3108
    ## 66:                    Turkey 1.6635 1.7068 1.7160 1.7384 1.7619 1.7560 1.7988
    ## 67:                   Ukraine 3.4904 3.4830 3.4842 3.4910 3.4965 2.9923     NA
    ## 68:            United Kingdom 2.6279 2.6265 2.6603 2.6667 2.6774 2.7149 2.7465
    ## 69:             United States 2.4471 2.4354 2.4641 2.4985 2.5596 2.5740 2.5781
    ## 70:                   Uruguay     NA 3.7360     NA     NA     NA     NA     NA
    ## 71:                Uzbekistan 2.5706 2.5432 2.5036 2.4451 2.4044 2.3742     NA
    ## 72:                  Viet Nam     NA     NA     NA     NA     NA     NA     NA
    ## 73:                     Yemen     NA     NA     NA     NA     NA     NA     NA
    ## 74:                    Zambia 0.0606 0.0614 0.1649 0.1658     NA     NA     NA
    ## 75:                  Zimbabwe 0.1210 0.1272 0.0817 0.0807 0.0834 0.1240 0.1815
    ##                 location_name  phy09  phy10  phy11  phy12  phy13  phy14  phy15
    ##      phy16  phy17
    ##  1: 1.8325 1.7879
    ##  2:     NA     NA
    ##  3: 4.0013 3.9901
    ##  4:     NA 4.4023
    ##  5: 3.5672 3.6778
    ##  6:     NA     NA
    ##  7:     NA     NA
    ##  8:     NA 2.1652
    ##  9:     NA     NA
    ## 10: 0.0645 0.0847
    ## 11: 2.3105 2.6102
    ## 12: 2.2937 2.4411
    ## 13: 1.8647 1.9798
    ## 14: 2.0335 2.1064
    ## 15: 1.9509     NA
    ## 16: 2.0368     NA
    ## 17: 3.4546 3.4629
    ## 18: 0.0464 0.0986
    ## 19: 3.8118     NA
    ## 20: 3.2376 3.2565
    ## 21: 5.9973 6.1297
    ## 22: 4.1944 4.2488
    ## 23: 0.1270 0.1359
    ## 24:     NA     NA
    ## 25: 3.2313 3.3447
    ## 26: 0.7590 0.7779
    ## 27:     NA 0.3767
    ## 28:     NA 1.1292
    ## 29: 0.8536 0.8375
    ## 30: 2.4115     NA
    ## 31: 1.3954 2.3237
    ## 32:     NA     NA
    ## 33: 2.3037 2.3608
    ## 34:     NA     NA
    ## 35:     NA     NA
    ## 36: 2.0115 2.0255
    ## 37:     NA 2.0905
    ## 38:     NA     NA
    ## 39: 0.1395     NA
    ## 40: 2.3244 2.3827
    ## 41:     NA 3.2066
    ## 42:     NA 0.7308
    ## 43: 3.5470 3.6054
    ## 44: 3.3833 3.4728
    ## 45: 0.4494     NA
    ## 46: 2.6983 2.8342
    ## 47: 0.9620 1.0005
    ## 48:     NA     NA
    ## 49: 1.3048     NA
    ## 50:     NA 0.6004
    ## 51: 2.4146 2.3788
    ## 52: 2.6944 0.0008
    ## 53: 2.2565 2.9807
    ## 54: 4.0139     NA
    ## 55: 0.1226 0.1371
    ## 56: 3.1131     NA
    ## 57: 2.2936     NA
    ## 58: 3.0007 3.0861
    ## 59: 0.7997 0.9054
    ## 60: 3.8112 3.8723
    ## 61: 3.9840     NA
    ## 62: 4.2473 4.2957
    ## 63: 0.4450 0.8075
    ## 64:     NA 3.3646
    ## 65: 1.2834 1.3025
    ## 66: 1.8142 1.8492
    ## 67:     NA     NA
    ## 68: 2.7563 2.7863
    ## 69: 2.5881 2.6120
    ## 70: 3.9558 5.0794
    ## 71:     NA     NA
    ## 72:     NA     NA
    ## 73:     NA     NA
    ## 74: 0.1628     NA
    ## 75: 0.1788 0.1859
    ##      phy16  phy17

``` r
na.omit(naphy17, c("phy17"), invert = TRUE) #able to see if can replace values
```

    ##           location_name  phy09  phy10  phy11  phy12  phy13  phy14  phy15  phy16
    ##  1:             Andorra 3.1479 4.0000     NA     NA     NA     NA 3.3333     NA
    ##  2:          Azerbaijan 3.6751 3.6629 3.4376 3.4901 3.4558 3.4460     NA     NA
    ##  3:             Belarus 3.2845 3.2498 4.7611 4.8352 4.8534 5.0120 5.1905     NA
    ##  4:            Bulgaria 3.7401 3.7661 3.8467 3.8995 3.9630 3.9750 4.0332     NA
    ##  5:              Cyprus 2.1064 2.1562 2.2484 2.2960 2.4076 2.4993 2.6236 1.9509
    ##  6:             Ecuador 1.5983 2.1111 1.6582     NA     NA     NA 2.0648 2.0368
    ##  7:             Finland 3.0861 3.2653 3.1221 3.2854 3.2966 3.3922 3.3309 3.8118
    ##  8:               Haiti     NA     NA 0.1378     NA     NA     NA 0.0852     NA
    ##  9:               Japan     NA 2.2059     NA 2.2740     NA 2.3413     NA 2.4115
    ## 10:          Kazakhstan 3.7808 3.9278 3.9483 3.9108 3.9508 3.9800     NA     NA
    ## 11:              Kuwait 1.8929 2.4296 2.4560 2.4662 2.5332 2.6522 2.6463     NA
    ## 12:          Kyrgyzstan     NA     NA     NA     NA     NA     NA     NA     NA
    ## 13:            Malaysia     NA 1.1691 1.2777 1.3320     NA     NA 1.5358     NA
    ## 14:                Mali 0.0923 0.1021 0.1037 0.1071     NA     NA     NA 0.1395
    ## 15:             Nigeria 0.3782 0.1836     NA     NA 0.3828     NA     NA 0.4494
    ## 16: Palestine, State of     NA     NA     NA     NA     NA     NA     NA     NA
    ## 17:                Peru 0.9472 0.9200     NA 1.1411     NA     NA     NA 1.3048
    ## 18:  Russian Federation 2.3924 2.3930 6.6305 4.1303 4.0705 4.0114 3.7494 4.0139
    ## 19:              Serbia 2.4467 2.4820 2.5003 2.4961 2.4945 2.4611 2.4603 3.1131
    ## 20:           Singapore 1.6758 1.7187 1.7232 1.7964 1.8958 2.0125     NA 2.2936
    ## 21:              Sweden 3.8065 3.8779 3.9569 4.0368 4.1259 4.2037 4.2856 3.9840
    ## 22:             Ukraine 3.4904 3.4830 3.4842 3.4910 3.4965 2.9923     NA     NA
    ## 23:          Uzbekistan 2.5706 2.5432 2.5036 2.4451 2.4044 2.3742     NA     NA
    ## 24:            Viet Nam     NA     NA     NA     NA     NA     NA     NA     NA
    ## 25:               Yemen     NA     NA     NA     NA     NA     NA     NA     NA
    ## 26:              Zambia 0.0606 0.0614 0.1649 0.1658     NA     NA     NA 0.1628
    ##           location_name  phy09  phy10  phy11  phy12  phy13  phy14  phy15  phy16
    ##     phy17
    ##  1:    NA
    ##  2:    NA
    ##  3:    NA
    ##  4:    NA
    ##  5:    NA
    ##  6:    NA
    ##  7:    NA
    ##  8:    NA
    ##  9:    NA
    ## 10:    NA
    ## 11:    NA
    ## 12:    NA
    ## 13:    NA
    ## 14:    NA
    ## 15:    NA
    ## 16:    NA
    ## 17:    NA
    ## 18:    NA
    ## 19:    NA
    ## 20:    NA
    ## 21:    NA
    ## 22:    NA
    ## 23:    NA
    ## 24:    NA
    ## 25:    NA
    ## 26:    NA
    ##     phy17

REPLACE

``` r
Andorra <- edit17["Andorra", c("location_name", "phy15")]
Azerbaijan  <- edit17["Azerbaijan", c("location_name", "phy14")]
Belarus <- edit17["Belarus", c("location_name", "phy15")]
Bulgaria    <-  edit17["Bulgaria", c("location_name", "phy15")]
Cyprus  <- edit17["Cyprus", c("location_name", "phy16")]
Ecuador <- edit17["Ecuador", c("location_name", "phy16")]
Finland     <- edit17["Finland", c("location_name", "phy16")]
Haiti       <-  edit17["Haiti", c("location_name", "phy15")]
Japan       <- edit17["Japan", c("location_name", "phy16")]
Kazakhstan  <- edit17["Kazakhstan", c("location_name", "phy14")]
Kuwait  <-  edit17["Kuwait", c("location_name", "phy15")]
#Kyrgyzstan     <- NA
Malaysia        <- edit17["Malaysia", c("location_name", "phy15")]
Mali        <- edit17["Mali", c("location_name", "phy16")]
Nigeria     <- edit17["Nigeria", c("location_name", "phy16")]
#Palestine, State of            <- NA
Peru        <- edit17["Peru", c("location_name", "phy16")]
RussianFederation   <- edit17["Russian Federation", c("location_name", "phy16")]
Serbia      <- edit17["Serbia", c("location_name", "phy16")]
Singapore   <- edit17["Singapore", c("location_name", "phy16")]
Sweden      <- edit17["Sweden", c("location_name", "phy16")]
Ukraine     <- edit17["Ukraine", c("location_name", "phy14")]
Uzbekistan      <- edit17["Uzbekistan", c("location_name", "phy14")]
#Viet Nam   <- NA
#Yemen      <- NA
Zambia  <- edit17["Zambia", c("location_name", "phy16")]
```

need to create table with missing phy14 values in one column the can use
rows\_patch to insert new values

``` r
phy17edit <- rbind(Andorra,
Azerbaijan,
Belarus ,
Bulgaria    ,
Cyprus  ,
Ecuador ,
Finland     ,
Haiti   ,
Japan       ,
Kazakhstan  ,
Kuwait  ,
Malaysia    ,
Mali    ,
Nigeria     ,
Peru        ,
RussianFederation,
Serbia  ,
Singapore   ,
Sweden  ,
Ukraine ,
Uzbekistan  ,
Zambia  ,
use.names=FALSE)

setnames(phy17edit, "phy15", "phy17edit") #new table with values to insert into the phy14 column
phy17edit
```

    ##          location_name phy17edit
    ##  1:            Andorra    3.3333
    ##  2:         Azerbaijan    3.4460
    ##  3:            Belarus    5.1905
    ##  4:           Bulgaria    4.0332
    ##  5:             Cyprus    1.9509
    ##  6:            Ecuador    2.0368
    ##  7:            Finland    3.8118
    ##  8:              Haiti    0.0852
    ##  9:              Japan    2.4115
    ## 10:         Kazakhstan    3.9800
    ## 11:             Kuwait    2.6463
    ## 12:           Malaysia    1.5358
    ## 13:               Mali    0.1395
    ## 14:            Nigeria    0.4494
    ## 15:               Peru    1.3048
    ## 16: Russian Federation    4.0139
    ## 17:             Serbia    3.1131
    ## 18:          Singapore    2.2936
    ## 19:             Sweden    3.9840
    ## 20:            Ukraine    2.9923
    ## 21:         Uzbekistan    2.3742
    ## 22:             Zambia    0.1628
    ##          location_name phy17edit

``` r
setnames(edit17, "phy17", "phy17edit")

edit_phy17 <- rows_patch(edit17, phy17edit)
```

    ## Matching, by = "location_name"

``` r
edit_phy17[,c("location_name", "phy17edit")]
```

    ##                 location_name phy17edit
    ##  1:                   Algeria    1.7879
    ##  2:                   Andorra    3.3333
    ##  3:                 Argentina    3.9901
    ##  4:                   Armenia    4.4023
    ##  5:                 Australia    3.6778
    ##  6:                Azerbaijan    3.4460
    ##  7:                   Belarus    5.1905
    ##  8:                    Brazil    2.1652
    ##  9:                  Bulgaria    4.0332
    ## 10:              Burkina Faso    0.0847
    ## 11:                    Canada    2.6102
    ## 12:                     Chile    2.4411
    ## 13:                     China    1.9798
    ## 14:                  Colombia    2.1064
    ## 15:                    Cyprus    1.9509
    ## 16:                   Ecuador    2.0368
    ## 17:                   Estonia    3.4629
    ## 18:                  Ethiopia    0.0986
    ## 19:                   Finland    3.8118
    ## 20:                    France    3.2565
    ## 21:                   Georgia    6.1297
    ## 22:                   Germany    4.2488
    ## 23:                     Ghana    0.1359
    ## 24:                     Haiti    0.0852
    ## 25:                   Hungary    3.3447
    ## 26:                     India    0.7779
    ## 27:                 Indonesia    0.3767
    ## 28: Iran, Islamic Republic of    1.1292
    ## 29:                      Iraq    0.8375
    ## 30:                     Japan    2.4115
    ## 31:                    Jordan    2.3237
    ## 32:                Kazakhstan    3.9800
    ## 33:       Korea (Republic of)    2.3608
    ## 34:                    Kuwait    2.6463
    ## 35:                Kyrgyzstan        NA
    ## 36:                   Lebanon    2.0255
    ## 37:                     Libya    2.0905
    ## 38:                  Malaysia    1.5358
    ## 39:                      Mali    0.1395
    ## 40:                    Mexico    2.3827
    ## 41:      Moldova, Republic of    3.2066
    ## 42:                   Morocco    0.7308
    ## 43:               Netherlands    3.6054
    ## 44:               New Zealand    3.4728
    ## 45:                   Nigeria    0.4494
    ## 46:                    Norway    2.8342
    ## 47:                  Pakistan    1.0005
    ## 48:       Palestine, State of        NA
    ## 49:                      Peru    1.3048
    ## 50:               Philippines    0.6004
    ## 51:                    Poland    2.3788
    ## 52:                     Qatar    0.0008
    ## 53:                   Romania    2.9807
    ## 54:        Russian Federation    4.0139
    ## 55:                    Rwanda    0.1371
    ## 56:                    Serbia    3.1131
    ## 57:                 Singapore    2.2936
    ## 58:                  Slovenia    3.0861
    ## 59:              South Africa    0.9054
    ## 60:                     Spain    3.8723
    ## 61:                    Sweden    3.9840
    ## 62:               Switzerland    4.2957
    ## 63:                  Thailand    0.8075
    ## 64:       Trinidad and Tobago    3.3646
    ## 65:                   Tunisia    1.3025
    ## 66:                    Turkey    1.8492
    ## 67:                   Ukraine    2.9923
    ## 68:            United Kingdom    2.7863
    ## 69:             United States    2.6120
    ## 70:                   Uruguay    5.0794
    ## 71:                Uzbekistan    2.3742
    ## 72:                  Viet Nam        NA
    ## 73:                     Yemen        NA
    ## 74:                    Zambia    0.1628
    ## 75:                  Zimbabwe    0.1859
    ##                 location_name phy17edit

``` r
na.omit(edit_phy17, "phy17edit", invert=TRUE)
```

    ##          location_name GSNI_PERIOD onebias twobias nobias political economic
    ## 1:          Kyrgyzstan   2010–2014   96.73   84.87   3.27      76.8    71.53
    ## 2: Palestine, State of   2010–2014   98.00   92.30   2.00      89.3    79.50
    ## 3:            Viet Nam   2005–2009   92.89   69.17   7.11      59.4    62.49
    ## 4:               Yemen   2010–2014   97.80   92.10   2.20      87.4    87.20
    ##    educational physical cvd05fem cvd06fem cvd07fem cvd08fem cvd09fem cvd10fem
    ## 1:       41.00    81.73 546.8872 553.7142 543.2533 536.8522 507.4175 488.1873
    ## 2:       26.70    83.50 367.3750 362.0774 355.8204 350.5859 343.9703 341.7886
    ## 3:       20.36    70.56 256.6274 256.6231 255.9745 255.1929 254.4622 252.5587
    ## 4:       45.30    81.00 493.0600 489.2440 485.1008 483.3384 478.3407 472.1759
    ##    cvd11fem cvd12fem cvd13fem cvd14fem cvd15fem cvd16fem cvd17fem cvd18fem
    ## 1: 483.2938 465.0509 442.7975 429.3656 435.0685 415.0933 408.2852 395.2001
    ## 2: 337.6966 320.1783 314.6376 323.3969 338.5276 350.9355 354.1550 349.6899
    ## 3: 250.1793 247.0552 243.0562 239.2128 234.9403 230.6141 226.8228 223.3228
    ## 4: 471.3592 468.9990 467.1613 464.4741 466.8673 466.8844 469.9199 473.8849
    ##    cvd19fem cvd05male cvd06male cvd07male cvd08male cvd09male cvd10male
    ## 1: 390.5737  748.0504  780.9492  781.9687  761.4547  734.8094  702.9963
    ## 2: 345.6669  524.2705  515.4991  498.1858  484.7670  474.6089  468.5019
    ## 3: 219.5527  448.4061  449.7884  453.6026  455.8597  457.1055  456.0361
    ## 4: 476.9648  579.9540  575.3879  568.8200  566.4953  558.3360  547.3893
    ##    cvd11male cvd12male cvd13male cvd14male cvd15male cvd16male cvd17male
    ## 1:  649.3122  632.0656  609.2108  620.6342  644.2163  618.5638  613.3843
    ## 2:  460.4963  432.5445  413.7575  433.8364  442.4321  433.5059  438.1050
    ## 3:  453.6313  450.9162  449.0089  446.5765  443.7173  440.1349  436.8855
    ## 4:  545.5135  541.5427  538.4431  532.5702  536.2482  536.1051  541.0473
    ##    cvd18male cvd19male cvd05both cvd06both cvd07both cvd08both cvd09both
    ## 1:  580.3176  569.3491  634.0089  651.1682  645.5907  632.7615  603.3141
    ## 2:  440.8897  433.9799  434.1983  427.1939  415.8933  407.1588  399.0904
    ## 3:  433.6069  429.9362  334.7621  335.3238  336.5239  337.0268  337.1512
    ## 4:  547.2489  550.6548  535.1088  530.9694  525.7088  523.7193  517.1861
    ##    cvd10both cvd11both cvd12both cvd13both cvd14both cvd15both cvd16both
    ## 1:  581.8522  562.6225  543.9251  519.5371  513.0161  523.1598  500.1438
    ## 2:  394.3262  387.9774  366.8452  355.9001  368.8565  380.8886  384.4684
    ## 3:  335.4539  333.0107  329.9312  326.6030  323.1828  319.1363  314.9546
    ## 4:  508.7890  507.4918  504.3873  501.9957  497.8142  500.9039  500.8572
    ##    cvd17both cvd18both cvd19both LEbirth2000 LE602000 LEbirth2010 LE602010
    ## 1:  493.8208  473.2671  466.3130        70.2     18.3        73.3     19.3
    ## 2:  388.1755  386.6795  381.6978          NA       NA          NA       NA
    ## 3:  311.2375  307.7080  303.9047        75.6     21.0        77.1     21.5
    ## 4:  504.8275  509.8769  513.0910        64.7     17.9        69.6     18.7
    ##    LEbirth2015 LE602015 LEbirth2019 LE602019 phy05 phy06 phy07 phy08 phy09
    ## 1:        75.3     20.2       77.30     21.7    NA    NA    NA    NA    NA
    ## 2:          NA       NA       76.47       NA    NA    NA    NA    NA    NA
    ## 3:        77.6     21.8       78.10     22.0    NA    NA    NA    NA    NA
    ## 4:        69.6     18.9       68.90     18.7    NA    NA    NA    NA    NA
    ##    phy10 phy11 phy12 phy13 phy14 phy15 phy16 phy17edit HE05 HE06 HE07 HE08 HE09
    ## 1:    NA    NA    NA    NA    NA    NA    NA        NA <NA> <NA> <NA> <NA> <NA>
    ## 2:    NA    NA    NA    NA    NA    NA    NA        NA <NA> <NA> <NA> <NA> <NA>
    ## 3:    NA    NA    NA    NA    NA    NA    NA        NA <NA> <NA> <NA> <NA> <NA>
    ## 4:    NA    NA    NA    NA    NA    NA    NA        NA <NA> <NA> <NA> <NA> <NA>
    ##    HE10 HE11 HE12 HE13 HE14 HE15 HE16 HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ## 1: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>  10.2  10.2  10.3  10.3  10.4  10.6
    ## 2: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>    NA    NA    NA    NA    NA    NA
    ## 3: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>    NA    NA    NA    NA    NA    NA
    ## 4: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>   1.9   2.0   2.2   2.3   2.5   2.6
    ##    scl11 scl12 scl13 scl14 scl15 scl16 scl17 GDP05 GDP06 GDP07 GDP08 GDP09
    ## 1:  10.6  10.7  10.7  10.8  10.8  10.9  10.9    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:   2.8   3.0   3.0   3.0   3.0   3.0   3.0    NA    NA    NA    NA    NA
    ##    GDP10 GDP11 GDP12 GDP13 GDP14 GDP15 GDP16 GDP17 MMR05 MMR06 MMR07 MMR08
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##    MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17 LMH09 LMH14
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    NA  <NA>  <NA>

``` r
#table "edit_phy17" now has column "phy17edit" which has inserted values from other years
# 4 NAs in total for phy
```

\#\#\#\#scl17

``` r
na.omit(edit_phy17, c("scl17"), invert = TRUE) # 6 missing scl 2017
```

    ##                location_name GSNI_PERIOD onebias twobias nobias political
    ## 1: Iran, Islamic Republic of   2005–2009   98.54   92.49   1.46     84.63
    ## 2:       Korea (Republic of)   2010–2014   87.07   62.91  12.93     63.68
    ## 3:      Moldova, Republic of   2005–2009   90.06   67.21   9.94     60.33
    ## 4:       Palestine, State of   2010–2014   98.00   92.30   2.00     89.30
    ## 5:        Russian Federation   2010–2014   86.83   68.56  13.17     68.43
    ## 6:                  Viet Nam   2005–2009   92.89   69.17   7.11     59.40
    ##    economic educational physical cvd05fem cvd06fem cvd07fem cvd08fem cvd09fem
    ## 1:    88.86       55.42    78.69 346.4549 333.0393 318.5381 305.9814 298.0127
    ## 2:    54.33       25.67    58.27 155.9141 143.8054 132.6713 120.8555 111.7862
    ## 3:    58.80       16.73    65.20 513.3270 491.9931 497.1151 484.5283 467.9101
    ## 4:    79.50       26.70    83.50 367.3750 362.0774 355.8204 350.5859 343.9703
    ## 5:    58.77       22.66    50.02 569.5149 527.8242 500.5882 492.7038 467.5455
    ## 6:    62.49       20.36    70.56 256.6274 256.6231 255.9745 255.1929 254.4622
    ##    cvd10fem cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem  cvd16fem
    ## 1: 290.3670 283.4457 278.28489 277.07263 275.18836 277.67423 278.51259
    ## 2: 105.3449 100.1279  95.58209  90.21631  85.58348  83.53081  81.91117
    ## 3: 459.1067 400.9103 387.36233 372.28763 377.82255 382.27748 381.40961
    ## 4: 341.7886 337.6966 320.17827 314.63758 323.39695 338.52757 350.93555
    ## 5: 459.6649 424.4358 406.44126 391.62857 386.28781 376.79609 366.11070
    ## 6: 252.5587 250.1793 247.05521 243.05616 239.21285 234.94030 230.61406
    ##     cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ## 1: 277.88105 271.04348 268.72894  400.2154  389.5281  376.5086  362.0445
    ## 2:  81.88305  82.38176  83.11824  190.0032  178.2896  167.8587  158.2836
    ## 3: 368.12912 361.05127 341.11337  702.8658  671.8424  666.6718  648.2151
    ## 4: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 5: 348.59567 349.51106 351.22730  956.8934  876.9782  830.0730  825.6957
    ## 6: 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026  455.8597
    ##    cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ## 1:  350.5229  338.1660  323.1285  311.8184  304.8604  299.1565  300.1241
    ## 2:  149.4912  143.7630  137.6913  131.7820  123.9082  117.5575  114.1778
    ## 3:  638.1296  641.1604  572.9933  565.3869  534.3178  552.1560  587.3132
    ## 4:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 5:  776.4243  770.5636  706.6609  675.9903  650.0326  646.2977  623.4263
    ## 6:  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765  443.7173
    ##    cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ## 1:  297.2979  294.9042  290.2879  288.5816  374.2741  361.9923  347.9814
    ## 2:  110.9073  110.6175  108.2689  108.8687  172.0355  160.0969  148.9444
    ## 3:  566.6405  523.8055  511.8000  498.5963  589.7769  564.1553  566.4516
    ## 4:  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939  415.8933
    ## 5:  604.2681  561.3748  549.7797  549.1739  727.6035  670.5814  635.1627
    ## 6:  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238  336.5239
    ##    cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ## 1:  334.1940  324.1761  313.9673  302.7896  294.4027  290.2060  286.3140
    ## 2:  137.6671  128.5412  122.2008  116.6089  111.6049  105.3768  100.1378
    ## 3:  552.6234  539.0321  535.4923  471.5840  460.0871  438.8196  449.0384
    ## 4:  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001  368.8565
    ## 5:  628.2161  592.7751  585.0586  537.7079  514.5833  495.2656  490.3853
    ## 6:  337.0268  337.1512  335.4539  333.0107  329.9312  326.6030  323.1828
    ##    cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ## 1: 287.96550 286.88535 285.32918 279.68642  277.7336        74.6     20.5
    ## 2:  97.49873  95.22265  95.17711  94.68896   95.4053        79.8     22.7
    ## 3: 463.27943 456.27984 432.82716 423.77893  404.9551        70.5     17.2
    ## 4: 380.88863 384.46844 388.17552 386.67955  381.6978          NA       NA
    ## 5: 475.65621 461.71309 434.85592 431.69136  432.9186        72.3     18.7
    ## 6: 319.13626 314.95461 311.23748 307.70802  303.9047        75.6     21.0
    ##    LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05  phy06
    ## 1:        78.0     22.3        78.5     22.3       79.10     22.5 0.8869 0.5361
    ## 2:        83.8     25.9        85.1     26.9       86.10     27.9 1.7529 1.8047
    ## 3:        73.1     18.2        75.0     19.5       77.10     20.9 2.3783 2.3768
    ## 4:          NA       NA          NA       NA       76.47       NA     NA     NA
    ## 5:        74.7     20.2        76.6     21.4       78.00     22.2 2.3204 2.3721
    ## 6:        77.1     21.5        77.6     21.8       78.10     22.0     NA     NA
    ##     phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14  phy15  phy16
    ## 1:     NA     NA     NA 0.8900     NA     NA     NA 1.5044 1.1526     NA
    ## 2: 1.8655 1.8407 1.9185 1.9839 2.0361 2.0798 2.1632 2.2070 2.2494 2.3037
    ## 3: 2.3871 2.3545 2.3912 2.3808 2.4142 2.4135 2.5028 2.4746 2.4836     NA
    ## 4:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 5: 2.3841 2.3812 2.3924 2.3930 6.6305 4.1303 4.0705 4.0114 3.7494 4.0139
    ## 6:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ##    phy17edit               HE05               HE06               HE07
    ## 1:    1.1292         5.30572176 5.1986260399999997         5.03977919
    ## 2:    2.3608 4.6178216900000004 4.9439678200000001 5.1149072599999998
    ## 3:    3.2066 7.9639606499999998 8.7857027100000007 9.1826009800000001
    ## 4:        NA               <NA>               <NA>               <NA>
    ## 5:    4.0139         4.76693487         4.76170969 4.7431106600000001
    ## 6:        NA               <NA>               <NA>               <NA>
    ##                  HE08               HE09               HE10               HE11
    ## 1: 5.2816843999999996 6.5595455200000004 6.7547311800000003 6.6072506899999999
    ## 2:         5.39896727 5.7822899799999998 5.9173440900000003         6.00844383
    ## 3: 9.1230525999999994        11.39545822 10.131128309999999 9.0967035299999992
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5: 4.8986678100000001 5.6382026700000001 4.9660777999999999 4.7900524100000004
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ##                  HE12               HE13               HE14               HE15
    ## 1: 6.6364860500000002         5.99379873 6.9135108000000001 7.7605791100000001
    ## 2:         6.13262033 6.2478942899999996 6.4743809700000003 6.6527166400000004
    ## 3: 9.1396207799999996 8.6830034299999994 8.6327571899999995 8.5576353100000002
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5: 4.9408035300000002 5.0798091899999998 5.1802287099999997 5.2956042300000004
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ##                  HE16               HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ## 1: 8.8595066100000004 8.6596641499999993    NA    NA    NA    NA    NA    NA
    ## 2: 6.9143271400000001         7.10694933    NA    NA    NA    NA    NA    NA
    ## 3: 7.5355224600000001 7.0132851599999997    NA    NA    NA    NA    NA    NA
    ## 4:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 5: 5.2652196900000003         5.34388065    NA    NA    NA    NA    NA    NA
    ## 6:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ##    scl11 scl12 scl13 scl14 scl15 scl16 scl17    GDP05    GDP06    GDP07
    ## 1:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ## 5:    NA    NA    NA    NA    NA    NA    NA 5323.463 6920.189 9101.255
    ## 6:    NA    NA    NA    NA    NA    NA    NA       NA       NA       NA
    ##       GDP08    GDP09 GDP10    GDP11    GDP12    GDP13    GDP14    GDP15
    ## 1:       NA       NA    NA       NA       NA       NA       NA       NA
    ## 2:       NA       NA    NA       NA       NA       NA       NA       NA
    ## 3:       NA       NA    NA       NA       NA       NA       NA       NA
    ## 4:       NA       NA    NA       NA       NA       NA       NA       NA
    ## 5: 11635.27 8562.813 10675 14311.08 15420.87 15974.64 14095.65 9313.014
    ## 6:       NA       NA    NA       NA       NA       NA       NA       NA
    ##       GDP16    GDP17 MMR05 MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13
    ## 1:       NA       NA    34    32    30    28    25    22    19    18    17
    ## 2:       NA       NA    15    14    15    15    16    15    14    13    13
    ## 3:       NA       NA    34    31    31    29    28    29    21    22    21
    ## 4:       NA       NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5: 8704.898 10720.33    42    36    32    30    27    25    23    22    20
    ## 6:       NA       NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##    MMR14 MMR15 MMR16 MMR17               LMH09               LMH14
    ## 1:    17    17    16    16 upper middle income upper middle income
    ## 2:    12    12    11    11         high income         high income
    ## 3:    23    22    20    19 lower middle income lower middle income
    ## 4:    NA    NA    NA    NA                <NA>                <NA>
    ## 5:    19    18    18    17 upper middle income         high income
    ## 6:    NA    NA    NA    NA                <NA>                <NA>

``` r
nascl <- edit_phy17[, c("location_name", "scl05", "scl06", "scl07", "scl08", "scl09","scl10", "scl11", "scl12", "scl13", "scl14", "scl15", "scl16", "scl17")]
nascl #scl columns 09-17
```

    ##                 location_name scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12
    ##  1:                   Algeria   6.9   7.0   7.2   6.7   6.9   7.1   7.4   7.6
    ##  2:                   Andorra   9.8  10.1  10.1  10.1  10.1  10.1  10.2  10.2
    ##  3:                 Argentina   9.1   9.0   8.9   8.9   9.3   9.8   9.8   9.8
    ##  4:                   Armenia  10.9  10.9  11.0  11.0  11.1  11.1  11.2  11.3
    ##  5:                 Australia  11.7  11.9  12.0  12.3  12.3  12.4  12.5  12.6
    ##  6:                Azerbaijan  10.7  10.7  10.2  10.2  10.7  10.7  10.7  10.7
    ##  7:                   Belarus   9.3  10.0  10.6  11.3  11.9  12.0  12.0  12.0
    ##  8:                    Brazil   6.3   6.4   6.5   6.7   6.8   6.9   7.1   7.3
    ##  9:                  Bulgaria  10.2  10.4  10.5  10.5  10.6  10.6  10.7  10.8
    ## 10:              Burkina Faso   1.3   1.3   1.3   1.3   1.4   1.4   1.4   1.4
    ## 11:                    Canada  12.2  12.3  12.4  12.4  12.5  12.6  12.7  12.8
    ## 12:                     Chile   9.5   9.5   9.4   9.9   9.9   9.8   9.8   9.9
    ## 13:                     China   6.9   6.9   7.0   7.0   7.1   7.3   7.4   7.5
    ## 14:                  Colombia   6.8   6.7   7.2   7.3   7.3   7.4   7.5   7.6
    ## 15:                    Cyprus  10.7  10.9  11.2  11.3  11.3  11.5  11.6  11.8
    ## 16:                   Ecuador   7.3   7.3   7.3   7.9   7.9   7.9   8.0   8.1
    ## 17:                   Estonia  12.1  12.2  12.2  12.3  12.4  12.5  12.5  12.5
    ## 18:                  Ethiopia   1.9   2.0   2.1   2.2   2.3   2.3   2.4   2.4
    ## 19:                   Finland  12.0  12.0  12.0  12.2  12.2  12.3  12.3  12.4
    ## 20:                    France  10.4  10.6  10.7  10.7  10.8  10.9  10.9  11.0
    ## 21:                   Georgia  12.1  12.1  12.1  12.2  12.2  12.2  12.2  12.5
    ## 22:                   Germany  13.3  13.6  13.7  13.7  13.8  13.8  13.9  14.0
    ## 23:                     Ghana   6.4   6.5   6.5   6.6   6.7   6.7   6.8   6.8
    ## 24:                     Haiti   4.3   4.4   4.5   4.6   4.7   4.7   4.8   4.9
    ## 25:                   Hungary  10.9  11.1  11.3  11.5  11.7  11.9  12.0  12.2
    ## 26:                     India   4.8   4.9   5.0   5.2   5.3   5.4   5.4   5.6
    ## 27:                 Indonesia   7.4   7.9   7.1   7.1   7.4   7.4   7.6   7.6
    ## 28: Iran, Islamic Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 29:                      Iraq   5.8   5.9   6.1   6.2   6.3   6.4   6.6   6.6
    ## 30:                     Japan  11.2  11.2  11.3  11.4  11.4  11.5  11.8  12.0
    ## 31:                    Jordan   9.7   9.8   9.8   9.8   9.9   9.8   9.9   9.9
    ## 32:                Kazakhstan  11.7  11.7  11.6  11.5  11.5  11.4  11.5  11.5
    ## 33:       Korea (Republic of)    NA    NA    NA    NA    NA    NA    NA    NA
    ## 34:                    Kuwait   5.8   6.2   6.3   6.5   6.6   6.8   7.0   7.2
    ## 35:                Kyrgyzstan  10.2  10.2  10.3  10.3  10.4  10.6  10.6  10.7
    ## 36:                   Lebanon   7.5   7.6   7.8   7.8   7.9   7.9   8.0   8.1
    ## 37:                     Libya   6.4   6.6   6.8   7.0   7.1   7.3   7.3   7.3
    ## 38:                  Malaysia   7.6   8.2   8.8   9.4   9.6   9.8  10.1  10.1
    ## 39:                      Mali   1.7   1.7   1.8   1.9   1.9   2.0   2.0   2.1
    ## 40:                    Mexico   7.6   8.0   8.0   8.0   8.2   8.0   8.4   8.6
    ## 41:      Moldova, Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 42:                   Morocco   3.9   4.0   4.0   4.1   4.2   4.2   4.4   4.6
    ## 43:               Netherlands  11.6  11.8  12.0  12.0  11.9  12.0  12.0  12.0
    ## 44:               New Zealand  11.7  11.7  11.8  11.8  11.9  12.0  12.0  12.1
    ## 45:                   Nigeria   5.2   5.2   5.2   5.2   5.2   5.2   5.5   5.7
    ## 46:                    Norway  12.4  12.5  12.6  12.7  12.7  12.7  12.8  12.6
    ## 47:                  Pakistan   4.5   4.4   4.4   4.4   4.5   4.7   4.8   4.8
    ## 48:       Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 49:                      Peru   8.7   8.1   8.1   8.4   8.4   8.4   9.1   8.6
    ## 50:               Philippines   8.7   8.8   8.9   9.0   9.0   8.9   9.0   9.1
    ## 51:                    Poland  11.6  11.7  11.8  11.9  12.1  12.2  12.3  11.7
    ## 52:                     Qatar   8.8   9.2   9.6   9.7   9.2   8.4   8.7   9.2
    ## 53:                   Romania  10.1  10.3  10.5  10.6  10.6  10.7  10.8  10.9
    ## 54:        Russian Federation    NA    NA    NA    NA    NA    NA    NA    NA
    ## 55:                    Rwanda   2.8   2.9   3.1   3.2   3.3   3.8   3.8   3.7
    ## 56:                    Serbia  10.2  10.2  10.2  10.4  10.4  10.4  10.6  10.5
    ## 57:                 Singapore  10.5  10.1  10.2  10.5  10.5  11.2  11.2  11.3
    ## 58:                  Slovenia  11.7  11.8  11.8  11.9  12.1  12.1  12.2  11.7
    ## 59:              South Africa   8.9   9.0   9.1   9.7  10.1  10.0   9.9   9.8
    ## 60:                     Spain   8.9   9.0   9.1   9.2   9.3   9.4   9.5   9.5
    ## 61:                    Sweden  12.4  12.4  12.5  12.2  12.2  12.3  12.4  12.4
    ## 62:               Switzerland  12.0  12.3  12.6  12.9  13.3  13.3  13.3  13.4
    ## 63:                  Thailand   7.0   7.0   7.1   7.3   7.5   7.7   7.5   7.7
    ## 64:       Trinidad and Tobago  10.0   9.9  10.5  10.7  10.7  10.7  10.8  10.8
    ## 65:                   Tunisia   5.8   5.9   6.1   6.3   6.4   6.7   6.9   6.8
    ## 66:                    Turkey   6.0   6.1   6.2   6.3   6.5   6.7   7.2   7.5
    ## 67:                   Ukraine  11.2  11.2  11.2  11.3  11.3  11.3  11.3  11.3
    ## 68:            United Kingdom  12.2  12.4  12.6  12.8  13.1  13.2  13.0  12.9
    ## 69:             United States  12.8  12.8  12.9  13.2  13.2  13.2  13.3  13.3
    ## 70:                   Uruguay   8.0   8.0   8.2   8.4   8.4   8.4   8.4   8.5
    ## 71:                Uzbekistan   9.8   9.9  10.1  10.3  10.5  10.7  10.9  11.1
    ## 72:                  Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA
    ## 73:                     Yemen   1.9   2.0   2.2   2.3   2.5   2.6   2.8   3.0
    ## 74:                    Zambia   6.3   6.4   6.4   6.5   6.5   6.6   6.7   6.7
    ## 75:                  Zimbabwe   6.8   6.8   7.0   7.0   7.2   7.3   7.3   7.9
    ##                 location_name scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12
    ##     scl13 scl14 scl15 scl16 scl17
    ##  1:   7.8   7.9   7.9   8.0   8.0
    ##  2:  10.2  10.2  10.2  10.2  10.2
    ##  3:   9.8   9.8   9.8   9.9   9.9
    ##  4:  11.4  11.5  11.6  11.7  11.7
    ##  5:  12.6  12.7  12.8  12.9  12.9
    ##  6:  10.8  10.7  10.7  10.7  10.7
    ##  7:  12.0  12.1  12.2  12.3  12.3
    ##  8:   7.4   7.4   7.6   7.8   7.8
    ##  9:  10.9  10.9  11.8  11.8  11.8
    ## 10:   1.4   1.4   1.4   1.5   1.5
    ## 11:  12.9  13.0  13.1  13.1  13.3
    ## 12:   9.9  10.1  10.3  10.3  10.3
    ## 13:   7.5   7.6   7.7   7.8   7.8
    ## 14:   7.8   8.0   8.1   8.3   8.3
    ## 15:  12.0  11.9  11.9  12.1  12.1
    ## 16:   8.3   8.5   8.4   8.7   8.7
    ## 17:  12.6  12.6  12.7  12.6  12.7
    ## 18:   2.5   2.5   2.6   2.7   2.7
    ## 19:  12.3  12.4  12.4  12.4  12.4
    ## 20:  11.2  11.4  11.5  11.5  11.5
    ## 21:  12.6  12.6  12.7  12.8  12.8
    ## 22:  14.0  14.0  14.1  14.1  14.1
    ## 23:   6.9   6.9   6.9   7.1   7.1
    ## 24:   5.0   5.1   5.2   5.2   5.3
    ## 25:  12.0  11.8  11.8  11.9  11.9
    ## 26:   5.8   6.1   6.3   6.4   6.4
    ## 27:   7.8   7.8   7.9   8.0   8.0
    ## 28:    NA    NA    NA    NA    NA
    ## 29:   6.6   6.6   6.6   6.7   6.8
    ## 30:  12.2  12.5  12.5  12.7  12.8
    ## 31:   9.9  10.1  10.3  10.4  10.4
    ## 32:  11.6  11.7  11.7  11.7  11.8
    ## 33:    NA    NA    NA    NA    NA
    ## 34:   6.7   6.9   7.1   7.2   7.3
    ## 35:  10.7  10.8  10.8  10.9  10.9
    ## 36:   8.3   8.4   8.5   8.6   8.7
    ## 37:   7.3   7.3   7.3   7.3   7.3
    ## 38:  10.1  10.1  10.2  10.2  10.2
    ## 39:   2.2   2.3   2.3   2.3   2.3
    ## 40:   8.4   8.4   8.6   8.6   8.6
    ## 41:    NA    NA    NA    NA    NA
    ## 42:   4.8   5.0   5.0   5.4   5.5
    ## 43:  12.1  12.1  12.1  12.1  12.2
    ## 44:  12.1  12.2  12.4  12.5  12.5
    ## 45:   5.9   5.9   6.0   6.2   6.2
    ## 46:  12.7  12.5  12.5  12.6  12.6
    ## 47:   4.9   5.1   5.1   5.2   5.2
    ## 48:    NA    NA    NA    NA    NA
    ## 49:   8.8   9.4   9.1   9.2   9.2
    ## 50:   9.1   9.2   9.3   9.3   9.3
    ## 51:  12.1  11.3  12.1  12.2  12.3
    ## 52:   9.9   9.8   9.8   9.8   9.8
    ## 53:  10.9  10.9  10.9  11.0  11.0
    ## 54:    NA    NA    NA    NA    NA
    ## 55:   3.8   4.0   4.0   4.1   4.1
    ## 56:  10.4  10.7  11.0  11.1  11.1
    ## 57:  11.4  11.4  11.5  11.5  11.5
    ## 58:  11.7  11.9  12.0  12.3  12.2
    ## 59:   9.9  10.0  10.1  10.1  10.1
    ## 60:   9.5   9.7   9.7   9.8   9.8
    ## 61:  12.2  12.3  12.4  12.4  12.4
    ## 62:  13.4  13.4  13.4  13.4  13.4
    ## 63:   7.5   7.6   7.6   7.6   7.6
    ## 64:  10.8  10.8  10.8  10.9  10.9
    ## 65:   6.8   6.9   7.0   7.1   7.2
    ## 66:   7.7   7.6   7.8   8.0   8.0
    ## 67:  11.3  11.3  11.3  11.3  11.3
    ## 68:  12.6  12.7  12.8  12.9  12.9
    ## 69:  13.2  13.3  13.3  13.4  13.4
    ## 70:   8.5   8.6   8.7   8.7   8.7
    ## 71:  11.3  11.3  11.4  11.4  11.5
    ## 72:    NA    NA    NA    NA    NA
    ## 73:   3.0   3.0   3.0   3.0   3.0
    ## 74:   6.8   6.9   6.9   7.0   7.0
    ## 75:   8.0   8.2   8.2   8.2   8.2
    ##     scl13 scl14 scl15 scl16 scl17

``` r
na.omit(nascl, c("scl17"), invert = TRUE) #able to see if can replace values, can't replace with any , still 6 NAs
```

    ##                location_name scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12
    ## 1: Iran, Islamic Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:       Korea (Republic of)    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:      Moldova, Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:       Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:        Russian Federation    NA    NA    NA    NA    NA    NA    NA    NA
    ## 6:                  Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA
    ##    scl13 scl14 scl15 scl16 scl17
    ## 1:    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA    NA
    ## 6:    NA    NA    NA    NA    NA

SCL no values Iran, Islamic Republic of NA  
Korea (Republic of) NA  
Moldova, Republic of NA  
Palestine, State of NA Russian Federation NA Viet Nam NA

\#\#\#\#GDP17

``` r
na.omit(edit_phy17, c("GDP17"), invert = TRUE) # 7 missing GDP2017
```

    ##                location_name GSNI_PERIOD onebias twobias nobias political
    ## 1: Iran, Islamic Republic of   2005–2009   98.54   92.49   1.46     84.63
    ## 2:       Korea (Republic of)   2010–2014   87.07   62.91  12.93     63.68
    ## 3:                Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80
    ## 4:      Moldova, Republic of   2005–2009   90.06   67.21   9.94     60.33
    ## 5:       Palestine, State of   2010–2014   98.00   92.30   2.00     89.30
    ## 6:                  Viet Nam   2005–2009   92.89   69.17   7.11     59.40
    ## 7:                     Yemen   2010–2014   97.80   92.10   2.20     87.40
    ##    economic educational physical cvd05fem cvd06fem cvd07fem cvd08fem cvd09fem
    ## 1:    88.86       55.42    78.69 346.4549 333.0393 318.5381 305.9814 298.0127
    ## 2:    54.33       25.67    58.27 155.9141 143.8054 132.6713 120.8555 111.7862
    ## 3:    71.53       41.00    81.73 546.8872 553.7142 543.2533 536.8522 507.4175
    ## 4:    58.80       16.73    65.20 513.3270 491.9931 497.1151 484.5283 467.9101
    ## 5:    79.50       26.70    83.50 367.3750 362.0774 355.8204 350.5859 343.9703
    ## 6:    62.49       20.36    70.56 256.6274 256.6231 255.9745 255.1929 254.4622
    ## 7:    87.20       45.30    81.00 493.0600 489.2440 485.1008 483.3384 478.3407
    ##    cvd10fem cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem  cvd16fem
    ## 1: 290.3670 283.4457 278.28489 277.07263 275.18836 277.67423 278.51259
    ## 2: 105.3449 100.1279  95.58209  90.21631  85.58348  83.53081  81.91117
    ## 3: 488.1873 483.2938 465.05092 442.79747 429.36559 435.06853 415.09331
    ## 4: 459.1067 400.9103 387.36233 372.28763 377.82255 382.27748 381.40961
    ## 5: 341.7886 337.6966 320.17827 314.63758 323.39695 338.52757 350.93555
    ## 6: 252.5587 250.1793 247.05521 243.05616 239.21285 234.94030 230.61406
    ## 7: 472.1759 471.3592 468.99902 467.16132 464.47411 466.86730 466.88438
    ##     cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ## 1: 277.88105 271.04348 268.72894  400.2154  389.5281  376.5086  362.0445
    ## 2:  81.88305  82.38176  83.11824  190.0032  178.2896  167.8587  158.2836
    ## 3: 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687  761.4547
    ## 4: 368.12912 361.05127 341.11337  702.8658  671.8424  666.6718  648.2151
    ## 5: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 6: 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026  455.8597
    ## 7: 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200  566.4953
    ##    cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ## 1:  350.5229  338.1660  323.1285  311.8184  304.8604  299.1565  300.1241
    ## 2:  149.4912  143.7630  137.6913  131.7820  123.9082  117.5575  114.1778
    ## 3:  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342  644.2163
    ## 4:  638.1296  641.1604  572.9933  565.3869  534.3178  552.1560  587.3132
    ## 5:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 6:  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765  443.7173
    ## 7:  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702  536.2482
    ##    cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ## 1:  297.2979  294.9042  290.2879  288.5816  374.2741  361.9923  347.9814
    ## 2:  110.9073  110.6175  108.2689  108.8687  172.0355  160.0969  148.9444
    ## 3:  618.5638  613.3843  580.3176  569.3491  634.0089  651.1682  645.5907
    ## 4:  566.6405  523.8055  511.8000  498.5963  589.7769  564.1553  566.4516
    ## 5:  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939  415.8933
    ## 6:  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238  336.5239
    ## 7:  536.1051  541.0473  547.2489  550.6548  535.1088  530.9694  525.7088
    ##    cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ## 1:  334.1940  324.1761  313.9673  302.7896  294.4027  290.2060  286.3140
    ## 2:  137.6671  128.5412  122.2008  116.6089  111.6049  105.3768  100.1378
    ## 3:  632.7615  603.3141  581.8522  562.6225  543.9251  519.5371  513.0161
    ## 4:  552.6234  539.0321  535.4923  471.5840  460.0871  438.8196  449.0384
    ## 5:  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001  368.8565
    ## 6:  337.0268  337.1512  335.4539  333.0107  329.9312  326.6030  323.1828
    ## 7:  523.7193  517.1861  508.7890  507.4918  504.3873  501.9957  497.8142
    ##    cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ## 1: 287.96550 286.88535 285.32918 279.68642  277.7336        74.6     20.5
    ## 2:  97.49873  95.22265  95.17711  94.68896   95.4053        79.8     22.7
    ## 3: 523.15984 500.14376 493.82082 473.26712  466.3130        70.2     18.3
    ## 4: 463.27943 456.27984 432.82716 423.77893  404.9551        70.5     17.2
    ## 5: 380.88863 384.46844 388.17552 386.67955  381.6978          NA       NA
    ## 6: 319.13626 314.95461 311.23748 307.70802  303.9047        75.6     21.0
    ## 7: 500.90386 500.85723 504.82748 509.87687  513.0910        64.7     17.9
    ##    LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05  phy06
    ## 1:        78.0     22.3        78.5     22.3       79.10     22.5 0.8869 0.5361
    ## 2:        83.8     25.9        85.1     26.9       86.10     27.9 1.7529 1.8047
    ## 3:        73.3     19.3        75.3     20.2       77.30     21.7     NA     NA
    ## 4:        73.1     18.2        75.0     19.5       77.10     20.9 2.3783 2.3768
    ## 5:          NA       NA          NA       NA       76.47       NA     NA     NA
    ## 6:        77.1     21.5        77.6     21.8       78.10     22.0     NA     NA
    ## 7:        69.6     18.7        69.6     18.9       68.90     18.7     NA     NA
    ##     phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14  phy15  phy16
    ## 1:     NA     NA     NA 0.8900     NA     NA     NA 1.5044 1.1526     NA
    ## 2: 1.8655 1.8407 1.9185 1.9839 2.0361 2.0798 2.1632 2.2070 2.2494 2.3037
    ## 3:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 4: 2.3871 2.3545 2.3912 2.3808 2.4142 2.4135 2.5028 2.4746 2.4836     NA
    ## 5:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 6:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 7:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ##    phy17edit               HE05               HE06               HE07
    ## 1:    1.1292         5.30572176 5.1986260399999997         5.03977919
    ## 2:    2.3608 4.6178216900000004 4.9439678200000001 5.1149072599999998
    ## 3:        NA               <NA>               <NA>               <NA>
    ## 4:    3.2066 7.9639606499999998 8.7857027100000007 9.1826009800000001
    ## 5:        NA               <NA>               <NA>               <NA>
    ## 6:        NA               <NA>               <NA>               <NA>
    ## 7:        NA               <NA>               <NA>               <NA>
    ##                  HE08               HE09               HE10               HE11
    ## 1: 5.2816843999999996 6.5595455200000004 6.7547311800000003 6.6072506899999999
    ## 2:         5.39896727 5.7822899799999998 5.9173440900000003         6.00844383
    ## 3:               <NA>               <NA>               <NA>               <NA>
    ## 4: 9.1230525999999994        11.39545822 10.131128309999999 9.0967035299999992
    ## 5:               <NA>               <NA>               <NA>               <NA>
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ## 7:               <NA>               <NA>               <NA>               <NA>
    ##                  HE12               HE13               HE14               HE15
    ## 1: 6.6364860500000002         5.99379873 6.9135108000000001 7.7605791100000001
    ## 2:         6.13262033 6.2478942899999996 6.4743809700000003 6.6527166400000004
    ## 3:               <NA>               <NA>               <NA>               <NA>
    ## 4: 9.1396207799999996 8.6830034299999994 8.6327571899999995 8.5576353100000002
    ## 5:               <NA>               <NA>               <NA>               <NA>
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ## 7:               <NA>               <NA>               <NA>               <NA>
    ##                  HE16               HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ## 1: 8.8595066100000004 8.6596641499999993    NA    NA    NA    NA    NA    NA
    ## 2: 6.9143271400000001         7.10694933    NA    NA    NA    NA    NA    NA
    ## 3:               <NA>               <NA>  10.2  10.2  10.3  10.3  10.4  10.6
    ## 4: 7.5355224600000001 7.0132851599999997    NA    NA    NA    NA    NA    NA
    ## 5:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 6:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 7:               <NA>               <NA>   1.9   2.0   2.2   2.3   2.5   2.6
    ##    scl11 scl12 scl13 scl14 scl15 scl16 scl17 GDP05 GDP06 GDP07 GDP08 GDP09
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:  10.6  10.7  10.7  10.8  10.8  10.9  10.9    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 6:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 7:   2.8   3.0   3.0   3.0   3.0   3.0   3.0    NA    NA    NA    NA    NA
    ##    GDP10 GDP11 GDP12 GDP13 GDP14 GDP15 GDP16 GDP17 MMR05 MMR06 MMR07 MMR08
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    34    32    30    28
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    15    14    15    15
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    34    31    31    29
    ## 5:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 6:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 7:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##    MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17               LMH09
    ## 1:    25    22    19    18    17    17    17    16    16 upper middle income
    ## 2:    16    15    14    13    13    12    12    11    11         high income
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 4:    28    29    21    22    21    23    22    20    19 lower middle income
    ## 5:    NA    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 6:    NA    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ## 7:    NA    NA    NA    NA    NA    NA    NA    NA    NA                <NA>
    ##                  LMH14
    ## 1: upper middle income
    ## 2:         high income
    ## 3:                <NA>
    ## 4: lower middle income
    ## 5:                <NA>
    ## 6:                <NA>
    ## 7:                <NA>

``` r
naGDP <- edit_phy17[, c("location_name", "GDP05", "GDP06", "GDP07", "GDP08", "GDP09","GDP10", "GDP11", "GDP12", "GDP13", "GDP14", "GDP15", "GDP16", "GDP17")]
naGDP #GDP columns 09-17
```

    ##                 location_name      GDP05      GDP06      GDP07      GDP08
    ##  1:                   Algeria  3113.0953  3478.7109  3946.6645  4923.8432
    ##  2:                   Andorra 40066.2569 42675.8128 47803.6936 48718.4969
    ##  3:                 Argentina  5109.8513  5919.0120  7245.4483  9020.8731
    ##  4:                   Armenia  1643.7530  2158.1437  3139.2775  4010.8572
    ##  5:                 Australia 33999.2429 36044.9228 40960.0545 49601.6567
    ##  6:                Azerbaijan  1578.4024  2473.0818  3851.4379  5574.6038
    ##  7:                   Belarus  3125.8105  3847.4341  4735.6576  6377.3697
    ##  8:                    Brazil  4790.4371  5886.4636  7348.0308  8831.0231
    ##  9:                  Bulgaria  3899.9076  4523.0508  5885.1043  7265.7355
    ## 10:              Burkina Faso   457.9336   473.4498   535.0626   643.4046
    ## 11:                    Canada 36266.1871 40385.8700 44543.0410 46594.4510
    ## 12:                     Chile  7598.5251  9464.5502 10502.3545 10751.4797
    ## 13:                     China  1753.4178  2099.2294  2693.9701  3468.3046
    ## 14:                  Colombia  3414.4652  3741.0928  4714.0731  5472.5365
    ## 15:                    Cyprus 24959.2592 26729.3234 31244.9262 35397.3637
    ## 16:                   Ecuador  3002.1369  3328.8830  3567.8364  4249.0193
    ## 17:                   Estonia 10406.3967 12631.5681 16741.9392 18227.1195
    ## 18:                  Ethiopia   162.4327   194.6874   244.2860   326.4368
    ## 19:                   Finland 39040.2889 41188.0937 48414.8451 53554.0389
    ## 20:                    France 34760.1878 36443.6234 41508.4340 45334.1144
    ## 21:                   Georgia  1642.7609  1996.0571  2635.3539  3324.7359
    ## 22:                   Germany 34507.3688 36323.4477 41587.2129 45427.1517
    ## 23:                     Ghana   492.5442   913.3939  1081.1663  1217.0648
    ## 24:                     Haiti   766.6921   792.8259   981.1113  1076.7013
    ## 25:                   Hungary 11200.5769 11475.8227 13918.9602 15753.4733
    ## 26:                     India   714.8610   806.7533  1028.3348   998.5223
    ## 27:                 Indonesia  1263.2873  1589.8015  1860.0028  2166.8542
    ## 28: Iran, Islamic Republic of         NA         NA         NA         NA
    ## 29:                      Iraq  1855.5220  2373.2148  3182.9480  4636.6110
    ## 30:                     Japan 37217.6487 35433.9890 35275.2284 39339.2976
    ## 31:                    Jordan  2183.3962  2513.0317  2735.3831  3455.7673
    ## 32:                Kazakhstan  3771.2790  5291.5757  6771.4148  8513.5646
    ## 33:       Korea (Republic of)         NA         NA         NA         NA
    ## 34:                    Kuwait 35591.0058 42781.3665 45782.2766 55494.9510
    ## 35:                Kyrgyzstan         NA         NA         NA         NA
    ## 36:                   Lebanon  4575.1055  4626.8598  5207.7960  6111.3324
    ## 37:                     Libya  8163.0108  9336.3567 11300.1913 14382.5763
    ## 38:                  Malaysia  5587.0256  6209.1245  7243.4560  8474.5868
    ## 39:                      Mali   489.0211   523.0386   596.6902   694.2777
    ## 40:                    Mexico  8277.6713  9068.2944  9642.6806 10016.5713
    ## 41:      Moldova, Republic of         NA         NA         NA         NA
    ## 42:                   Morocco  2018.0257  2196.0122  2499.2599  2890.3601
    ## 43:               Netherlands 41979.0558 44863.3506 51733.4421 57644.4800
    ## 44:               New Zealand 27751.0655 26671.3294 32511.1267 31290.2537
    ## 45:                   Nigeria  1268.3834  1656.4248  1883.4613  2242.8719
    ## 46:                    Norway 66810.4785 74148.3201 85139.9604 96944.0956
    ## 47:                  Pakistan   748.9226   836.8605   908.0951   990.8466
    ## 48:       Palestine, State of         NA         NA         NA         NA
    ## 49:                      Peru  2729.4987  3154.3312  3606.0704  4220.6170
    ## 50:               Philippines  1244.3490  1452.4387  1744.6403  1991.2315
    ## 51:                    Poland  8021.5057  9035.4105 11254.5174 13996.0252
    ## 52:                     Qatar 51455.5942 59530.1535 65421.7528 80234.4701
    ## 53:                   Romania  4617.9290  5757.4964  8360.1663 10435.0440
    ## 54:        Russian Federation  5323.4631  6920.1891  9101.2550 11635.2729
    ## 55:                    Rwanda   331.8112   367.0325   438.8336   543.7651
    ## 56:                    Serbia  3720.4792  4382.6173  5848.4764  7101.0401
    ## 57:                 Singapore 29961.2633 33769.1542 39432.9383 40007.4693
    ## 58:                  Slovenia 18098.9085 19672.9656 23787.6466 27483.3372
    ## 59:              South Africa  5383.6565  5602.0110  6095.6224  5760.8053
    ## 60:                     Spain 26419.2969 28365.3135 32549.9710 35366.2596
    ## 61:                    Sweden 43437.0631 46593.6022 53700.0053 56152.5523
    ## 62:               Switzerland 54952.6738 57579.5020 63555.2375 72487.8459
    ## 63:                  Thailand  2894.0627  3369.5434  3973.0170  4379.6585
    ## 64:       Trinidad and Tobago 12327.2332 14102.4958 16539.8781 21204.1050
    ## 65:                   Tunisia  3193.2066  3370.0339  3775.7500  4307.1559
    ## 66:                    Turkey  7456.3877  8102.1215  9791.6516 10940.9912
    ## 67:                   Ukraine  1826.9314  2300.7697  3065.6113  3887.2423
    ## 68:            United Kingdom 42030.2866 44599.6976 50566.8266 47286.9985
    ## 69:             United States 44114.7478 46298.7314 47975.9677 48382.5584
    ## 70:                   Uruguay  5226.9378  5887.8487  7026.5115  9091.0790
    ## 71:                Uzbekistan   546.7769   654.2838   830.4077  1082.2860
    ## 72:                  Viet Nam         NA         NA         NA         NA
    ## 73:                     Yemen         NA         NA         NA         NA
    ## 74:                    Zambia   702.7409  1047.9192  1124.2906  1394.0006
    ## 75:                  Zimbabwe   476.5553   447.8549   431.7872   356.6933
    ##                 location_name      GDP05      GDP06      GDP07      GDP08
    ##          GDP09      GDP10       GDP11       GDP12       GDP13      GDP14
    ##  1:  3883.1324  4479.3417   5462.2609   5591.2124   5498.7841  5494.3523
    ##  2: 43503.1855 40852.6668  43335.3289  38686.4613  39538.7667 41303.9294
    ##  3:  8225.1372 10385.9644  12848.8642  13082.6643  13080.2547 12334.7982
    ##  4:  2994.3425  3218.3727   3525.8047   3681.8575   3838.1858  3986.2316
    ##  5: 42772.3592 52022.1256  62517.8337  68012.1479  68150.1070 62510.7912
    ##  6:  4950.2948  5842.8058   7189.6912   7496.2946   7875.7570  7891.3131
    ##  7:  5351.3554  6029.3968   6519.2302   6940.1593   7978.8726  8318.5127
    ##  8:  8597.9154 11286.2430  13245.6125  12370.0242  12300.3249 12112.5882
    ##  9:  6988.2333  6812.4063   7809.4251   7395.8498   7655.1297  7876.8665
    ## 10:   624.1752   647.8358    751.1730    758.0007    787.4702   792.8468
    ## 11: 40773.0615 47448.0132  52087.4464  52678.3901  52652.5937 50893.4467
    ## 12: 10208.9068 12808.0346  14637.2402  15351.5513  15842.9408 14670.9968
    ## 13:  3832.2364  4550.4531   5618.1323   6316.9183   7050.6463  7678.5995
    ## 14:  5193.2415  6336.7095   7335.1669   8050.2554   8218.3478  8114.3439
    ## 15: 32109.2425 31023.6383  32396.3857  28912.1569  27729.1927 27129.6261
    ## 16:  4231.6158  4633.5904   5200.5558   5682.0450   6056.3308  6377.0915
    ## 17: 14794.9711 14790.8209  17621.5480  17534.4213  19174.1003 20367.1032
    ## 18:   380.5690   341.5541    354.4796    467.0779    499.5316   566.9265
    ## 19: 47293.9928 46459.9733  51081.9977  47710.7902  49878.0432 50260.2999
    ## 20: 41575.4187 40638.3340  43790.7320  40874.7035  42592.9341 43011.2631
    ## 21:  2822.6674  3233.2959   4021.7433   4421.8182   4623.7457  4739.1883
    ## 22: 41485.9016 41531.9342  46644.7760  43858.3631  46285.7641 47959.9933
    ## 23:  1077.6622  1299.3449   1549.4629   1587.5612   2345.3929  1971.0333
    ## 24:  1150.2111  1172.0985   1287.9541   1337.3354   1393.9560  1402.1002
    ## 25: 13046.4810 13191.6213  14216.1656  12950.6865  13687.5141 14267.0122
    ## 26:  1101.9608  1357.5637   1458.1035   1443.8795   1449.6059  1573.8815
    ## 27:  2261.2472  3122.3628   3643.0439   3694.3489   3623.9116  3491.6248
    ## 28:         NA         NA          NA          NA          NA         NA
    ## 29:  3853.9409  4655.4250   6036.3962   6829.9640   7076.8772  6818.8046
    ## 30: 40855.1756 44507.6764  48167.9973  48603.4766  40454.4475 38109.4121
    ## 31:  3559.6911  3736.6465   3852.7528   3909.9076   4043.7490  4130.8791
    ## 32:  7165.2232  9070.4883  11634.0019  12386.7000  13890.6318 12807.2607
    ## 33:         NA         NA          NA          NA          NA         NA
    ## 34: 37561.6727 38577.4983  48631.6913  51979.1052  49388.1374 44062.3170
    ## 35:         NA         NA          NA          NA          NA         NA
    ## 36:  7354.9536  7761.6462   7674.8354   7950.6954   7931.0805  7686.2560
    ## 37: 10275.2666 12064.7807   5554.1792  13025.2814  10363.7895  6466.9103
    ## 38:  7292.4944  9040.5663  10399.3728  10817.4429  10970.1233 11319.0798
    ## 39:   698.8989   710.2742    837.6034    778.6193    805.0328   848.2741
    ## 40:  8002.9721  9271.3982  10203.4209  10241.7279  10725.1833 10928.9168
    ## 41:         NA         NA          NA          NA          NA         NA
    ## 42:  2866.9242  2839.9252   3046.9491   2912.6583   3121.6812  3171.6992
    ## 43: 52514.0271 50950.0343  54159.3466  50073.0057  52184.0619 52830.1742
    ## 44: 28205.7328 33700.1260  38437.5432  39982.7543  42962.9882 44553.2822
    ## 45:  1891.3354  2280.4374   2487.5982   2723.8228   2961.5503  3098.9863
    ## 46: 79977.6971 87693.7901 100600.5624 101524.1419 102913.4508 97019.1828
    ## 47:   957.9957   987.4097   1164.9761   1198.1090   1208.9043  1251.1641
    ## 48:         NA         NA          NA          NA          NA         NA
    ## 49:  4196.3128  5082.3548   5869.3231   6528.9722   6756.7528  6672.8803
    ## 50:  1905.8947  2217.4740   2450.7337   2694.3055   2871.4309  2959.6485
    ## 51: 11526.0559 12613.0110  13879.5610  13097.2708  13696.4663 14271.3059
    ## 52: 59094.4449 67403.1603  82409.5773  85076.1415  85050.8663 83858.4769
    ## 53:  8548.1187  8214.0769   9099.2175   8507.1048   9547.8522 10043.6774
    ## 54:  8562.8133 10674.9958  14311.0843  15420.8745  15974.6446 14095.6487
    ## 55:   579.9390   610.0124    668.8690    725.6277    723.2583   743.9948
    ## 56:  6169.1142  5735.4229   6809.1598   6015.9452   6755.0737  6600.0568
    ## 57: 38927.2069 47236.9602  53890.4287  55546.4885  56967.4258 57562.5308
    ## 58: 24694.2305 23509.5431  25095.1323  22643.1003  23496.6025 24214.9221
    ## 59:  5862.7973  7328.6156   8007.4128   7501.4700   6832.4569  6433.1873
    ## 60: 32042.4741 30502.7197  31636.4463  28324.4293  29059.5480 29461.5503
    ## 61: 46946.9603 52869.0443  60755.7596  58037.8213  61126.9432 60020.3605
    ## 62: 69927.4688 74605.7211  88415.6280  83538.2300  85112.4644 86605.5634
    ## 63:  4213.0063  5076.3402   5492.1213   5860.5825   6168.2630  5951.8837
    ## 64: 14514.1417 16683.3554  19034.1492  19157.4170  20143.6644 20270.8594
    ## 65:  4128.4628  4141.9764   4264.6749   4152.6786   4222.7032  4305.4742
    ## 66:  9103.7099 10742.4301  11420.7733  11795.3167  12614.4803 12157.3380
    ## 67:  2542.9954  2965.1397   3569.7581   3855.4177   4029.7113  3104.6432
    ## 68: 38713.1374 39435.8399  42038.5723  42462.7716  43444.5330 47425.6077
    ## 69: 47099.9805 48467.5158  49886.8181  51610.6053  53117.6678 55064.7445
    ## 70:  9451.9324 11992.0166  14236.6812  15171.5847  16973.6742 16831.9729
    ## 71:  1213.2653  1634.3121   1926.2930   2137.0251   2281.4110  2492.3366
    ## 72:         NA         NA          NA          NA          NA         NA
    ## 73:         NA         NA          NA          NA          NA         NA
    ## 74:  1159.9078  1489.4593   1672.9083   1763.0727   1878.9097  1763.0626
    ## 75:   771.5988   948.3319   1093.6540   1304.9698   1430.0008  1434.8993
    ##          GDP09      GDP10       GDP11       GDP12       GDP13      GDP14
    ##          GDP15      GDP16      GDP17
    ##  1:  4187.5097  3945.4821  4111.2941
    ##  2: 35762.5231 37474.6654 38962.8804
    ##  3: 13789.0604 12790.2425 14613.0418
    ##  4:  3607.2967  3591.8293  3914.5013
    ##  5: 56755.7217 49971.1315 54027.9668
    ##  6:  5500.3104  3880.7387  4147.0897
    ##  7:  5949.1063  5022.6266  5761.7471
    ##  8:  8814.0010  8710.0967  9925.3862
    ##  9:  7055.9357  7548.8550  8334.0817
    ## 10:   653.3270   688.2497   734.9944
    ## 11: 43585.5120 42322.4848 45148.5527
    ## 12: 13574.1718 13753.5944 14999.3701
    ## 13:  8066.9426  8147.9377  8879.4387
    ## 14:  6175.8760  5870.7780  6376.7067
    ## 15: 23333.7149 24532.5191 26338.6943
    ## 16:  6124.4916  6060.0933  6213.5013
    ## 17: 17522.2302 18437.2528 20458.4607
    ## 18:   640.5419   717.1246   768.5223
    ## 19: 42784.6984 43784.2840 46336.6633
    ## 20: 36638.1849 37037.3742 38812.1610
    ## 21:  4014.1859  4062.1699  4357.0009
    ## 22: 41086.7297 42107.5173 44552.8194
    ## 23:  1743.8510  1931.3895  2025.9324
    ## 24:  1389.1195  1265.9876  1294.2397
    ## 25: 12706.8912 13090.5067 14605.8543
    ## 26:  1605.6054  1732.5643  1981.6510
    ## 27:  3331.6951  3562.8458  3837.6517
    ## 28:         NA         NA         NA
    ## 29:  4989.8031  4777.1976  5205.2883
    ## 30: 34524.4699 38761.8182 38386.5111
    ## 31:  4164.1079  4176.5889  4234.4031
    ## 32: 10510.7719  7714.8418  9247.5813
    ## 33:         NA         NA         NA
    ## 34: 29869.5294 27653.0668 29759.4365
    ## 35:         NA         NA         NA
    ## 36:  7644.5487  7629.8911  7801.1787
    ## 37:  4337.9191  4035.1943  5756.6984
    ## 38:  9955.2437  9817.7385 10259.1818
    ## 39:   751.4748   780.7186   830.0184
    ## 40:  9616.6450  8744.5158  9287.8497
    ## 41:         NA         NA         NA
    ## 42:  2875.2580  2896.7200  3036.3253
    ## 43: 45175.2319 46007.8529 48675.2223
    ## 44: 38615.9952 40105.6134 42849.4263
    ## 45:  2687.4801  2176.0022  1968.5647
    ## 46: 74355.5159 70459.1825 75496.7541
    ## 47:  1356.6678  1368.4543  1464.9933
    ## 48:         NA         NA         NA
    ## 49:  6229.1017  6204.9973  6710.5080
    ## 50:  3001.0404  3073.6536  3123.2342
    ## 51: 12578.4955 12447.4396 13864.6818
    ## 52: 63039.0635 57163.0757 59124.9324
    ## 53:  8969.1489  9548.5874 10807.7954
    ## 54:  9313.0136  8704.8984 10720.3326
    ## 55:   751.6394   745.3428   772.3185
    ## 56:  5588.9807  5765.2008  6292.5436
    ## 57: 55646.6187 56828.2953 60913.7453
    ## 58: 20881.7669 21663.6434 23512.8173
    ## 59:  5734.6336  5272.9184  6132.4798
    ## 60: 25732.0184 26505.3432 28170.1679
    ## 61: 51545.4836 51965.1572 53791.5087
    ## 62: 82081.5972 80172.2321 80449.9945
    ## 63:  5840.0465  5994.2315  6592.9149
    ## 64: 18289.7043 16176.9474 16238.1932
    ## 65:  3861.6885  3697.9308  3481.2287
    ## 66: 11006.2497 10895.3187 10591.4744
    ## 67:  2124.6623  2187.7305  2640.6757
    ## 68: 44974.8319 41064.1334 40361.4174
    ## 69: 56839.3818 57951.5841 60062.2223
    ## 70: 15613.7643 15387.1440 17322.1474
    ## 71:  2615.0251  2567.7992  1826.5669
    ## 72:         NA         NA         NA
    ## 73:         NA         NA         NA
    ## 74:  1337.7956  1280.5784  1534.8668
    ## 75:  1445.0711  1464.5835  1548.1701
    ##          GDP15      GDP16      GDP17

``` r
na.omit(naGDP, c("GDP17"), invert = TRUE) #able to see if can replace values, can't replace with any , still 7 NAs
```

    ##                location_name GDP05 GDP06 GDP07 GDP08 GDP09 GDP10 GDP11 GDP12
    ## 1: Iran, Islamic Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:       Korea (Republic of)    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:                Kyrgyzstan    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:      Moldova, Republic of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:       Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 6:                  Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA
    ## 7:                     Yemen    NA    NA    NA    NA    NA    NA    NA    NA
    ##    GDP13 GDP14 GDP15 GDP16 GDP17
    ## 1:    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA    NA
    ## 6:    NA    NA    NA    NA    NA
    ## 7:    NA    NA    NA    NA    NA

GDP (no values) Iran, Islamic Republic of NA  
Korea (Republic of) NA  
Kyrgyzstan NA  
Moldova, Republic of NA  
Palestine, State of NA  
Viet Nam NA  
Yemen NA

\#\#\#\#MMR17

``` r
na.omit(edit_phy17, c("MMR17"), invert = TRUE) # 5 missing MMR2017
```

    ##          location_name GSNI_PERIOD onebias twobias nobias political economic
    ## 1:             Andorra   2005–2009   27.01    7.43  72.99     14.08     8.73
    ## 2:          Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80    71.53
    ## 3: Palestine, State of   2010–2014   98.00   92.30   2.00     89.30    79.50
    ## 4:            Viet Nam   2005–2009   92.89   69.17   7.11     59.40    62.49
    ## 5:               Yemen   2010–2014   97.80   92.10   2.20     87.40    87.20
    ##    educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem  cvd09fem
    ## 1:        1.81    12.01  94.99074  94.10549  92.00298  91.82684  93.03548
    ## 2:       41.00    81.73 546.88719 553.71417 543.25326 536.85221 507.41753
    ## 3:       26.70    83.50 367.37503 362.07740 355.82036 350.58586 343.97033
    ## 4:       20.36    70.56 256.62741 256.62311 255.97453 255.19294 254.46217
    ## 5:       45.30    81.00 493.05999 489.24403 485.10075 483.33838 478.34072
    ##     cvd10fem  cvd11fem  cvd12fem  cvd13fem cvd14fem  cvd15fem  cvd16fem
    ## 1:  93.64847  94.72272  94.77537  95.38794  95.7992  96.76678  96.28366
    ## 2: 488.18726 483.29378 465.05092 442.79747 429.3656 435.06853 415.09331
    ## 3: 341.78855 337.69661 320.17827 314.63758 323.3969 338.52757 350.93555
    ## 4: 252.55867 250.17926 247.05521 243.05616 239.2128 234.94030 230.61406
    ## 5: 472.17590 471.35922 468.99902 467.16132 464.4741 466.86730 466.88438
    ##     cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male cvd08male
    ## 1:  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052  126.3548
    ## 2: 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687  761.4547
    ## 3: 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858  484.7670
    ## 4: 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026  455.8597
    ## 5: 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200  566.4953
    ##    cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male cvd15male
    ## 1:  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398  119.3653
    ## 2:  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342  644.2163
    ## 3:  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364  442.4321
    ## 4:  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765  443.7173
    ## 5:  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702  536.2482
    ##    cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both cvd07both
    ## 1:  118.6682  117.7883  116.9475  115.8910  114.4628  112.7694  110.3326
    ## 2:  618.5638  613.3843  580.3176  569.3491  634.0089  651.1682  645.5907
    ## 3:  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939  415.8933
    ## 4:  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238  336.5239
    ## 5:  536.1051  541.0473  547.2489  550.6548  535.1088  530.9694  525.7088
    ##    cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both cvd14both
    ## 1:  109.3167  109.3833  109.1886  109.4555  108.8728  108.7915  108.3474
    ## 2:  632.7615  603.3141  581.8522  562.6225  543.9251  519.5371  513.0161
    ## 3:  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001  368.8565
    ## 4:  337.0268  337.1512  335.4539  333.0107  329.9312  326.6030  323.1828
    ## 5:  523.7193  517.1861  508.7890  507.4918  504.3873  501.9957  497.8142
    ##    cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000 LE602000
    ## 1:  108.1292  107.5617  106.9530  106.3957  105.7119          NA       NA
    ## 2:  523.1598  500.1438  493.8208  473.2671  466.3130        70.2     18.3
    ## 3:  380.8886  384.4684  388.1755  386.6795  381.6978          NA       NA
    ## 4:  319.1363  314.9546  311.2375  307.7080  303.9047        75.6     21.0
    ## 5:  500.9039  500.8572  504.8275  509.8769  513.0910        64.7     17.9
    ##    LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019  phy05  phy06
    ## 1:          NA       NA          NA       NA       84.95       NA 3.2319 3.0123
    ## 2:        73.3     19.3        75.3     20.2       77.30     21.7     NA     NA
    ## 3:          NA       NA          NA       NA       76.47       NA     NA     NA
    ## 4:        77.1     21.5        77.6     21.8       78.10     22.0     NA     NA
    ## 5:        69.6     18.7        69.6     18.9       68.90     18.7     NA     NA
    ##     phy07 phy08  phy09 phy10 phy11 phy12 phy13 phy14  phy15 phy16 phy17edit
    ## 1: 3.0109    NA 3.1479     4    NA    NA    NA    NA 3.3333    NA    3.3333
    ## 2:     NA    NA     NA    NA    NA    NA    NA    NA     NA    NA        NA
    ## 3:     NA    NA     NA    NA    NA    NA    NA    NA     NA    NA        NA
    ## 4:     NA    NA     NA    NA    NA    NA    NA    NA     NA    NA        NA
    ## 5:     NA    NA     NA    NA    NA    NA    NA    NA     NA    NA        NA
    ##                  HE05               HE06               HE07               HE08
    ## 1: 5.5754260999999996 4.9350943599999999 4.9255475999999998 5.8059859300000003
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3:               <NA>               <NA>               <NA>               <NA>
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5:               <NA>               <NA>               <NA>               <NA>
    ##                  HE09       HE10               HE11               HE12
    ## 1: 6.2023372700000001 6.64963865 6.2465286300000002 6.1015033699999996
    ## 2:               <NA>       <NA>               <NA>               <NA>
    ## 3:               <NA>       <NA>               <NA>               <NA>
    ## 4:               <NA>       <NA>               <NA>               <NA>
    ## 5:               <NA>       <NA>               <NA>               <NA>
    ##                  HE13               HE14               HE15               HE16
    ## 1: 5.9878034600000003 5.9791245499999999 6.2324533500000001 6.3434934600000004
    ## 2:               <NA>               <NA>               <NA>               <NA>
    ## 3:               <NA>               <NA>               <NA>               <NA>
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5:               <NA>               <NA>               <NA>               <NA>
    ##                  HE17 scl05 scl06 scl07 scl08 scl09 scl10 scl11 scl12 scl13
    ## 1: 6.5443186799999999   9.8  10.1  10.1  10.1  10.1  10.1  10.2  10.2  10.2
    ## 2:               <NA>  10.2  10.2  10.3  10.3  10.4  10.6  10.6  10.7  10.7
    ## 3:               <NA>    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:               <NA>    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:               <NA>   1.9   2.0   2.2   2.3   2.5   2.6   2.8   3.0   3.0
    ##    scl14 scl15 scl16 scl17    GDP05    GDP06    GDP07   GDP08    GDP09    GDP10
    ## 1:  10.2  10.2  10.2  10.2 40066.26 42675.81 47803.69 48718.5 43503.19 40852.67
    ## 2:  10.8  10.8  10.9  10.9       NA       NA       NA      NA       NA       NA
    ## 3:    NA    NA    NA    NA       NA       NA       NA      NA       NA       NA
    ## 4:    NA    NA    NA    NA       NA       NA       NA      NA       NA       NA
    ## 5:   3.0   3.0   3.0   3.0       NA       NA       NA      NA       NA       NA
    ##       GDP11    GDP12    GDP13    GDP14    GDP15    GDP16    GDP17 MMR05 MMR06
    ## 1: 43335.33 38686.46 39538.77 41303.93 35762.52 37474.67 38962.88    NA    NA
    ## 2:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ## 3:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ## 4:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ## 5:       NA       NA       NA       NA       NA       NA       NA    NA    NA
    ##    MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13 MMR14 MMR15 MMR16 MMR17
    ## 1:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##          LMH09       LMH14
    ## 1: high income high income
    ## 2:        <NA>        <NA>
    ## 3:        <NA>        <NA>
    ## 4:        <NA>        <NA>
    ## 5:        <NA>        <NA>

``` r
naMMR <- edit_phy17[, c("location_name", "MMR05", "MMR06", "MMR07", "MMR08", "MMR09","MMR10", "MMR11", "MMR12", "MMR13", "MMR14", "MMR15", "MMR16", "MMR17")]
naMMR #MMR columns 09-17
```

    ##                 location_name MMR05 MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12
    ##  1:                   Algeria   127   122   119   117   117   115   116   116
    ##  2:                   Andorra    NA    NA    NA    NA    NA    NA    NA    NA
    ##  3:                 Argentina    59    57    56    53    56    51    48    47
    ##  4:                   Armenia    35    36    32    36    32    32    30    30
    ##  5:                 Australia     5     5     5     5     5     5     6     6
    ##  6:                Azerbaijan    42    35    34    32    32    31    30    29
    ##  7:                   Belarus    11     9     7     6     6     5     5     4
    ##  8:                    Brazil    71    72    71    70    69    65    61    60
    ##  9:                  Bulgaria    15    14    13    13    12    12    12    11
    ## 10:              Burkina Faso   437   422   410   401   393   385   377   369
    ## 11:                    Canada    11    11    11    12    12    11    11    11
    ## 12:                     Chile    25    25    23    21    21    20    18    17
    ## 13:                     China    44    42    40    40    37    36    34    33
    ## 14:                  Colombia    83    82    83    84    87    85    84    85
    ## 15:                    Cyprus    12     9     8     9     8     8     8     7
    ## 16:                   Ecuador    94    90    85    82    80    78    76    71
    ## 17:                   Estonia    18    16    17    14    14    11    13    12
    ## 18:                  Ethiopia   865   795   731   681   638   597   558   527
    ## 19:                   Finland     5     4     4     4     4     4     4     4
    ## 20:                    France     9     9     9     9     9     9     9     9
    ## 21:                   Georgia    39    33    36    39    43    32    32    30
    ## 22:                   Germany     6     6     6     6     6     6     6     5
    ## 23:                     Ghana   371   359   349   342   339   339   339   336
    ## 24:                     Haiti   459   467   473   484   484   506   496   500
    ## 25:                   Hungary    15    14    15    14    13    13    13    12
    ## 26:                     India   286   270   255   240   225   210   197   185
    ## 27:                 Indonesia   252   249   243   239   234   228   221   214
    ## 28: Iran, Islamic Republic of    34    32    30    28    25    22    19    18
    ## 29:                      Iraq   127   158   138    90    75    70    67    66
    ## 30:                     Japan     7     7     6     6     6     6     6     5
    ## 31:                    Jordan    62    58    56    55    54    53    52    52
    ## 32:                Kazakhstan    43    40    36    30    24    22    19    17
    ## 33:       Korea (Republic of)    15    14    15    15    16    15    14    13
    ## 34:                    Kuwait    10    10    10    10    10    10    10    11
    ## 35:                Kyrgyzstan    NA    NA    NA    NA    NA    NA    NA    NA
    ## 36:                   Lebanon    24    24    23    23    23    23    25    25
    ## 37:                     Libya    57    55    53    52    53    53    56    55
    ## 38:                  Malaysia    31    30    30    29    29    30    30    30
    ## 39:                      Mali   691   675   663   661   661   660   663   663
    ## 40:                    Mexico    54    51    49    49    51    46    43    41
    ## 41:      Moldova, Republic of    34    31    31    29    28    29    21    22
    ## 42:                   Morocco   131   121   113   106    99    92    86    81
    ## 43:               Netherlands    11     9     9     8     8     7     7     7
    ## 44:               New Zealand    11    11    11    10    11    11    10    10
    ## 45:                   Nigeria  1080  1040  1010   996   987   978   972   963
    ## 46:                    Norway     5     4     4     4     4     4     4     3
    ## 47:                  Pakistan   237   222   214   205   199   191   180   173
    ## 48:       Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA
    ## 49:                      Peru   118   114   112   108   106   104   102   100
    ## 50:               Philippines   156   156   149   148   149   144   141   139
    ## 51:                    Poland     4     4     4     4     3     3     3     3
    ## 52:                     Qatar    12    11    11    11    10    10    10    10
    ## 53:                   Romania    35    32    30    28    28    27    24    23
    ## 54:        Russian Federation    42    36    32    30    27    25    23    22
    ## 55:                    Rwanda   643   541   469   427   424   373   349   329
    ## 56:                    Serbia    12    12    12    12    12    12    12    12
    ## 57:                 Singapore    13    13    12    11    10    10    10    10
    ## 58:                  Slovenia    10     8     8     8     8     8     8     7
    ## 59:              South Africa   201   201   199   191   179   171   161   143
    ## 60:                     Spain     5     4     4     4     4     4     4     4
    ## 61:                    Sweden     5     5     5     5     5     4     5     5
    ## 62:               Switzerland     7     7     6     6     6     6     6     6
    ## 63:                  Thailand    43    42    42    43    43    42    41    39
    ## 64:       Trinidad and Tobago    76    73    65    72    74    71    72    71
    ## 65:                   Tunisia    51    49    47    47    46    46    46    46
    ## 66:                    Turkey    33    30    28    27    25    24    23    22
    ## 67:                   Ukraine    33    31    33    33    27    25    23    24
    ## 68:            United Kingdom    11    11    11    11    10    10     9     8
    ## 69:             United States    13    14    14    14    15    15    15    16
    ## 70:                   Uruguay    22    20    20    19    19    17    17    17
    ## 71:                Uzbekistan    38    37    35    34    32    31    32    32
    ## 72:                  Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA
    ## 73:                     Yemen    NA    NA    NA    NA    NA    NA    NA    NA
    ## 74:                    Zambia   421   406   387   356   329   305   283   267
    ## 75:                  Zimbabwe   685   680   671   657   632   598   557   528
    ##                 location_name MMR05 MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12
    ##     MMR13 MMR14 MMR15 MMR16 MMR17
    ##  1:   115   114   114   113   112
    ##  2:    NA    NA    NA    NA    NA
    ##  3:    44    42    41    40    39
    ##  4:    26    27    28    26    26
    ##  5:     6     6     6     6     6
    ##  6:    28    28    27    26    26
    ##  7:     3     3     3     3     2
    ##  8:    61    62    63    62    60
    ##  9:    10    11    10    10    10
    ## 10:   362   353   343   331   320
    ## 11:    11    11    11    10    10
    ## 12:    16    15    14    13    13
    ## 13:    32    31    30    29    29
    ## 14:    85    85    85    84    83
    ## 15:     6     8     7     6     6
    ## 16:    67    65    63    61    59
    ## 17:    10    11    10    11     9
    ## 18:   498   472   446   422   401
    ## 19:     4     3     3     3     3
    ## 20:     9     8     8     8     8
    ## 21:    30    29    27    26    25
    ## 22:     5     5     5     5     7
    ## 23:   331   325   320   314   308
    ## 24:   496   492   488   489   480
    ## 25:    12    12    12    12    12
    ## 26:   175   166   158   150   145
    ## 27:   207   199   192   184   177
    ## 28:    17    17    17    16    16
    ## 29:    75    92    83    78    79
    ## 30:     5     5     5     5     5
    ## 31:    51    49    48    47    46
    ## 32:    14    13    12    10    10
    ## 33:    13    12    12    11    11
    ## 34:    11    11    11    12    12
    ## 35:    NA    NA    NA    NA    NA
    ## 36:    27    28    29    29    29
    ## 37:    58    63    70    73    72
    ## 38:    30    30    30    29    29
    ## 39:   663   642   620   590   562
    ## 40:    39    38    36    34    33
    ## 41:    21    23    22    20    19
    ## 42:    78    76    74    72    70
    ## 43:     6     6     6     6     5
    ## 44:     9    10    10    10     9
    ## 45:   951   943   931   925   917
    ## 46:     3     3     3     3     2
    ## 47:   166   161   154   143   140
    ## 48:    NA    NA    NA    NA    NA
    ## 49:    98    96    94    91    88
    ## 50:   136   131   127   124   121
    ## 51:     3     2     2     2     2
    ## 52:    10    10     9     9     9
    ## 53:    21    21    21    21    19
    ## 54:    20    19    18    18    17
    ## 55:   308   291   275   260   248
    ## 56:    12    12    13    12    12
    ## 57:     9     8     9     8     8
    ## 58:     7     7     7     7     7
    ## 59:   133   128   125   122   119
    ## 60:     4     4     4     4     4
    ## 61:     5     5     4     4     4
    ## 62:     6     5     5     5     5
    ## 63:    39    38    38    37    37
    ## 64:    70    69    68    68    67
    ## 65:    46    46    46    45    43
    ## 66:    20    19    19    18    17
    ## 67:    23    24    21    20    19
    ## 68:     8     8     8     7     7
    ## 69:    16    16    18    19    19
    ## 70:    18    17    18    18    17
    ## 71:    31    30    30    29    29
    ## 72:    NA    NA    NA    NA    NA
    ## 73:    NA    NA    NA    NA    NA
    ## 74:   254   242   232   222   213
    ## 75:   509   494   480   468   458
    ##     MMR13 MMR14 MMR15 MMR16 MMR17

``` r
na.omit(naMMR, c("MMR17"), invert = TRUE) #able to see if can replace values, can't replace any
```

    ##          location_name MMR05 MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13
    ## 1:             Andorra    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:          Kyrgyzstan    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 3: Palestine, State of    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 4:            Viet Nam    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:               Yemen    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##    MMR14 MMR15 MMR16 MMR17
    ## 1:    NA    NA    NA    NA
    ## 2:    NA    NA    NA    NA
    ## 3:    NA    NA    NA    NA
    ## 4:    NA    NA    NA    NA
    ## 5:    NA    NA    NA    NA

MMR NAs (no values) Andorra NA  
Kyrgyzstan NA  
Palestine, State of NA Viet Nam NA Yemen NA

``` r
na.omit(edit_phy17, c("cvd17fem", "cvd17both", "LEbirth2019", "phy17edit", "scl17", "GDP17", "MMR17"), invert = TRUE) #28 rows with one missing value for 2017
```

    ##                location_name GSNI_PERIOD onebias twobias nobias political
    ## 1:                   Andorra   2005–2009   27.01    7.43  72.99     14.08
    ## 2: Iran, Islamic Republic of   2005–2009   98.54   92.49   1.46     84.63
    ## 3:       Korea (Republic of)   2010–2014   87.07   62.91  12.93     63.68
    ## 4:                Kyrgyzstan   2010–2014   96.73   84.87   3.27     76.80
    ## 5:      Moldova, Republic of   2005–2009   90.06   67.21   9.94     60.33
    ## 6:       Palestine, State of   2010–2014   98.00   92.30   2.00     89.30
    ## 7:        Russian Federation   2010–2014   86.83   68.56  13.17     68.43
    ## 8:                  Viet Nam   2005–2009   92.89   69.17   7.11     59.40
    ## 9:                     Yemen   2010–2014   97.80   92.10   2.20     87.40
    ##    economic educational physical  cvd05fem  cvd06fem  cvd07fem  cvd08fem
    ## 1:     8.73        1.81    12.01  94.99074  94.10549  92.00298  91.82684
    ## 2:    88.86       55.42    78.69 346.45487 333.03926 318.53806 305.98136
    ## 3:    54.33       25.67    58.27 155.91408 143.80539 132.67130 120.85549
    ## 4:    71.53       41.00    81.73 546.88719 553.71417 543.25326 536.85221
    ## 5:    58.80       16.73    65.20 513.32695 491.99307 497.11512 484.52829
    ## 6:    79.50       26.70    83.50 367.37503 362.07740 355.82036 350.58586
    ## 7:    58.77       22.66    50.02 569.51494 527.82419 500.58817 492.70377
    ## 8:    62.49       20.36    70.56 256.62741 256.62311 255.97453 255.19294
    ## 9:    87.20       45.30    81.00 493.05999 489.24403 485.10075 483.33838
    ##     cvd09fem  cvd10fem  cvd11fem  cvd12fem  cvd13fem  cvd14fem  cvd15fem
    ## 1:  93.03548  93.64847  94.72272  94.77537  95.38794  95.79920  96.76678
    ## 2: 298.01274 290.36702 283.44570 278.28489 277.07263 275.18836 277.67423
    ## 3: 111.78620 105.34493 100.12794  95.58209  90.21631  85.58348  83.53081
    ## 4: 507.41753 488.18726 483.29378 465.05092 442.79747 429.36559 435.06853
    ## 5: 467.91012 459.10667 400.91032 387.36233 372.28763 377.82255 382.27748
    ## 6: 343.97033 341.78855 337.69661 320.17827 314.63758 323.39695 338.52757
    ## 7: 467.54550 459.66489 424.43583 406.44126 391.62857 386.28781 376.79609
    ## 8: 254.46217 252.55867 250.17926 247.05521 243.05616 239.21285 234.94030
    ## 9: 478.34072 472.17590 471.35922 468.99902 467.16132 464.47411 466.86730
    ##     cvd16fem  cvd17fem  cvd18fem  cvd19fem cvd05male cvd06male cvd07male
    ## 1:  96.28366  95.83302  95.48617  95.05596  133.5110  131.0153  128.3052
    ## 2: 278.51259 277.88105 271.04348 268.72894  400.2154  389.5281  376.5086
    ## 3:  81.91117  81.88305  82.38176  83.11824  190.0032  178.2896  167.8587
    ## 4: 415.09331 408.28525 395.20011 390.57369  748.0504  780.9492  781.9687
    ## 5: 381.40961 368.12912 361.05127 341.11337  702.8658  671.8424  666.6718
    ## 6: 350.93555 354.15503 349.68993 345.66686  524.2705  515.4991  498.1858
    ## 7: 366.11070 348.59567 349.51106 351.22730  956.8934  876.9782  830.0730
    ## 8: 230.61406 226.82280 223.32279 219.55272  448.4061  449.7884  453.6026
    ## 9: 466.88438 469.91989 473.88486 476.96476  579.9540  575.3879  568.8200
    ##    cvd08male cvd09male cvd10male cvd11male cvd12male cvd13male cvd14male
    ## 1:  126.3548  125.2114  124.1698  124.0550  123.0521  122.3036  120.9398
    ## 2:  362.0445  350.5229  338.1660  323.1285  311.8184  304.8604  299.1565
    ## 3:  158.2836  149.4912  143.7630  137.6913  131.7820  123.9082  117.5575
    ## 4:  761.4547  734.8094  702.9963  649.3122  632.0656  609.2108  620.6342
    ## 5:  648.2151  638.1296  641.1604  572.9933  565.3869  534.3178  552.1560
    ## 6:  484.7670  474.6089  468.5019  460.4963  432.5445  413.7575  433.8364
    ## 7:  825.6957  776.4243  770.5636  706.6609  675.9903  650.0326  646.2977
    ## 8:  455.8597  457.1055  456.0361  453.6313  450.9162  449.0089  446.5765
    ## 9:  566.4953  558.3360  547.3893  545.5135  541.5427  538.4431  532.5702
    ##    cvd15male cvd16male cvd17male cvd18male cvd19male cvd05both cvd06both
    ## 1:  119.3653  118.6682  117.7883  116.9475  115.8910  114.4628  112.7694
    ## 2:  300.1241  297.2979  294.9042  290.2879  288.5816  374.2741  361.9923
    ## 3:  114.1778  110.9073  110.6175  108.2689  108.8687  172.0355  160.0969
    ## 4:  644.2163  618.5638  613.3843  580.3176  569.3491  634.0089  651.1682
    ## 5:  587.3132  566.6405  523.8055  511.8000  498.5963  589.7769  564.1553
    ## 6:  442.4321  433.5059  438.1050  440.8897  433.9799  434.1983  427.1939
    ## 7:  623.4263  604.2681  561.3748  549.7797  549.1739  727.6035  670.5814
    ## 8:  443.7173  440.1349  436.8855  433.6069  429.9362  334.7621  335.3238
    ## 9:  536.2482  536.1051  541.0473  547.2489  550.6548  535.1088  530.9694
    ##    cvd07both cvd08both cvd09both cvd10both cvd11both cvd12both cvd13both
    ## 1:  110.3326  109.3167  109.3833  109.1886  109.4555  108.8728  108.7915
    ## 2:  347.9814  334.1940  324.1761  313.9673  302.7896  294.4027  290.2060
    ## 3:  148.9444  137.6671  128.5412  122.2008  116.6089  111.6049  105.3768
    ## 4:  645.5907  632.7615  603.3141  581.8522  562.6225  543.9251  519.5371
    ## 5:  566.4516  552.6234  539.0321  535.4923  471.5840  460.0871  438.8196
    ## 6:  415.8933  407.1588  399.0904  394.3262  387.9774  366.8452  355.9001
    ## 7:  635.1627  628.2161  592.7751  585.0586  537.7079  514.5833  495.2656
    ## 8:  336.5239  337.0268  337.1512  335.4539  333.0107  329.9312  326.6030
    ## 9:  525.7088  523.7193  517.1861  508.7890  507.4918  504.3873  501.9957
    ##    cvd14both cvd15both cvd16both cvd17both cvd18both cvd19both LEbirth2000
    ## 1:  108.3474 108.12924 107.56173 106.95304 106.39570  105.7119          NA
    ## 2:  286.3140 287.96550 286.88535 285.32918 279.68642  277.7336        74.6
    ## 3:  100.1378  97.49873  95.22265  95.17711  94.68896   95.4053        79.8
    ## 4:  513.0161 523.15984 500.14376 493.82082 473.26712  466.3130        70.2
    ## 5:  449.0384 463.27943 456.27984 432.82716 423.77893  404.9551        70.5
    ## 6:  368.8565 380.88863 384.46844 388.17552 386.67955  381.6978          NA
    ## 7:  490.3853 475.65621 461.71309 434.85592 431.69136  432.9186        72.3
    ## 8:  323.1828 319.13626 314.95461 311.23748 307.70802  303.9047        75.6
    ## 9:  497.8142 500.90386 500.85723 504.82748 509.87687  513.0910        64.7
    ##    LE602000 LEbirth2010 LE602010 LEbirth2015 LE602015 LEbirth2019 LE602019
    ## 1:       NA          NA       NA          NA       NA       84.95       NA
    ## 2:     20.5        78.0     22.3        78.5     22.3       79.10     22.5
    ## 3:     22.7        83.8     25.9        85.1     26.9       86.10     27.9
    ## 4:     18.3        73.3     19.3        75.3     20.2       77.30     21.7
    ## 5:     17.2        73.1     18.2        75.0     19.5       77.10     20.9
    ## 6:       NA          NA       NA          NA       NA       76.47       NA
    ## 7:     18.7        74.7     20.2        76.6     21.4       78.00     22.2
    ## 8:     21.0        77.1     21.5        77.6     21.8       78.10     22.0
    ## 9:     17.9        69.6     18.7        69.6     18.9       68.90     18.7
    ##     phy05  phy06  phy07  phy08  phy09  phy10  phy11  phy12  phy13  phy14  phy15
    ## 1: 3.2319 3.0123 3.0109     NA 3.1479 4.0000     NA     NA     NA     NA 3.3333
    ## 2: 0.8869 0.5361     NA     NA     NA 0.8900     NA     NA     NA 1.5044 1.1526
    ## 3: 1.7529 1.8047 1.8655 1.8407 1.9185 1.9839 2.0361 2.0798 2.1632 2.2070 2.2494
    ## 4:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 5: 2.3783 2.3768 2.3871 2.3545 2.3912 2.3808 2.4142 2.4135 2.5028 2.4746 2.4836
    ## 6:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 7: 2.3204 2.3721 2.3841 2.3812 2.3924 2.3930 6.6305 4.1303 4.0705 4.0114 3.7494
    ## 8:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ## 9:     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
    ##     phy16 phy17edit               HE05               HE06               HE07
    ## 1:     NA    3.3333 5.5754260999999996 4.9350943599999999 4.9255475999999998
    ## 2:     NA    1.1292         5.30572176 5.1986260399999997         5.03977919
    ## 3: 2.3037    2.3608 4.6178216900000004 4.9439678200000001 5.1149072599999998
    ## 4:     NA        NA               <NA>               <NA>               <NA>
    ## 5:     NA    3.2066 7.9639606499999998 8.7857027100000007 9.1826009800000001
    ## 6:     NA        NA               <NA>               <NA>               <NA>
    ## 7: 4.0139    4.0139         4.76693487         4.76170969 4.7431106600000001
    ## 8:     NA        NA               <NA>               <NA>               <NA>
    ## 9:     NA        NA               <NA>               <NA>               <NA>
    ##                  HE08               HE09               HE10               HE11
    ## 1: 5.8059859300000003 6.2023372700000001         6.64963865 6.2465286300000002
    ## 2: 5.2816843999999996 6.5595455200000004 6.7547311800000003 6.6072506899999999
    ## 3:         5.39896727 5.7822899799999998 5.9173440900000003         6.00844383
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5: 9.1230525999999994        11.39545822 10.131128309999999 9.0967035299999992
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ## 7: 4.8986678100000001 5.6382026700000001 4.9660777999999999 4.7900524100000004
    ## 8:               <NA>               <NA>               <NA>               <NA>
    ## 9:               <NA>               <NA>               <NA>               <NA>
    ##                  HE12               HE13               HE14               HE15
    ## 1: 6.1015033699999996 5.9878034600000003 5.9791245499999999 6.2324533500000001
    ## 2: 6.6364860500000002         5.99379873 6.9135108000000001 7.7605791100000001
    ## 3:         6.13262033 6.2478942899999996 6.4743809700000003 6.6527166400000004
    ## 4:               <NA>               <NA>               <NA>               <NA>
    ## 5: 9.1396207799999996 8.6830034299999994 8.6327571899999995 8.5576353100000002
    ## 6:               <NA>               <NA>               <NA>               <NA>
    ## 7: 4.9408035300000002 5.0798091899999998 5.1802287099999997 5.2956042300000004
    ## 8:               <NA>               <NA>               <NA>               <NA>
    ## 9:               <NA>               <NA>               <NA>               <NA>
    ##                  HE16               HE17 scl05 scl06 scl07 scl08 scl09 scl10
    ## 1: 6.3434934600000004 6.5443186799999999   9.8  10.1  10.1  10.1  10.1  10.1
    ## 2: 8.8595066100000004 8.6596641499999993    NA    NA    NA    NA    NA    NA
    ## 3: 6.9143271400000001         7.10694933    NA    NA    NA    NA    NA    NA
    ## 4:               <NA>               <NA>  10.2  10.2  10.3  10.3  10.4  10.6
    ## 5: 7.5355224600000001 7.0132851599999997    NA    NA    NA    NA    NA    NA
    ## 6:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 7: 5.2652196900000003         5.34388065    NA    NA    NA    NA    NA    NA
    ## 8:               <NA>               <NA>    NA    NA    NA    NA    NA    NA
    ## 9:               <NA>               <NA>   1.9   2.0   2.2   2.3   2.5   2.6
    ##    scl11 scl12 scl13 scl14 scl15 scl16 scl17     GDP05     GDP06     GDP07
    ## 1:  10.2  10.2  10.2  10.2  10.2  10.2  10.2 40066.257 42675.813 47803.694
    ## 2:    NA    NA    NA    NA    NA    NA    NA        NA        NA        NA
    ## 3:    NA    NA    NA    NA    NA    NA    NA        NA        NA        NA
    ## 4:  10.6  10.7  10.7  10.8  10.8  10.9  10.9        NA        NA        NA
    ## 5:    NA    NA    NA    NA    NA    NA    NA        NA        NA        NA
    ## 6:    NA    NA    NA    NA    NA    NA    NA        NA        NA        NA
    ## 7:    NA    NA    NA    NA    NA    NA    NA  5323.463  6920.189  9101.255
    ## 8:    NA    NA    NA    NA    NA    NA    NA        NA        NA        NA
    ## 9:   2.8   3.0   3.0   3.0   3.0   3.0   3.0        NA        NA        NA
    ##       GDP08     GDP09    GDP10    GDP11    GDP12    GDP13    GDP14     GDP15
    ## 1: 48718.50 43503.186 40852.67 43335.33 38686.46 39538.77 41303.93 35762.523
    ## 2:       NA        NA       NA       NA       NA       NA       NA        NA
    ## 3:       NA        NA       NA       NA       NA       NA       NA        NA
    ## 4:       NA        NA       NA       NA       NA       NA       NA        NA
    ## 5:       NA        NA       NA       NA       NA       NA       NA        NA
    ## 6:       NA        NA       NA       NA       NA       NA       NA        NA
    ## 7: 11635.27  8562.813 10675.00 14311.08 15420.87 15974.64 14095.65  9313.014
    ## 8:       NA        NA       NA       NA       NA       NA       NA        NA
    ## 9:       NA        NA       NA       NA       NA       NA       NA        NA
    ##        GDP16    GDP17 MMR05 MMR06 MMR07 MMR08 MMR09 MMR10 MMR11 MMR12 MMR13
    ## 1: 37474.665 38962.88    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 2:        NA       NA    34    32    30    28    25    22    19    18    17
    ## 3:        NA       NA    15    14    15    15    16    15    14    13    13
    ## 4:        NA       NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 5:        NA       NA    34    31    31    29    28    29    21    22    21
    ## 6:        NA       NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 7:  8704.898 10720.33    42    36    32    30    27    25    23    22    20
    ## 8:        NA       NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ## 9:        NA       NA    NA    NA    NA    NA    NA    NA    NA    NA    NA
    ##    MMR14 MMR15 MMR16 MMR17               LMH09               LMH14
    ## 1:    NA    NA    NA    NA         high income         high income
    ## 2:    17    17    16    16 upper middle income upper middle income
    ## 3:    12    12    11    11         high income         high income
    ## 4:    NA    NA    NA    NA                <NA>                <NA>
    ## 5:    23    22    20    19 lower middle income lower middle income
    ## 6:    NA    NA    NA    NA                <NA>                <NA>
    ## 7:    19    18    18    17 upper middle income         high income
    ## 8:    NA    NA    NA    NA                <NA>                <NA>
    ## 9:    NA    NA    NA    NA                <NA>                <NA>

``` r
#now have 10 rows with one missing either outcome or confounding. 

analysis17 <- edit_phy17[, c("location_name", "GSNI_PERIOD", "twobias", "cvd17fem", "cvd17both", "LEbirth2019", "phy17edit", "scl17", "GDP17", "MMR17")]
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
# table name = analysis17
```

``` r
write.csv(analysis14,"analysis14.csv")
write.csv(analysis17,"analysis17.csv")
```

``` r
countrylist <- cbind(edit[, c("location_name", "LMH14")])
countrylist
```

    ##                 location_name               LMH14
    ##  1:                   Algeria upper middle income
    ##  2:                   Andorra         high income
    ##  3:                 Argentina         high income
    ##  4:                   Armenia lower middle income
    ##  5:                 Australia         high income
    ##  6:                Azerbaijan upper middle income
    ##  7:                   Belarus upper middle income
    ##  8:                    Brazil upper middle income
    ##  9:                  Bulgaria upper middle income
    ## 10:              Burkina Faso          low income
    ## 11:                    Canada         high income
    ## 12:                     Chile         high income
    ## 13:                     China upper middle income
    ## 14:                  Colombia upper middle income
    ## 15:                    Cyprus         high income
    ## 16:                   Ecuador upper middle income
    ## 17:                   Estonia         high income
    ## 18:                  Ethiopia          low income
    ## 19:                   Finland         high income
    ## 20:                    France         high income
    ## 21:                   Georgia lower middle income
    ## 22:                   Germany         high income
    ## 23:                     Ghana lower middle income
    ## 24:                     Haiti          low income
    ## 25:                   Hungary         high income
    ## 26:                     India lower middle income
    ## 27:                 Indonesia lower middle income
    ## 28: Iran, Islamic Republic of upper middle income
    ## 29:                      Iraq upper middle income
    ## 30:                     Japan         high income
    ## 31:                    Jordan upper middle income
    ## 32:                Kazakhstan upper middle income
    ## 33:       Korea (Republic of)         high income
    ## 34:                    Kuwait         high income
    ## 35:                Kyrgyzstan                <NA>
    ## 36:                   Lebanon upper middle income
    ## 37:                     Libya upper middle income
    ## 38:                  Malaysia upper middle income
    ## 39:                      Mali          low income
    ## 40:                    Mexico upper middle income
    ## 41:      Moldova, Republic of lower middle income
    ## 42:                   Morocco lower middle income
    ## 43:               Netherlands         high income
    ## 44:               New Zealand         high income
    ## 45:                   Nigeria lower middle income
    ## 46:                    Norway         high income
    ## 47:                  Pakistan lower middle income
    ## 48:       Palestine, State of                <NA>
    ## 49:                      Peru upper middle income
    ## 50:               Philippines lower middle income
    ## 51:                    Poland         high income
    ## 52:                     Qatar         high income
    ## 53:                   Romania upper middle income
    ## 54:        Russian Federation         high income
    ## 55:                    Rwanda          low income
    ## 56:                    Serbia upper middle income
    ## 57:                 Singapore         high income
    ## 58:                  Slovenia         high income
    ## 59:              South Africa upper middle income
    ## 60:                     Spain         high income
    ## 61:                    Sweden         high income
    ## 62:               Switzerland         high income
    ## 63:                  Thailand upper middle income
    ## 64:       Trinidad and Tobago         high income
    ## 65:                   Tunisia upper middle income
    ## 66:                    Turkey upper middle income
    ## 67:                   Ukraine lower middle income
    ## 68:            United Kingdom         high income
    ## 69:             United States         high income
    ## 70:                   Uruguay         high income
    ## 71:                Uzbekistan lower middle income
    ## 72:                  Viet Nam                <NA>
    ## 73:                     Yemen                <NA>
    ## 74:                    Zambia lower middle income
    ## 75:                  Zimbabwe          low income
    ##                 location_name               LMH14

``` r
write.csv(countrylist, "countrylist.csv")

countrylist75 <- cbind(gsni_cvd_lifexp_phys_sch_gdp_mmr_lmh[, c("location_name", "LMH14")])
countrylist75
```

    ##                 location_name               LMH14
    ##  1:                   Algeria upper middle income
    ##  2:                   Andorra         high income
    ##  3:                 Argentina         high income
    ##  4:                   Armenia lower middle income
    ##  5:                 Australia         high income
    ##  6:                Azerbaijan upper middle income
    ##  7:                   Belarus upper middle income
    ##  8:                    Brazil upper middle income
    ##  9:                  Bulgaria upper middle income
    ## 10:              Burkina Faso          low income
    ## 11:                    Canada         high income
    ## 12:                     Chile         high income
    ## 13:                     China upper middle income
    ## 14:                  Colombia upper middle income
    ## 15:                    Cyprus         high income
    ## 16:                   Ecuador upper middle income
    ## 17:                   Estonia         high income
    ## 18:                  Ethiopia          low income
    ## 19:                   Finland         high income
    ## 20:                    France         high income
    ## 21:                   Georgia lower middle income
    ## 22:                   Germany         high income
    ## 23:                     Ghana lower middle income
    ## 24:                     Haiti          low income
    ## 25:                   Hungary         high income
    ## 26:                     India lower middle income
    ## 27:                 Indonesia lower middle income
    ## 28: Iran, Islamic Republic of upper middle income
    ## 29:                      Iraq upper middle income
    ## 30:                     Japan         high income
    ## 31:                    Jordan upper middle income
    ## 32:                Kazakhstan upper middle income
    ## 33:       Korea (Republic of)         high income
    ## 34:                    Kuwait         high income
    ## 35:                Kyrgyzstan                <NA>
    ## 36:                   Lebanon upper middle income
    ## 37:                     Libya upper middle income
    ## 38:                  Malaysia upper middle income
    ## 39:                      Mali          low income
    ## 40:                    Mexico upper middle income
    ## 41:      Moldova, Republic of lower middle income
    ## 42:                   Morocco lower middle income
    ## 43:               Netherlands         high income
    ## 44:               New Zealand         high income
    ## 45:                   Nigeria lower middle income
    ## 46:                    Norway         high income
    ## 47:                  Pakistan lower middle income
    ## 48:       Palestine, State of                <NA>
    ## 49:                      Peru upper middle income
    ## 50:               Philippines lower middle income
    ## 51:                    Poland         high income
    ## 52:                     Qatar         high income
    ## 53:                   Romania upper middle income
    ## 54:        Russian Federation         high income
    ## 55:                    Rwanda          low income
    ## 56:                    Serbia upper middle income
    ## 57:                 Singapore         high income
    ## 58:                  Slovenia         high income
    ## 59:              South Africa upper middle income
    ## 60:                     Spain         high income
    ## 61:                    Sweden         high income
    ## 62:               Switzerland         high income
    ## 63:                  Thailand upper middle income
    ## 64:       Trinidad and Tobago         high income
    ## 65:                   Tunisia upper middle income
    ## 66:                    Turkey upper middle income
    ## 67:                   Ukraine lower middle income
    ## 68:            United Kingdom         high income
    ## 69:             United States         high income
    ## 70:                   Uruguay         high income
    ## 71:                Uzbekistan lower middle income
    ## 72:                  Viet Nam                <NA>
    ## 73:                     Yemen                <NA>
    ## 74:                    Zambia lower middle income
    ## 75:                  Zimbabwe          low income
    ##                 location_name               LMH14

``` r
write.csv(countrylist75, "countrylist75.csv")
```
