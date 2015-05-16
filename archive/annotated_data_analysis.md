---
title: "Annotated data analysis"
output: 
  html_document:
  toc: true
---

## Data cleaning

```r
setwd("D:/Dropbox/projects/2015lag/archive")

# Dependencies
library(stringr)
library(ggplot2)
library(knitr)
library(plyr)

# Read in data
dat <- read.csv(file = '20150514collected_data.csv')

# Assimilate journal naming
# See discrepancies
table(dat$data.journal)
```

```
## 
##                             none                     PLoS Biology 
##                             6632                             1947 
##                     PLOS Biology             PLoS Clinical Trials 
##                               56                               44 
##       PLoS Computational Biology       PLOS Computational Biology 
##                             3179                              180 
##                    PLoS Genetics                    PLOS Genetics 
##                             4373                              255 
##                     PLoS Medicin                    PLoS Medicine 
##                                1                             1031 
##                    PLOS Medicine PLoS Neglected Tropical Diseases 
##                               20                             2655 
## PLOS Neglected Tropical Diseases                         PLoS One 
##                              262                                1 
##                         PLoS ONE                         PLOS ONE 
##                           107911                             9653 
##                   PLoS Pathogens                   PLOS Pathogens 
##                             3683                              216
```

```r
# Most due to using PLoS and PLOS

# Solve this by making all journal names lowercase
dat$data.journal <- tolower(dat$data.journal)
table(dat$data.journal)
```

```
## 
##                             none                     plos biology 
##                             6632                             2003 
##             plos clinical trials       plos computational biology 
##                               44                             3359 
##                    plos genetics                     plos medicin 
##                             4628                                1 
##                    plos medicine plos neglected tropical diseases 
##                             1051                             2917 
##                         plos one                   plos pathogens 
##                           117565                             3899
```

```r
# plos medicin = typo
dat$data.journal[dat$data.journal == "plos medicin"] <- "plos medicine"

# remove the journals with "none"
dat <- dat[!dat$data.journal == "none", ]
table(dat$data.journal)
```

```
## 
##                     plos biology             plos clinical trials 
##                             2003                               44 
##       plos computational biology                    plos genetics 
##                             3359                             4628 
##                    plos medicine plos neglected tropical diseases 
##                             1052                             2917 
##                         plos one                   plos pathogens 
##                           117565                             3899
```

```r
# Making the dates readable
dat$received <- as.Date(substring(dat$data.received_date,
                                  first = 0,
                                  last = 10))
dat$accepted <- as.Date(substring(dat$data.accepted_date,
                                  first = 0,
                                  last = 10))
dat$published <- as.Date(substring(dat$data.publication_date,
                                   first = 0,
                                   last = 10))

# Listwise deletion
# Nr of rows deleted
dim(dat)[1] - sum(!is.na(dat$received) &
                    !is.na(dat$accepted) &
                    !is.na(dat$published))
```

```
## [1] 10
```

```r
dat <- dat[!is.na(dat$received) &
             !is.na(dat$accepted) &
             !is.na(dat$published), ]

# Adding a year variable based on year published
dat$year <- as.numeric(substring(dat$data.publication_date,
                                 first = 0,
                                 last = 4))

# Adding the number of authors on the paper
# By counting the separators + 1 (for initial author)
# E.g., A; B counts 1 semi-colon, + 1 = 2 authors
dat$authors <- str_count(string = dat$data.author, ";")

# Adding a dummy for competing interests
# 0 = "no competing interests"
# 1 = !"no competing interest"
dat$coi <- !grepl(pattern = "no competing interests",
                  dat$data.competing_interest)
```

## Date checking
The data was cleaned in several steps, but the data were also inspected for some illogical dates, where the paper was published before it was accepted (i.e., 2 cases) or accepted before received (i.e., 33 cases). Below I give the DOIs for those papers and eliminate them from the dataset.


```r
# Checking for illogical dates
# Published before accepted
as.matrix(dat$data.id[dat$published < dat$accepted])
```

```
##      [,1]                          
## [1,] "10.1371/journal.pone.0052595"
## [2,] "10.1371/journal.pone.0005466"
```

```r
dat <- dat[!dat$published < dat$accepted, ]

# Accepted before received
as.matrix(dat$data.id[dat$accepted < dat$received])
```

```
##       [,1]                          
##  [1,] "10.1371/journal.pntd.0003729"
##  [2,] "10.1371/journal.pmed.0040325"
##  [3,] "10.1371/journal.pbio.0030204"
##  [4,] "10.1371/journal.pbio.0030092"
##  [5,] "10.1371/journal.pbio.0040092"
##  [6,] "10.1371/journal.pone.0080696"
##  [7,] "10.1371/journal.pntd.0001618"
##  [8,] "10.1371/journal.pntd.0001819"
##  [9,] "10.1371/journal.pntd.0001741"
## [10,] "10.1371/journal.pntd.0002247"
## [11,] "10.1371/journal.pntd.0002177"
## [12,] "10.1371/journal.ppat.1004231"
## [13,] "10.1371/journal.pone.0098569"
## [14,] "10.1371/journal.ppat.1003643"
## [15,] "10.1371/journal.ppat.1003408"
## [16,] "10.1371/journal.ppat.1003411"
## [17,] "10.1371/journal.ppat.1003356"
## [18,] "10.1371/journal.ppat.1003330"
## [19,] "10.1371/journal.ppat.1003580"
## [20,] "10.1371/journal.ppat.1003454"
## [21,] "10.1371/journal.pntd.0002876"
## [22,] "10.1371/journal.pntd.0003002"
## [23,] "10.1371/journal.pntd.0002394"
## [24,] "10.1371/journal.pntd.0002230"
## [25,] "10.1371/journal.pone.0105900"
## [26,] "10.1371/journal.pntd.0002360"
## [27,] "10.1371/journal.pntd.0002219"
## [28,] "10.1371/journal.pntd.0002215"
## [29,] "10.1371/journal.pntd.0002196"
## [30,] "10.1371/journal.pntd.0002305"
## [31,] "10.1371/journal.pcbi.0020013"
## [32,] "10.1371/journal.pcbi.0020027"
## [33,] "10.1371/journal.ppat.1002754"
```

```r
dat <- dat[!dat$accepted < dat$received, ]
```
## Data prepping
### Computing days between received, accepted, and published
In order to actually conduct analyses on the publication lag, the dates for received-, accepted-, and published submission must be reformatted into difference data in days. The actual code running these computations is commented out to save time. The objects loaded in are saves from a previous run of the commented out code.


```r
# Calculating the days between received, accepted, and published
calc_days <- Vectorize(function(a, b) 
  length(seq(a, b, "days"))) 

# received_accepted <- calc_days(dat$received, dat$accepted)
# save(received_accepted, file = "received_accepted")
# 
# accepted_published <- calc_days(dat$accepted, dat$ published)
# save(accepted_published, file = "accepted_published")
# 
# received_published <- calc_days(dat$received, dat$published)
# save(received_published, file = "received_published")

# Load the objects created in the commented out code above
# Decreases runtime
load("received_accepted")
dat$received_accepted <- received_accepted
load("accepted_published")
dat$accepted_published <- accepted_published
load("received_published")
dat$received_published <- received_published
```

## Data analysis
Prior to data analysis I stress once again that the data *is* the population of PLOS research articles and that making inferences based on p-value hypothesis testing therefore makes no sense. I will therefore eliminate all hypothesis testing statistics and limit myself to point estimates.

### Descriptives
The median full publication cycle is 153 days, with the majority of this being the review process (i.e., 112 days) and not the production process (i.e., 39 days). When we split this per journal, we see the following publication cycle.

```
##                       data.journal Min. 1st Qu. Median  Mean 3rd Qu. Max.
## 7                         plos one   15   113.0  147.0 163.7     194 1956
## 2             plos clinical trials   92   156.2  181.5 182.0     206  304
## 4                    plos genetics   31   143.0  183.0 196.8     233 1053
## 6 plos neglected tropical diseases   50   145.0  185.0 200.6     234 1188
## 8                   plos pathogens   50   147.0  185.0 197.4     231 1033
## 1                     plos biology   43   148.0  191.0 201.9     240  651
## 3       plos computational biology   66   162.0  199.0 215.5     249 1163
## 5                    plos medicine   34   189.5  232.0 247.1     286  884
```
which, when split up into the review process and production process looks as follows

```
##                       data.journal Min. 1st Qu. Median  Mean 3rd Qu. Max.
## 7                         plos one    1   75.00    107 123.8   152.0 1928
## 2             plos clinical trials   24   99.75    126 127.1   156.0  246
## 4                    plos genetics    3   93.00    132 145.5   180.0 1001
## 6 plos neglected tropical diseases   17   98.00    135 152.2   185.0 1150
## 8                   plos pathogens    1  102.00    140 152.4   185.0 1009
## 1                     plos biology    8   97.00    142 152.0   191.2  591
## 3       plos computational biology   14  112.00    149 163.8   197.0 1118
## 5                    plos medicine    9  137.00    176 192.7   227.5  824
```

```
##                       data.journal Min. 1st Qu. Median  Mean 3rd Qu. Max.
## 7                         plos one    1      30     37 40.87      47  759
## 8                   plos pathogens   14      35     44 45.98      54  226
## 6 plos neglected tropical diseases   11      38     46 49.44      56  217
## 1                     plos biology   16      42     47 50.84      56  423
## 5                    plos medicine    9      42     48 55.34      64  166
## 3       plos computational biology   23      39     49 52.67      60  259
## 4                    plos genetics   17      37     51 52.22      61  510
## 2             plos clinical trials   38      46     53 55.86      64   86
```
This indicates that the publication cycle is shortest for PLOS ONE, and longest for PLOS Medicine. This could be due to efficiency in handling more publications (i.e., ONE: 117551; Med.: 1051), but could also represent selectivity. PLOS ONE prouds itself of selecting papers only on scientific rigor and not on results, whereas PLOS medicine does include selectivity in its criteria for publication (e.g., originality of research; see their guidelines [here](http://journals.plos.org/plosmedicine/s/journal-information)).

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

These plots indicate that publication cycles have increased in length, except for Neglected Tropical Diseases, which shows a decreasing trend. Clinical trials was only published in 2006 and 2007, after which it was discontinued and rolled into PLOS ONE. Considering that the review process and production process are substantively different, it makes sense to investigate whether these trends differ across these parts of the publication cycle.

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

The trends for the review process seem highly similar to the overall. Considering that the review process takes up the largest part of the entire publication cycle (i.e., 112 days of the full 153 days), it makes sense that the trends for the full publication cycle are mostly made up of the trends in the review process.

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

However, when we look at the production process, we see that the trends are relatively stable or decreasing. This indicates that some of the PLOS journals have increased the efficiency of the production process, whereas others have not. 

From these analyses I conclude two things in analyzing publication cycles:

1. The year of the publication should be taken into account
2. Not all PLOS journals are alike, so inspect whether the overall trend is valid for the journals separately.

### Correlational data analysis
The correlation between the time to go from received to accepted and the time to go from accepted to published is 0.049. Note that competing interests is excluded from the correlation matrix because it is a dummy variable. Below the squared correlation matrix is given, for easier interpretation.


```r
d <- data.frame(receive_accept = as.numeric(received_accepted),
                accept_publish = as.numeric(accepted_published),
                receive_publish = as.numeric(received_published),
                authors = as.numeric(dat$authors),
                pages = as.numeric(dat$data.pagecount), 
                years = dat$year)
cor(d)^2
```

```
##                 receive_accept accept_publish receive_publish      authors
## receive_accept    1.0000000000    0.002370601    0.9526938694 0.0002264097
## accept_publish    0.0023706007    1.000000000    0.0701005015 0.0045490508
## receive_publish   0.9526938694    0.070100502    1.0000000000 0.0008534703
## authors           0.0002264097    0.004549051    0.0008534703 1.0000000000
## pages             0.0071925442    0.003845470    0.0090979125 0.0017404394
## years             0.0105719431    0.010204915    0.0147054730 0.0001078640
##                       pages       years
## receive_accept  0.007192544 0.010571943
## accept_publish  0.003845470 0.010204915
## receive_publish 0.009097913 0.014705473
## authors         0.001740439 0.000107864
## pages           1.000000000 0.003796193
## years           0.003796193 1.000000000
```

Here we see that squared correlations between the days from receive to accept and receive to published are high. This is logical because the majority of the publication cycle *is* the review process. The publication process, on the other hand, has only a medium correlation with the entire publication cycle.

Other squared correlations are all small. The largest uncontrolled effect is 1% explained variance. Controlling for other explanatory variables is likely to have only little effect, because of low squared correlations between the explanatory variables.

This indicates the regression analyses will most likely indicate that the effects of the predictor variables will be small and the publication process will prove highly random in its duration.

### Linearity
Because correlations rest on the assumption of linearity, let us check the linearity to ensure that we are not jumping the gun with the previous section (should actually do this before but I forgot...)

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

The plots of the predictor variables on the publication variables indicate curvilinear relations. This means that a linear correlation is insufficient to model the relationship.

### Regression models
Before running the regression models, let me recapitulate what the previous analyses indicated:

1. Publication year as covariate
2. Analyze review process and production process separately
3. Linear and curvilinear estimates for predictor variables.
4. Check whether overall results hold for journals separately

Additionally, I will mean center the predictor variables `authors` and `pages` so intercept estimates are meaningful. All regression models will be Poisson regression, considering that number of days in each part of publication cycle is a count variable.


```r
dat$authors.centred <- as.numeric(dat$authors - mean(dat$authors, na.rm = TRUE))
dat$authors.centred.sq <- authors.centred^2
dat$pages.centred <- as.numeric(dat$data.pagecount) - mean(as.numeric(dat$data.pagecount), na.rm = TRUE)
dat$pages.centred.sq <- pages.centred^2

x <- glm(received_accepted ~ 
           authors.centred +
           authors.centred.sq + 
           pages.centred + 
           pages.centred.sq +
           as.factor(year),
         data = dat[dat$data.journal == unique(dat$data.journal)[1],],
         family = "poisson")

options(scipen = 5)
summary(x)
```

```
## 
## Call:
## glm(formula = received_accepted ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year), family = "poisson", 
##     data = dat[dat$data.journal == unique(dat$data.journal)[1], 
##         ])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -15.865   -4.656   -1.507    2.380   86.432  
## 
## Coefficients:
##                          Estimate    Std. Error z value Pr(>|z|)    
## (Intercept)          3.6427879668  0.0141154062 258.072  < 2e-16 ***
## authors.centred      0.0005002302  0.0000696875   7.178 7.06e-13 ***
## authors.centred.sq  -0.0000005881  0.0000012288  -0.479    0.632    
## pages.centred       -0.0008501991  0.0000154919 -54.880  < 2e-16 ***
## pages.centred.sq    -0.0000445039  0.0000014955 -29.758  < 2e-16 ***
## as.factor(year)2007  0.7035485911  0.0144677499  48.629  < 2e-16 ***
## as.factor(year)2008  0.9253651227  0.0142255981  65.049  < 2e-16 ***
## as.factor(year)2009  0.9534771925  0.0141700094  67.288  < 2e-16 ***
## as.factor(year)2010  1.0560690485  0.0141358300  74.709  < 2e-16 ***
## as.factor(year)2011  1.1157804118  0.0141092582  79.081  < 2e-16 ***
## as.factor(year)2012  1.2241281041  0.0141027586  86.801  < 2e-16 ***
## as.factor(year)2013  1.2174888520  0.0140958605  86.372  < 2e-16 ***
## as.factor(year)2014  1.2670928198  0.0140953974  89.894  < 2e-16 ***
## as.factor(year)2015  1.3730553890  0.0141137364  97.285  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 4762596  on 117550  degrees of freedom
## Residual deviance: 4560079  on 117537  degrees of freedom
## AIC: 5323216
## 
## Number of Fisher Scoring iterations: 5
```

The intercept estimate indicates that the average publication takes 38.198 days in the review process.
