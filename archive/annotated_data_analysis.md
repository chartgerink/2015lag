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
source('multiplot.r')

# Read in data
dat <- read.csv(file = '20150604collected_data.csv')

# Assimilate journal naming
# See discrepancies
table(dat$data.journal)
```

```
## 
##                             none                     PLoS Biology 
##                             6632                             1947 
##                     PLOS Biology             PLoS Clinical Trials 
##                               61                               44 
##       PLoS Computational Biology       PLOS Computational Biology 
##                             3179                              209 
##                    PLoS Genetics                    PLOS Genetics 
##                             4373                              303 
##                     PLoS Medicin                    PLoS Medicine 
##                                1                             1031 
##                    PLOS Medicine PLoS Neglected Tropical Diseases 
##                               26                             2655 
## PLOS Neglected Tropical Diseases                         PLoS One 
##                              300                                1 
##                         PLoS ONE                         PLOS ONE 
##                           107911                            11539 
##                   PLoS Pathogens                   PLOS Pathogens 
##                             3683                              254
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
##                             6632                             2008 
##             plos clinical trials       plos computational biology 
##                               44                             3388 
##                    plos genetics                     plos medicin 
##                             4676                                1 
##                    plos medicine plos neglected tropical diseases 
##                             1057                             2955 
##                         plos one                   plos pathogens 
##                           119451                             3937
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
##                             2008                               44 
##       plos computational biology                    plos genetics 
##                             3388                             4676 
##                    plos medicine plos neglected tropical diseases 
##                             1058                             2955 
##                         plos one                   plos pathogens 
##                           119451                             3937
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
The data was cleaned in several steps, but the data were also inspected for some illogical dates, where the paper was published before it was accepted (i.e., 2 cases) or accepted before received (i.e., 34 cases). Below I give the DOIs for those papers and eliminate them from the dataset.


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
## [34,] "10.1371/journal.ppat.1004917"
```

```r
dat <- dat[!dat$accepted < dat$received, ]

# Selecting out those papers which have equivalent
# received - accepted
as.matrix(dat$data.id[dat$received == dat$accepted])
```

```
##      [,1]                          
## [1,] "10.1371/journal.pone.0031292"
## [2,] "10.1371/journal.ppat.0030027"
## [3,] "10.1371/journal.pone.0001311"
```

```r
dat <- dat[!dat$received == dat$accepted, ]

print(dim(dat)[1])
```

```
## [1] 137468
```

```r
print(table(dat$data.journal))
```

```
## 
##                     plos biology             plos clinical trials 
##                             2005                               44 
##       plos computational biology                    plos genetics 
##                             3385                             4676 
##                    plos medicine plos neglected tropical diseases 
##                             1057                             2940 
##                         plos one                   plos pathogens 
##                           119435                             3926
```
## Data prepping
### Computing days between received, accepted, and published
In order to actually conduct analyses on the publication lag, the dates for received-, accepted-, and published submission must be reformatted into difference data in days. The actual code running these computations is commented out to save time. The objects loaded in are saves from a previous run of the commented out code.


```r
# Calculating the days between received, accepted, and published
calc_days <- Vectorize(function(a, b) 
  length(seq(a, b, "days")) - 1)
# Minus 1 because otherwise count will be 1 for the same date

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
The median full publication cycle is 152 days, with the majority of this being the review process (i.e., 111 days) and not the production process (i.e., 38 days). When we split this per journal, we see the following publication cycle.

```r
x <- ddply(.data = dat, .(data.journal),
           .fun = function(x) summary(x$received_published))
x[order(x$Median),]
```

```
##                       data.journal Min. 1st Qu. Median  Mean 3rd Qu. Max.
## 7                         plos one   14   112.0  147.0 163.1     193 1955
## 2             plos clinical trials   91   155.2  180.5 181.0     205  303
## 4                    plos genetics   30   142.0  182.0 195.7     232 1052
## 6 plos neglected tropical diseases   49   143.0  183.0 199.4     233 1187
## 8                   plos pathogens   23   146.0  183.0 196.3     230 1032
## 1                     plos biology   42   147.0  190.0 200.9     239  650
## 3       plos computational biology   65   161.0  199.0 214.7     248 1162
## 5                    plos medicine   33   189.0  231.0 246.1     285  883
```
which, when split up into the review process and production process looks as follows

```r
x <- ddply(.data = dat, .(data.journal),
           .fun = function(x) summary(x$received_accepted))
x[order(x$Median),]
```

```
##                       data.journal Min. 1st Qu. Median  Mean 3rd Qu. Max.
## 7                         plos one    1   74.00  106.0 123.1     151 1927
## 2             plos clinical trials   23   98.75  125.0 126.1     155  245
## 4                    plos genetics    2   93.00  131.0 144.6     179 1000
## 6 plos neglected tropical diseases   16   97.00  133.0 151.2     184 1149
## 8                   plos pathogens    5  101.00  138.5 151.4     184 1008
## 1                     plos biology    7   96.00  141.0 151.1     191  590
## 3       plos computational biology   13  111.00  148.0 162.9     196 1117
## 5                    plos medicine    8  137.00  175.0 191.8     227  823
```

```r
x <- ddply(.data = dat, .(data.journal),
           .fun = function(x) summary(x$accepted_published))
x[order(x$Median),]
```

```
##                       data.journal Min. 1st Qu. Median  Mean 3rd Qu. Max.
## 7                         plos one    0      29     37 39.98      46  758
## 8                   plos pathogens   13      33     43 44.86      53  225
## 6 plos neglected tropical diseases   10      37     45 48.19      55  216
## 1                     plos biology   15      41     46 49.83      55  422
## 5                    plos medicine    8      41     47 54.27      63  165
## 3       plos computational biology   22      38     48 51.78      59  258
## 4                    plos genetics   16      36     50 51.09      60  509
## 2             plos clinical trials   37      45     52 54.86      63   85
```
This indicates that the publication cycle is shortest for PLOS ONE, and longest for PLOS Medicine. This could be due to efficiency in handling more publications (i.e., ONE: 119435; Med.: 1057), but could also represent selectivity. PLOS ONE prouds itself of selecting papers only on scientific rigor and not on results, whereas PLOS medicine does include selectivity in its criteria for publication (e.g., originality of research; see their guidelines [here](http://journals.plos.org/plosmedicine/s/journal-information)).


```r
x <- ddply(dat, .(data.journal, year), function(x) summary(x$received_published))

ggplot(x, aes(x = year, y = Median, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(list(title = "Full publication cycle", x = "Median days", y = "Years")) +
  xlim(c(2003, 2015)) + 
  ylim(c(0, 225))
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 7 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 8 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 2 rows containing missing values
## (geom_path).
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

These plots indicate that publication cycles have increased in length, except for Neglected Tropical Diseases, which shows a decreasing trend. Clinical trials was only published in 2006 and 2007, after which it was discontinued and rolled into PLOS ONE. Considering that the review process and production process are substantively different, it makes sense to investigate whether these trends differ across these parts of the publication cycle.


```r
x <- ddply(dat, .(data.journal, year), function(x) summary(x$received_accepted))

p1 <- ggplot(x, aes(x = year, y = Median, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(list(title = "Review process", x = "Median days", y = "Years")) +
  xlim(c(2003, 2015)) + 
  ylim(c(0, 225)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks=2003:2015)
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will replace the existing scale.
```

```r
print(p1)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

The trends for the review process seem highly similar to the overall. Considering that the review process takes up the largest part of the entire publication cycle (i.e., 111 days of the full 152 days), it makes sense that the trends for the full publication cycle are mostly made up of the trends in the review process.


```r
x <- ddply(dat, .(data.journal, year), function(x) summary(x$accepted_published))

p2 <- ggplot(x, aes(x = year, y = Median, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(list(title = "Production process", x = "Median days", y = "Years")) +
  xlim(c(2003, 2015)) + 
  ylim(c(0, 225)) + 
  theme(legend.position = "top") + 
  scale_x_continuous(breaks = 2003:2015)
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will replace the existing scale.
```

```r
print(p2)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

However, when we look at the production process, we see that the trends are relatively stable or decreasing. This indicates that some of the PLOS journals have increased the efficiency of the production process, whereas others have not. 

From these analyses I conclude two things in analyzing publication cycles:

1. The year of the publication should be taken into account
2. Not all PLOS journals are alike, so inspect whether the overall trend is valid for the journals separately.

### Correlational data analysis
The correlation between the time to go from received to accepted and the time to go from accepted to published is 0.05. Note that competing interests is excluded from the correlation matrix because it is a dummy variable. Below the squared correlation matrix is given, for easier interpretation.


```r
d <- data.frame(receive_accept = as.numeric(received_accepted),
                accept_publish = as.numeric(accepted_published),
                receive_publish = as.numeric(received_published),
                authors = as.numeric(dat$authors),
                pages = as.numeric(dat$data.pagecount), 
                years = dat$year)
cor(d)
```

```
##                 receive_accept accept_publish receive_publish     authors
## receive_accept      1.00000000     0.04984820      0.97606857  0.01466268
## accept_publish      0.04984820     1.00000000      0.26584788  0.06653484
## receive_publish     0.97606857     0.26584788      1.00000000  0.02863951
## authors             0.01466268     0.06653484      0.02863951  1.00000000
## pages              -0.08555901    -0.06373101     -0.09645922 -0.04138199
## years               0.10468037     0.10399521      0.12368231  0.01030564
##                       pages       years
## receive_accept  -0.08555901  0.10468037
## accept_publish  -0.06373101  0.10399521
## receive_publish -0.09645922  0.12368231
## authors         -0.04138199  0.01030564
## pages            1.00000000 -0.07213815
## years           -0.07213815  1.00000000
```

```r
cor(d)^2
```

```
##                 receive_accept accept_publish receive_publish      authors
## receive_accept    1.0000000000    0.002484843    0.9527098585 0.0002149941
## accept_publish    0.0024848434    1.000000000    0.0706750963 0.0044268849
## receive_publish   0.9527098585    0.070675096    1.0000000000 0.0008202215
## authors           0.0002149941    0.004426885    0.0008202215 1.0000000000
## pages             0.0073203450    0.004061641    0.0093043819 0.0017124688
## years             0.0109579799    0.010815003    0.0152973147 0.0001062062
##                       pages        years
## receive_accept  0.007320345 0.0109579799
## accept_publish  0.004061641 0.0108150035
## receive_publish 0.009304382 0.0152973147
## authors         0.001712469 0.0001062062
## pages           1.000000000 0.0052039124
## years           0.005203912 1.0000000000
```

Here we see that squared correlations between the days from receive to accept and receive to published are high. This is logical because the majority of the publication cycle *is* the review process. The production process, on the other hand, has only a medium correlation with the entire publication cycle.

Other squared correlations are all small. The largest uncontrolled effect is 1% explained variance. Controlling for other explanatory variables is likely to have only little effect, because of low squared correlations between the explanatory variables.

This indicates the regression analyses will most likely indicate that the effects of the predictor variables will be small and the publication process will prove highly random in its duration.

### Linearity
Because correlations rest on the assumption of linearity, let us check the linearity to ensure that we are not jumping the gun with the previous section (should actually do this before but I forgot...)


```r
pairs(x = d[,-6])
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

The plots of the predictor variables on the publication variables indicate curvilinear relations. This means that a linear correlation is insufficient to model the relationship.

### Regression models
Before running the regression models, let me recapitulate what the previous analyses indicated:

1. Publication year as covariate
2. Analyze review process and production process separately
3. Linear and curvilinear estimates for predictor variables.
4. Check whether overall results hold for journals separately

Additionally, I will mean center the predictor variables `authors` and `pages` so intercept estimates are meaningful. All regression models will be Poisson regression, considering that number of days in each part of publication cycle is a count variable. Due to overdispersion in the data, Quasi-ML estimation will be used.


```r
dat$authors.centred <- as.numeric(dat$authors - mean(dat$authors, na.rm = TRUE))
dat$authors.centred.sq <- dat$authors.centred^2
dat$pages.centred <- as.numeric(dat$data.pagecount) - mean(as.numeric(dat$data.pagecount), na.rm = TRUE)
dat$pages.centred.sq <- dat$pages.centred^2

review_process <- glm(received_accepted ~ 
                        authors.centred +
                        authors.centred.sq + 
                        pages.centred + 
                        pages.centred.sq +
                        as.factor(year) + 
                        as.factor(coi),
                      data = dat,
                      family = "quasipoisson")

options(scipen = 5)
print(review_process)
```

```
## 
## Call:  glm(formula = received_accepted ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = dat)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##         4.187379085          0.001885353         -0.000007218  
##       pages.centred     pages.centred.sq  as.factor(year)2004  
##        -0.000934009         -0.000109952          0.687728621  
## as.factor(year)2005  as.factor(year)2006  as.factor(year)2007  
##         0.738653817          0.693650711          0.548916416  
## as.factor(year)2008  as.factor(year)2009  as.factor(year)2010  
##         0.619997156          0.592943351          0.658202470  
## as.factor(year)2011  as.factor(year)2012  as.factor(year)2013  
##         0.652439350          0.734696312          0.736832729  
## as.factor(year)2014  as.factor(year)2015   as.factor(coi)TRUE  
##         0.773077684          0.844193512         -0.018998220  
## 
## Degrees of Freedom: 137467 Total (i.e. Null);  137450 Residual
## Null Deviance:	    5673000 
## Residual Deviance: 5529000 	AIC: NA
```

```r
reviewdays <- exp(review_process$coefficients[1])
i = 2
for(j in 6:17){
  reviewdays[i] <- exp(review_process$coefficients[1] +
                         review_process$coefficients[j])
  i = i + 1}

production_process <- glm(accepted_published ~ 
                            authors.centred +
                            authors.centred.sq + 
                            pages.centred + 
                            pages.centred.sq +
                            as.factor(year) +
                            as.factor(coi),
                          data = dat,
                          family = "quasipoisson")

options(scipen = 5)
print(production_process)
```

```
## 
## Call:  glm(formula = accepted_published ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = dat)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          4.24956932           0.00591583          -0.00001488  
##       pages.centred     pages.centred.sq  as.factor(year)2004  
##          0.00004318          -0.00011172           0.10164675  
## as.factor(year)2005  as.factor(year)2006  as.factor(year)2007  
##         -0.12942828          -0.10928853          -0.46095013  
## as.factor(year)2008  as.factor(year)2009  as.factor(year)2010  
##         -0.57019109          -0.56629725          -0.66378719  
## as.factor(year)2011  as.factor(year)2012  as.factor(year)2013  
##         -0.56771495          -0.47268199          -0.37085600  
## as.factor(year)2014  as.factor(year)2015   as.factor(coi)TRUE  
##         -0.53778005          -0.24988634           0.00491181  
## 
## Degrees of Freedom: 137467 Total (i.e. Null);  137450 Residual
## Null Deviance:	    833700 
## Residual Deviance: 736100 	AIC: NA
```

```r
productiondays <- exp(production_process$coefficients[1])
i = 2
for(j in 6:17){
  productiondays[i] <- exp(production_process$coefficients[1] +
                             production_process$coefficients[j])
  i = i + 1}

plotdf <- data.frame(years = 2003:2015, revdays = reviewdays,
                     proddays = productiondays)

p1 <- ggplot(plotdf, aes(x = years, y = revdays)) +
  geom_point() + 
  stat_smooth(method = "loess", se = FALSE) +
  labs(list(x = "Year", y = "Estimated review time")) + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = 2003:2015)

p2 <- ggplot(plotdf, aes(x = years, y = proddays)) +
  geom_point() + 
  stat_smooth(method = "loess", se = FALSE) +
  labs(list(x = "Year", y = "Estimated production time")) + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = 2003:2015)

multiplot(p1, p2 + guides(colour = guide_legend(nrow = 3)))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 




```r
x <- ddply(dat, .(year, data.journal), function(x){
  data.frame(revdays = x$received_accepted,
             proddays = x$accepted_published,
             year = x$year,
             coi = x$coi,
             authors.centred = x$authors.centred,
             authors.centred.sq = x$authors.centred.sq,
             pages.centred = x$pages.centred,
             pages.centred.sq = x$pages.centred.sq)
})

plotdf <- data.frame(data.journal = NULL,
                     years = NULL,
                     revdays = NULL,
                     proddays = NULL)

for(journal in unique(x$data.journal)){
  # Selecting the data for one journal
  sel <- x[x$data.journal == journal, ]
  
  # Estimating the model for the journal in the rev process
  review_process <- glm(revdays ~ 
                          authors.centred +
                          authors.centred.sq + 
                          pages.centred + 
                          pages.centred.sq +
                          as.factor(coi) +
                          as.factor(year),
                        data = sel,
                        family = "quasipoisson")
  print((review_process))
  
  # Calculating the estimated days of review in the model
  reviewdays <- exp(review_process$coefficients[1])
  i = 2
  for(j in 7 : length(review_process$coefficients)){
    reviewdays[i] <- exp(review_process$coefficients[1] +
                           review_process$coefficients[j])
    i = i + 1}
  
  # Estimating the days of production in the model
  production_process <- glm(proddays ~ 
                              authors.centred +
                              authors.centred.sq + 
                              pages.centred + 
                              pages.centred.sq +
                              as.factor(year) +
                              as.factor(coi),
                            data = sel,
                            family = "quasipoisson")
  
  print((production_process))
  # Calculating the estimates per year
  productiondays <- exp(production_process$coefficients[1])
  i = 2
  for(j in 7 : length(production_process$coefficients)){
    productiondays[i] <- exp(production_process$coefficients[1] +
                               production_process$coefficients[j])
    i = i + 1}
  
  temp <- data.frame(data.journal = journal,
                     years = unique(sel$year),
                     revdays = reviewdays,
                     proddays = productiondays)
  
  # Saving out the data
  plotdf <- rbind(plotdf, temp)
}
```

```
## 
## Call:  glm(formula = revdays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(coi) + as.factor(year), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          4.11694165           0.00183624          -0.00001217  
##       pages.centred     pages.centred.sq   as.factor(coi)TRUE  
##          0.00249938          -0.00016616           0.00002218  
## as.factor(year)2004  as.factor(year)2005  as.factor(year)2006  
##          0.71403255           0.89456751           0.90460052  
## as.factor(year)2007  as.factor(year)2008  as.factor(year)2009  
##          1.15528807           1.05714109           1.13692298  
## as.factor(year)2010  as.factor(year)2011  as.factor(year)2012  
##          1.08368074           1.09508326           1.05699442  
## as.factor(year)2013  as.factor(year)2014  as.factor(year)2015  
##          1.09529349           0.93613224           0.95577527  
## 
## Degrees of Freedom: 2004 Total (i.e. Null);  1987 Residual
## Null Deviance:	    72570 
## Residual Deviance: 65330 	AIC: NA
## 
## Call:  glm(formula = proddays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          4.07244803          -0.00055656          -0.00001259  
##       pages.centred     pages.centred.sq  as.factor(year)2004  
##          0.00005685          -0.00004097           0.09593973  
## as.factor(year)2005  as.factor(year)2006  as.factor(year)2007  
##         -0.02502313          -0.00216962           0.04243833  
## as.factor(year)2008  as.factor(year)2009  as.factor(year)2010  
##         -0.24374524          -0.26749993          -0.31204595  
## as.factor(year)2011  as.factor(year)2012  as.factor(year)2013  
##         -0.29000522          -0.27001546          -0.30689055  
## as.factor(year)2014  as.factor(year)2015   as.factor(coi)TRUE  
##         -0.32081451          -0.32245318           0.11073458  
## 
## Degrees of Freedom: 2004 Total (i.e. Null);  1987 Residual
## Null Deviance:	    7688 
## Residual Deviance: 4592 	AIC: NA
## 
## Call:  glm(formula = revdays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(coi) + as.factor(year), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          4.50470815           0.00074942          -0.00002711  
##       pages.centred     pages.centred.sq   as.factor(coi)TRUE  
##          0.00083057          -0.00020866           0.03638316  
## as.factor(year)2005  as.factor(year)2006  as.factor(year)2007  
##          0.60856638           0.80556744           0.88026851  
## as.factor(year)2008  as.factor(year)2009  as.factor(year)2010  
##          0.91670421           1.12473927           0.82414906  
## as.factor(year)2011  as.factor(year)2012  as.factor(year)2013  
##          0.96224537           1.05791709           0.82152713  
## as.factor(year)2014  as.factor(year)2015  
##          0.81579391           0.97310684  
## 
## Degrees of Freedom: 1056 Total (i.e. Null);  1040 Residual
## Null Deviance:	    41160 
## Residual Deviance: 35570 	AIC: NA
## 
## Call:  glm(formula = proddays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##         4.227978181          0.000211564         -0.000009064  
##       pages.centred     pages.centred.sq  as.factor(year)2005  
##         0.000504044          0.000003046         -0.002780969  
## as.factor(year)2006  as.factor(year)2007  as.factor(year)2008  
##         0.209747318         -0.200954534         -0.270278523  
## as.factor(year)2009  as.factor(year)2010  as.factor(year)2011  
##        -0.402857473         -0.508820957         -0.444387179  
## as.factor(year)2012  as.factor(year)2013  as.factor(year)2014  
##        -0.449022375         -0.447800780         -0.456451182  
## as.factor(year)2015   as.factor(coi)TRUE  
##        -0.465600956          0.008333598  
## 
## Degrees of Freedom: 1056 Total (i.e. Null);  1040 Residual
## Null Deviance:	    6460 
## Residual Deviance: 2475 	AIC: NA
## 
## Call:  glm(formula = revdays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(coi) + as.factor(year), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##           4.8967586           -0.0026681           -0.0005642  
##       pages.centred     pages.centred.sq   as.factor(coi)TRUE  
##           0.0012405           -0.0003496           -0.0054980  
## as.factor(year)2006  as.factor(year)2007  as.factor(year)2008  
##           0.2432795            0.3435141            0.4869778  
## as.factor(year)2009  as.factor(year)2010  as.factor(year)2011  
##           0.4082567            0.4586731            0.3718249  
## as.factor(year)2012  as.factor(year)2013  as.factor(year)2014  
##           0.3359486            0.3934695            0.3857876  
## as.factor(year)2015  
##           0.3123629  
## 
## Degrees of Freedom: 3384 Total (i.e. Null);  3369 Residual
## Null Deviance:	    118700 
## Residual Deviance: 113000 	AIC: NA
## 
## Call:  glm(formula = proddays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##           3.7351050            0.0008529            0.0005100  
##       pages.centred     pages.centred.sq  as.factor(year)2006  
##           0.0004500           -0.0001047            0.1808127  
## as.factor(year)2007  as.factor(year)2008  as.factor(year)2009  
##           0.2378988            0.1023408           -0.0842979  
## as.factor(year)2010  as.factor(year)2011  as.factor(year)2012  
##          -0.1512043            0.3749224            0.3901254  
## as.factor(year)2013  as.factor(year)2014  as.factor(year)2015  
##           0.3543211            0.3831363            0.5924781  
##  as.factor(coi)TRUE  
##          -0.0146962  
## 
## Degrees of Freedom: 3384 Total (i.e. Null);  3369 Residual
## Null Deviance:	    20790 
## Residual Deviance: 12450 	AIC: NA
## 
## Call:  glm(formula = revdays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(coi) + as.factor(year), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##         4.759530203          0.000868954         -0.000006656  
##       pages.centred     pages.centred.sq   as.factor(coi)TRUE  
##         0.001731565         -0.000364188         -0.009072820  
## as.factor(year)2006  as.factor(year)2007  as.factor(year)2008  
##         0.239519003          0.181319810          0.384765236  
## as.factor(year)2009  as.factor(year)2010  as.factor(year)2011  
##         0.340389185          0.509333383          0.386127721  
## as.factor(year)2012  as.factor(year)2013  as.factor(year)2014  
##         0.374821631          0.462420160          0.465760986  
## as.factor(year)2015  
##         0.366564896  
## 
## Degrees of Freedom: 4675 Total (i.e. Null);  4660 Residual
## Null Deviance:	    173700 
## Residual Deviance: 164200 	AIC: NA
## 
## Call:  glm(formula = proddays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          3.93650038           0.00297663          -0.00000508  
##       pages.centred     pages.centred.sq  as.factor(year)2006  
##          0.00041195          -0.00003956           0.03723607  
## as.factor(year)2007  as.factor(year)2008  as.factor(year)2009  
##         -0.02286547          -0.30093929          -0.46625561  
## as.factor(year)2010  as.factor(year)2011  as.factor(year)2012  
##         -0.40044364           0.05057414           0.20592433  
## as.factor(year)2013  as.factor(year)2014  as.factor(year)2015  
##          0.16142727           0.22567714          -0.12118357  
##  as.factor(coi)TRUE  
##         -0.00580276  
## 
## Degrees of Freedom: 4675 Total (i.e. Null);  4660 Residual
## Null Deviance:	    30790 
## Residual Deviance: 17690 	AIC: NA
## 
## Call:  glm(formula = revdays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(coi) + as.factor(year), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          4.82756225           0.00775773          -0.00050023  
##       pages.centred     pages.centred.sq   as.factor(coi)TRUE  
##         -0.00008574          -0.00028716          -0.06294371  
## as.factor(year)2006  as.factor(year)2007  as.factor(year)2008  
##          0.18859917           0.18170095           0.36194702  
## as.factor(year)2009  as.factor(year)2010  as.factor(year)2011  
##          0.33780700           0.41887006           0.30041897  
## as.factor(year)2012  as.factor(year)2013  as.factor(year)2014  
##          0.27488323           0.35136502           0.32158604  
## as.factor(year)2015  
##          0.25958544  
## 
## Degrees of Freedom: 3925 Total (i.e. Null);  3910 Residual
## Null Deviance:	    132900 
## Residual Deviance: 127500 	AIC: NA
## 
## Call:  glm(formula = proddays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          3.90922497           0.00147421           0.00008953  
##       pages.centred     pages.centred.sq  as.factor(year)2006  
##         -0.00004401           0.00001657          -0.12761976  
## as.factor(year)2007  as.factor(year)2008  as.factor(year)2009  
##         -0.00981846          -0.40749207          -0.55331450  
## as.factor(year)2010  as.factor(year)2011  as.factor(year)2012  
##         -0.42387711           0.08510586          -0.00578096  
## as.factor(year)2013  as.factor(year)2014  as.factor(year)2015  
##          0.07411762          -0.03106040          -0.23120524  
##  as.factor(coi)TRUE  
##         -0.00799686  
## 
## Degrees of Freedom: 3925 Total (i.e. Null);  3910 Residual
## Null Deviance:	    18550 
## Residual Deviance: 10270 	AIC: NA
## 
## Call:  glm(formula = revdays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(coi) + as.factor(year), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##           5.2769911           -0.0030499           -0.0011882  
##       pages.centred     pages.centred.sq   as.factor(coi)TRUE  
##           0.0058133           -0.0007946            0.1227862  
## as.factor(year)2007  
##           0.3201423  
## 
## Degrees of Freedom: 43 Total (i.e. Null);  37 Residual
## Null Deviance:	    836.8 
## Residual Deviance: 603.8 	AIC: NA
## 
## Call:  glm(formula = proddays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          3.85762582           0.00523503           0.00006294  
##       pages.centred     pages.centred.sq  as.factor(year)2007  
##         -0.00036135           0.00007648           0.13825123  
##  as.factor(coi)TRUE  
##          0.03347334  
## 
## Degrees of Freedom: 43 Total (i.e. Null);  37 Residual
## Null Deviance:	    96.94 
## Residual Deviance: 83.86 	AIC: NA
## 
## Call:  glm(formula = revdays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(coi) + as.factor(year), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##         3.619016931          0.000610472         -0.000002031  
##       pages.centred     pages.centred.sq   as.factor(coi)TRUE  
##        -0.000804018         -0.000048886         -0.010399452  
## as.factor(year)2007  as.factor(year)2008  as.factor(year)2009  
##         0.718244524          0.942025039          0.970712782  
## as.factor(year)2010  as.factor(year)2011  as.factor(year)2012  
##         1.074679964          1.134877924          1.244470231  
## as.factor(year)2013  as.factor(year)2014  as.factor(year)2015  
##         1.238048336          1.287880175          1.387457080  
## 
## Degrees of Freedom: 119434 Total (i.e. Null);  119420 Residual
## Null Deviance:	    4894000 
## Residual Deviance: 4683000 	AIC: NA
## 
## Call:  glm(formula = proddays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          4.13948013           0.00582869          -0.00001658  
##       pages.centred     pages.centred.sq  as.factor(year)2007  
##          0.00037649          -0.00008122          -0.66381655  
## as.factor(year)2008  as.factor(year)2009  as.factor(year)2010  
##         -0.52489551          -0.44203940          -0.59241461  
## as.factor(year)2011  as.factor(year)2012  as.factor(year)2013  
##         -0.54539601          -0.42241762          -0.30375622  
## as.factor(year)2014  as.factor(year)2015   as.factor(coi)TRUE  
##         -0.48949999          -0.13868700           0.02110927  
## 
## Degrees of Freedom: 119434 Total (i.e. Null);  119420 Residual
## Null Deviance:	    695700 
## Residual Deviance: 601800 	AIC: NA
## 
## Call:  glm(formula = revdays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(coi) + as.factor(year), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          4.73623522           0.00504142          -0.00011078  
##       pages.centred     pages.centred.sq   as.factor(coi)TRUE  
##         -0.00064066           0.00006489           0.03309152  
## as.factor(year)2008  as.factor(year)2009  as.factor(year)2010  
##          0.35252263           0.15628949           0.27560221  
## as.factor(year)2011  as.factor(year)2012  as.factor(year)2013  
##          0.24380663           0.23596354           0.26013658  
## as.factor(year)2014  as.factor(year)2015  
##          0.21299259           0.17111324  
## 
## Degrees of Freedom: 2939 Total (i.e. Null);  2926 Residual
## Null Deviance:	    114000 
## Residual Deviance: 112700 	AIC: NA
## 
## Call:  glm(formula = proddays ~ authors.centred + authors.centred.sq + 
##     pages.centred + pages.centred.sq + as.factor(year) + as.factor(coi), 
##     family = "quasipoisson", data = sel)
## 
## Coefficients:
##         (Intercept)      authors.centred   authors.centred.sq  
##          4.61852593           0.00082814           0.00014587  
##       pages.centred     pages.centred.sq  as.factor(year)2008  
##          0.00001438           0.00002480          -0.97747211  
## as.factor(year)2009  as.factor(year)2010  as.factor(year)2011  
##         -1.18385710          -0.91642789          -0.68380228  
## as.factor(year)2012  as.factor(year)2013  as.factor(year)2014  
##         -0.66634555          -0.78517089          -0.67741432  
## as.factor(year)2015   as.factor(coi)TRUE  
##         -0.82580873           0.00910202  
## 
## Degrees of Freedom: 2939 Total (i.e. Null);  2926 Residual
## Null Deviance:	    18390 
## Residual Deviance: 14740 	AIC: NA
```

```r
plotdf$data.journal <- as.factor(plotdf$data.journal)

p1 <- ggplot(plotdf, aes(x = years, y = revdays, colour = data.journal, group = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "loess", se = FALSE) + 
  labs(list(x = "Year", y = "Estimated mean review days")) + 
  xlim(c(2003, 2015)) + 
  ylim(c(0, 225)) +
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = 2003:2015)
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will replace the existing scale.
```

```r
# Loess curves kept giving errors here
# Use LM curves instead...
p2 <- ggplot(plotdf, aes(x = years, y = proddays, colour = data.journal, group = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) + 
  labs(list(x = "Year", y = "Estimated mean review days")) + 
  xlim(c(2003, 2015)) + 
  ylim(c(0, 225)) + 
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 2003:2015)
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which will replace the existing scale.
```

```r
multiplot(p1, p2 + guides(colour = guide_legend(nrow = 3)))
```

```
## Warning in loop_apply(n, do.ply): Removed 5 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 6 rows containing missing values
## (geom_point).
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
