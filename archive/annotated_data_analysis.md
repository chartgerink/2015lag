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
```

```
## Error in `$<-.data.frame`(`*tmp*`, "received_accepted", value = c(96L, : replacement has 135422 rows, data has 137468
```

```r
load("accepted_published")
dat$accepted_published <- accepted_published
```

```
## Error in `$<-.data.frame`(`*tmp*`, "accepted_published", value = c(42L, : replacement has 135422 rows, data has 137468
```

```r
load("received_published")
dat$received_published <- received_published
```

```
## Error in `$<-.data.frame`(`*tmp*`, "received_published", value = c(137L, : replacement has 135422 rows, data has 137468
```

## Data analysis
Prior to data analysis I stress once again that the data *is* the population of PLOS research articles and that making inferences based on p-value hypothesis testing therefore makes no sense. I will therefore eliminate all hypothesis testing statistics and limit myself to point estimates.

### Descriptives
The median full publication cycle is  days, with the majority of this being the review process (i.e.,  days) and not the production process (i.e.,  days). When we split this per journal, we see the following publication cycle.

```r
x <- ddply(.data = dat, .(data.journal),
           .fun = function(x) summary(x$received_published))
x[order(x$Median),]
```

```
## Error in order(x$Median): argument 1 is not a vector
```
which, when split up into the review process and production process looks as follows

```r
x <- ddply(.data = dat, .(data.journal),
           .fun = function(x) summary(x$received_accepted))
x[order(x$Median),]
```

```
## Error in order(x$Median): argument 1 is not a vector
```

```r
x <- ddply(.data = dat, .(data.journal),
           .fun = function(x) summary(x$accepted_published))
x[order(x$Median),]
```

```
## Error in order(x$Median): argument 1 is not a vector
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
## Error in eval(expr, envir, enclos): object 'Median' not found
```

These plots indicate that publication cycles have increased in length, except for Neglected Tropical Diseases, which shows a decreasing trend. Clinical trials was only published in 2006 and 2007, after which it was discontinued and rolled into PLOS ONE. Considering that the review process and production process are substantively different, it makes sense to investigate whether these trends differ across these parts of the publication cycle.


```r
x <- ddply(dat, .(data.journal, year), function(x) summary(x$received_accepted))

ggplot(x, aes(x = year, y = Median, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(list(title = "Review process", x = "Median days", y = "Years")) +
  xlim(c(2003, 2015)) + 
  ylim(c(0, 225))
```

```
## Error in eval(expr, envir, enclos): object 'Median' not found
```

The trends for the review process seem highly similar to the overall. Considering that the review process takes up the largest part of the entire publication cycle (i.e.,  days of the full  days), it makes sense that the trends for the full publication cycle are mostly made up of the trends in the review process.


```r
x <- ddply(dat, .(data.journal, year), function(x) summary(x$accepted_published))

ggplot(x, aes(x = year, y = Median, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(list(title = "Production process", x = "Median days", y = "Years")) +
  xlim(c(2003, 2015)) + 
  ylim(c(0, 225))
```

```
## Error in eval(expr, envir, enclos): object 'Median' not found
```

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
```

```
## Error in data.frame(receive_accept = as.numeric(received_accepted), accept_publish = as.numeric(accepted_published), : arguments imply differing number of rows: 135422, 137468
```

```r
cor(d)^2
```

```
## Error in is.data.frame(x): object 'd' not found
```

Here we see that squared correlations between the days from receive to accept and receive to published are high. This is logical because the majority of the publication cycle *is* the review process. The production process, on the other hand, has only a medium correlation with the entire publication cycle.

Other squared correlations are all small. The largest uncontrolled effect is 1% explained variance. Controlling for other explanatory variables is likely to have only little effect, because of low squared correlations between the explanatory variables.

This indicates the regression analyses will most likely indicate that the effects of the predictor variables will be small and the publication process will prove highly random in its duration.

### Linearity
Because correlations rest on the assumption of linearity, let us check the linearity to ensure that we are not jumping the gun with the previous section (should actually do this before but I forgot...)


```r
pairs(x = d[,-6])
```

```
## Error in pairs(x = d[, -6]): object 'd' not found
```

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
```

```
## Error in model.frame.default(formula = received_accepted ~ authors.centred + : variable lengths differ (found for 'authors.centred')
```

```r
options(scipen = 5)
summary(review_process)
```

```
## Error in summary(review_process): object 'review_process' not found
```

```r
reviewdays <- exp(review_process$coefficients[1])
```

```
## Error in eval(expr, envir, enclos): object 'review_process' not found
```

```r
i = 2
for(j in 6:17){
  reviewdays[i] <- exp(review_process$coefficients[1] +
                         review_process$coefficients[j])
  i = i + 1}
```

```
## Error: object 'review_process' not found
```

```r
production_process <- glm(accepted_published ~ 
                            authors.centred +
                            authors.centred.sq + 
                            pages.centred + 
                            pages.centred.sq +
                            as.factor(year) +
                            as.factor(coi),
                          data = dat,
                          family = "quasipoisson")
```

```
## Error in model.frame.default(formula = accepted_published ~ authors.centred + : variable lengths differ (found for 'authors.centred')
```

```r
options(scipen = 5)
summary(production_process)
```

```
## Error in summary(production_process): object 'production_process' not found
```

```r
productiondays <- exp(production_process$coefficients[1])
```

```
## Error in eval(expr, envir, enclos): object 'production_process' not found
```

```r
i = 2
for(j in 6:17){
  productiondays[i] <- exp(production_process$coefficients[1] +
                             production_process$coefficients[j])
  i = i + 1}
```

```
## Error: object 'production_process' not found
```

```r
plotdf <- data.frame(years = 2003:2015, revdays = reviewdays,
                     proddays = productiondays)
```

```
## Error in data.frame(years = 2003:2015, revdays = reviewdays, proddays = productiondays): object 'reviewdays' not found
```

```r
p1 <- ggplot(plotdf, aes(x = years, y = revdays)) +
  geom_point() + 
  stat_smooth(method = "loess", se = FALSE) +
  labs(list(x = "Year", y = "Days to review"))
```

```
## Error in ggplot(plotdf, aes(x = years, y = revdays)): object 'plotdf' not found
```

```r
p2 <- ggplot(plotdf, aes(x = years, y = proddays)) +
  geom_point() + 
  stat_smooth(method = "loess", se = FALSE) +
  labs(list(x = "Year", y = "Production days"))
```

```
## Error in ggplot(plotdf, aes(x = years, y = proddays)): object 'plotdf' not found
```

```r
multiplot(p1, p2)
```

```
## Error in multiplot(p1, p2): object 'p1' not found
```




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
```

```
## Error in eval(expr, envir, enclos): arguments imply differing number of rows: 0, 33
```

```r
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
  print(summary(review_process))
  
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
  
  print(summary(production_process))
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
## Error in eval(expr, envir, enclos): object 'revdays' not found
```

```r
plotdf$data.journal <- as.factor(plotdf$data.journal)

p1 <- ggplot(plotdf, aes(x = years, y = revdays, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) + 
  labs(list(x = "Year", y = "Estimated mean review days")) + 
  xlim(c(2003, 2015)) + 
  ylim(c(0, 225))

p2 <- ggplot(plotdf, aes(x = years, y = proddays, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) + 
  labs(list(x = "Year", y = "Estimated mean review days")) + 
  xlim(c(2003, 2015)) + 
  ylim(c(0, 225))

multiplot(p1, p2)
```

```
## Error in eval(expr, envir, enclos): object 'years' not found
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
