# Publication lag in academia: A case study of the Public Library of Science (PLOS)

This repository is linked to [this](https://www.authorea.com/users/2013/articles/36067/_show_article) Authorea manuscript. This README file contains the main analyses including `R` code, and is included in a separate file in the `archive` folder called `data_analysis.r`. This README document provides the framework for the manuscript.

Note that the dataset includes *all* published papers in PLOS. Considering this as a case study of PLOS research articles, the entire population is included in the analyses. This makes inferences and p-value hypothesis testing unnecessary.



```r
setwd("D:/Dropbox/projects/2015lag/archive")

# Dependencies
library(stringr)
library(ggplot2)
library(knitr)

# Read in data
dat <- read.csv(file = '20150514collected_data.csv')

#################
# Data cleaning #
#################

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
dat$submitted <- as.Date(substring(dat$data.received_date,
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
dim(dat)[1] - sum(!is.na(dat$submitted) &
                    !is.na(dat$accepted) &
                    !is.na(dat$published))
```

```
## [1] 10
```

```r
dat <- dat[!is.na(dat$submitted) &
             !is.na(dat$accepted) &
             !is.na(dat$published), ]
```

The data was cleaned in several steps, but the data were also inspected for some illogical dates, where the paper was published before it was accepted (i.e., `{r} sum(dat$published < dat$accepted)` cases) or accepted before submitted (i.e., `{r} sum(dat$accepted < dat$submitted` cases). Below I extend on this and give the DOIs for those papers


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

# Accepted before submitted
as.matrix(dat$data.id[dat$accepted < dat$submitted])
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
dat <- dat[!dat$accepted < dat$submitted, ]

#################
```




```r
# Calculating the days between submitted, accepted, and published
calc_days <- Vectorize(function(a, b) 
  length(seq(a, b, "days"))) 

# submitted_accepted <- calc_days(dat$submitted, dat$accepted)
# save(submitted_accepted, file = "submitted_accepted")
# 
# accepted_published <- calc_days(dat$accepted, dat$ published)
# save(accepted_published, file = "accepted_published")

# Load the objects created in the commented out code above
# Decreases runtime
load("submitted_accepted")
load("accepted_published")
```
