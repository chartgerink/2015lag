setwd("D:/Dropbox/projects/2015lag")

# Dependencies
library(stringr)
library(ggplot2)

# Read in data
dat <- read.csv(file = '20150514collected_data.csv')

# Making the dates readable
dat$submitted <- as.Date(substring(dat$data.received_date, first = 0, last = 10))
dat$accepted <- as.Date(substring(dat$data.accepted_date, first = 0, last = 10))
dat$published <- as.Date(substring(dat$data.publication_date, first = 0, last = 10))

# Listwise deletion
# Nr of rows
dim(dat)[1] - sum(!is.na(dat$submitted) &
                    !is.na(dat$accepted) &
                    !is.na(dat$published))

dat <- dat[!is.na(dat$submitted) &
             !is.na(dat$accepted) &
             !is.na(dat$published),]

# Calculating the days between submitted, accepted, and published
calc_days <- Vectorize(function(a, b) 
  seq(a, b, "days") - 2) 
# Minus 2 two subtract submission day, accept day, and publication day 

submitted_accepted <- calc_days(dat$submitted, dat$accepted)

accepted_ published <- calc_days(dat$accepted, dat$ published)
