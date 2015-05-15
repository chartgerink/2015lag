setwd("D:/Dropbox/projects/2015lag")

# Dependencies
library(stringr)
library(ggplot2)

# Read in data
dat <- read.csv(file = 'archive/20150514collected_data.csv')

#################
# Data cleaning #
#################

# Assimilate journal naming
# See discrepancies
table(dat$data.journal)
# Most due to using PLoS and PLOS

# Solve this by making all journal names lowercase
dat$data.journal <- tolower(dat$data.journal)
table(dat$data.journal)

# plos medicin = typo
dat$data.journal[dat$data.journal == "plos medicin"] <- "plos medicine"

# remove the journals with "none"
dat <- dat[!dat$data.journal == "none", ]
table(dat$data.journal)

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

dat <- dat[!is.na(dat$submitted) &
             !is.na(dat$accepted) &
             !is.na(dat$published), ]

# Checking for illogical dates
# Published before accepted
sum(dat$published < dat$accepted)
dat <- dat[!dat$published < dat$accepted, ]

# Accepted before submitted
sum(dat$accepted < dat$submitted)
dat <- dat[!dat$accepted < dat$submitted, ]

#################

# Calculating the days between submitted, accepted, and published
calc_days <- Vectorize(function(a, b) 
  length(seq(a, b, "days"))) 

submitted_accepted <- calc_days(dat$submitted, dat$accepted)
save(submitted_accepted, file = "archive/submitted_accepted")

accepted_published <- calc_days(dat$accepted, dat$ published)
save(accepted_published, file = "archive/accepted_published")

# Load the objects created in the commented out code above
# Decreases runtime
load("archive/submitted_accepted")
load("archive/accepted_published")
