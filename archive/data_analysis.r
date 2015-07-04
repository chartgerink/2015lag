setwd("D:/Dropbox/projects/2015lag/archive")

# Dependencies
library(stringr)
library(ggplot2)
library(knitr)
library(plyr)
source('multiplot.r')

# Read in data
dat <- read.csv(file = '20150704collected_data.csv')

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

# Checking for illogical dates
# Published before accepted
as.matrix(dat$data.id[dat$published < dat$accepted])
dat <- dat[!dat$published < dat$accepted, ]

# Accepted before received
as.matrix(dat$data.id[dat$accepted < dat$received])
dat <- dat[!dat$accepted < dat$received, ]

# Selecting out those papers which have equivalent
# received - accepted
as.matrix(dat$data.id[dat$received == dat$accepted])
dat <- dat[!dat$received == dat$accepted, ]

print(dim(dat)[1])
print(table(dat$data.journal))

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

median(dat$received_published)
median(dat$received_accepted)
median(dat$accepted_published) 

x <- ddply(.data = dat, .(data.journal),
           .fun = function(x) summary(x$received_published))
x[order(x$Median),]

x <- ddply(.data = dat, .(data.journal),
           .fun = function(x) summary(x$received_accepted))
x[order(x$Median),]

x <- ddply(.data = dat, .(data.journal),
           .fun = function(x) summary(x$accepted_published))
x[order(x$Median),]

x <- ddply(dat, .(data.journal, year), function(x) summary(x$received_published))

ggplot(x, aes(x = year, y = Median, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(list(title = "Full publication cycle", x = "Median days", y = "Years")) +
  xlim(c(2003, 2015)) + 
  ylim(c(0, 250))

x <- ddply(dat, .(data.journal, year), function(x) summary(x$received_accepted))

p1 <- ggplot(x, aes(x = year, y = Median, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(list(title = "Review process", x = "Median days", y = "Years")) +
  xlim(c(2003, 2015)) + 
  ylim(c(0, 250)) + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks=2003:2015)

x <- ddply(dat, .(data.journal, year), function(x) summary(x$accepted_published))

p2 <- ggplot(x, aes(x = year, y = Median, colour = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(list(title = "Production process", x = "Median days", y = "Years")) +
  xlim(c(2003, 2015)) + 
  ylim(c(0, 250)) + 
  theme(legend.position = "top") + 
  scale_x_continuous(breaks = 2003:2015)

pdf('observed review and production times.pdf',
    width = 7.5, height = 8.75)
multiplot(p1, p2 + guides(colour = guide_legend(nrow = 3)))
dev.off()

d <- data.frame(receive_accept = as.numeric(received_accepted),
                accept_publish = as.numeric(accepted_published),
                receive_publish = as.numeric(received_published),
                authors = as.numeric(dat$authors),
                pages = as.numeric(dat$data.pagecount), 
                years = dat$year)
cor(d)
cor(d)^2

pairs(x = d[,-6])

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
  labs(list(x = "Year", y = "Estimated review time in days")) + 
  theme(legend.position = "none") + 
  ylim(c(0, 250)) + 
  scale_x_continuous(breaks = 2003:2015)

p2 <- ggplot(plotdf, aes(x = years, y = proddays)) +
  geom_point() + 
  stat_smooth(method = "loess", se = FALSE) +
  labs(list(x = "Year", y = "Estimated production time in days")) + 
  theme(legend.position = "none") + 
  ylim(c(0, 250)) + 
  scale_x_continuous(breaks = 2003:2015)

pdf('Fig1.pdf',
    width = 7.5, height = 8.75)
multiplot(p1, p2 + guides(colour = guide_legend(nrow = 3)))
dev.off()


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

plotdf$data.journal <- as.factor(plotdf$data.journal)

p1 <- ggplot(plotdf, aes(x = years, y = revdays, colour = data.journal, group = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "loess", se = FALSE) + 
  labs(list(x = "Year", y = "Estimated mean review days")) + 
  xlim(c(2003, 2015)) + 
  ylim(c(0, 250)) +
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = 2003:2015)

# Loess curves kept giving errors here
# Use LM curves instead...
p2 <- ggplot(plotdf, aes(x = years, y = proddays, colour = data.journal, group = data.journal)) +
  geom_point(aes(col = data.journal)) + 
  stat_smooth(method = "lm", se = FALSE) + 
  labs(list(x = "Year", y = "Estimated mean production days")) + 
  xlim(c(2003, 2015)) + 
  ylim(c(0, 250)) + 
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 2003:2015)

pdf('Fig2.pdf',
    width = 7.5, height = 8.75)
multiplot(p1, p2 + guides(colour = guide_legend(nrow = 3)))
dev.off()
