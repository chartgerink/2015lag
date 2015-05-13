if(!require(rplos)){install.packages('rplos')}
library(rplos)

# The query
que <- "*.*"

# The selection in PLOS
sel <- list('cross_published_journal_key:PLoSONE',
     'doc_type:full',
     'article_type:"Research Article"')

# The fields to be retrieved from the API
ids <- c("accepted_date",
  "article_type",
  "author",
  "competing_interest",
  "counter_total_all",
  "counter_total_month",
  "editor",
  "pagecount",
  "publication_date",
  "received_date")

# Nr. of hits to set as limit when collecting data.
hits <- searchplos(q = que,
                   fl = 'id', 
                   fq = sel,
                   start=0,
                   limit=1)$meta$numFound


search_results <- searchplos(q = que,
                             fl = ids, 
                             fq = sel,
                             start=0,
                             limit=hits)

write.csv(search_results, '20150513collected_data')