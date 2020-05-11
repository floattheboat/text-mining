library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(Matrix)
library(janeaustenr)
library(tm.plugin.webmining)
library(purrr)





data("AssociatedPress", package = "topicmodels")
AssociatedPress




terms <- Terms(AssociatedPress)
head(terms)





ap_td <- tidy(AssociatedPress)
ap_td


ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments




ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()


data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)


inaug_dfm

inaug_td <- tidy(inaug_dfm)
inaug_td


inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf



year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")


ap_td %>%
  cast_dtm(document, term, count)


ap_td %>%
  cast_dfm(document, term, count)





# cast into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)

dim(m)





austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm




data("acq")
acq

# first document
acq[[1]]

acq_td <- tidy(acq)
acq_td


acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)


# tf-idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))


#### broken below this comment ####

# for some reason it can't run the document download


download_articles <- function(symbol) {
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}

company <- c(#"Microsoft"
             #, 
  "Apple"
             #, "Google"
             #, "Amazon"
             #, "Facebook"
             #, "Twitter"
             #, "IBM"
             #, "Yahoo"
             #, "Netflix"
             )
symbol <- c(#"MSFT"
            #, 
  "AAPL"
            #, "GOOG"
            #, "AMZN"
            #, "FB"
            #, "TWTR"
            #, "IBM"
            #, "YHOO"
            #, "NFLX"
            )

stock_articles <- tibble(company = company,
                         symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))













