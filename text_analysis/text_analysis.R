# Text Analysis
rm(list=ls())

# String: Text can, of course, be stored as strings, i.e., character vectors, within R, and often text data is first read into memory in this form.
# Corpus: These types of objects typically contain raw strings annotated with additional metadata and details.
# Document-term matrix: This is a sparse matrix describing a collection (i.e., a corpus) of documents with one row for each document and one column for each term. 
# The value in the matrix is typically word count or tf-idf.

install.load::install_load('janeaustenr', 'dplyr','stringr','tidytext','ggplot2','wordcloud','RColorBrewer','wordcloud2','tm', 'gutenbergr','scales','textdata')

View(austen_books())

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

View(original_books)

# one-token-per-row format
# This function uses the tokenizers package to separate each line of text in the original data frame into tokens. 
tidy_books <- original_books %>%
  unnest_tokens(word, text)

View(tidy_books)

# stopwrods
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

View(tidy_books)

tidy_books %>%
  count(word, sort = TRUE) 

View(tidy_books)

# graphs
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

barplot


# sentiment analysis
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

View(tidy_books)

get_sentiments("nrc")
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

View(tidy_books)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  tidyr::spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

View(jane_austen_sentiment)
