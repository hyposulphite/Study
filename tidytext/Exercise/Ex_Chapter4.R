
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(igraph)
library(ggraph)
library(gutenbergr)
library(stringr)
library(widyr)

###4.1 Tokenizing by n-gram
#?# create austen_bigrams: tokenized by bigram
austen_bigrams = austen_books() %>% 
  unnest_tokens(bigram, text, token="ngrams", n=2)

###4.1.1 Counting and filtering n-grams
#?# austen_bigrams count words
austen_bigrams %>% 
  count(bigram, sort=TRUE)

#?# create bigrams_separated: austen_bigrams separated bigrams
bigrams_separated = austen_bigrams %>% 
  separate(col=bigram, into=c("word1","word2"), sep=" ")

#?# create bigrams_filtered: bigrams_separated neither words in stop_words
bigrams_filtered = bigrams_separated %>% 
  anti_join(stop_words %>% rename(word1=word)) %>% 
  anti_join(stop_words %>% rename(word2=word))

#?#   1. Create bigrams_counts as count from bigrams_filtered
bigrams_counts = bigrams_filtered %>% 
  count(word1, word2, sort=TRUE)

#?#   2. Also count and unite the bigrams
bigrams_united = bigrams_filtered %>% 
  unite(col=bigram, word1, word2, sep=" ")

#?# word count by trigram
austen_books() %>% 
  unnest_tokens(trigram, text, token="ngrams", n=3) %>% 
  count(trigram, sort=TRUE)

###4.1.2 Analyzing bigrams
#?# from bigrams_filtered, check word count for XX street, create bigrams_tf_idf, and plot
bigrams_filtered %>% 
  filter(word2=="street") %>% 
  count(book, word1, sort=TRUE)

bigrams_tf_idf = bigrams_filtered %>% 
  unite(col=bigram, word1, word2, sep=" ") %>% 
  count(book, bigram) %>% 
  bind_tf_idf(bigram, book, n) %>% 
  arrange(-tf_idf)

  

###4.1.3 Using bigrams to provide context in sentiment analysis
#?# Plot The most common positive or negative words to follow negations 
#?##   such as ‘never’, ‘no’, ‘not’, and ‘without’, by negation words
bigrams_separated %>% 
  filter(word1 %in% c("never","no","not","without")) %>% 
  inner_join(get_sentiments("bing"), by=c("word2"="word")) %>% 
  count(word2, sentiment, sort=TRUE) %>% 
  ungroup() %>% 
  group_by(sentiment) %>% 
  top_n(n=10, wt=n) %>% 
  mutate(word2=reorder(word2, n)) %>% 
  ggplot(aes(word2, n, fill=sentiment)) +
  geom_bar(stat="identity") +
  facet_wrap(~sentiment, scale="free") +
  coord_flip()

###4.1.4 Visualizing a network of bigrams with igraph
#?# This section: check original code and study
bigrams_counts

bigrams_graph = bigrams_counts %>% 
  ungroup() %>% 
  top_n(60, n) %>% 
  graph_from_data_frame()

ggraph(bigrams_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)




###

###4.2 Counting and correlating pairs of words with the widyr package
###4.2.1 Counting and correlating among sections
#?# create austen_section_words: austen_books filtered by "Pride & Prejudice"
#?#   section=10 rows block and filter out stopwords
austen_section_words = austen_books() %>% 
  unnest_tokens(word, text) %>% 
  filter(book=="Pride & Prejudice") %>% 
  mutate(section = row_number() %/% 10) %>% 
  anti_join(stop_words)

#?# create word_pairs: count words co-occuring within sections
word_pairs = austen_section_words %>% 
  pairwise_count(word, section, sort=TRUE)

###4.2.2 Pairwise correlation
#?# create word_cors: from austen_section_words,
#?#   filter common words (count>20) and get pairwise correlation
word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

#?# plot words having highest correlation with ("elizabeth", "pounds", "married", "pride")
