library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(gutenbergr)
library(stringr)
library(tidyr)
library(scales)

#?# get data AssociatedPress
data("AssociatedPress")
AssociatedPress

#?# create ap_lda: conduct LDA with k=2
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

#?# create ap_topics: tidy beta matrix
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

#?# create ap_top_terms: get top 10 words that define the two topics
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#?# plot the words
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#?# find words making greatest difference between topic1 and topic2
#?#   difference defined as log2(topic2/topic1)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

rbind(beta_spread %>% top_n(10, log_ratio),
      beta_spread %>% top_n(10, -log_ratio)) %>% 
  arrange(log_ratio) %>% 
  mutate(term=reorder(term, log_ratio), topic=factor(ifelse(log_ratio>0,1,2))) %>% 
  ggplot(aes(term, log_ratio, fill=topic)) +
  geom_col(show.legend=TRUE) +
  coord_flip()


### 6.1.2 Document-topic probabilities
#?# create ap_documents: from ap_lda get tidy version of gamma matrix 
#?#   gamma matrix: per-document-per-topic probability
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

#?# check why document 6 has high topic 2 probability
tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))



### 6.2 Example: the great library heist
#_# titles:
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")
#__#

#?# create books: download from gutenberg:
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

#?# create by_chapter: divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

#?# create by_chapter_word: split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

#?# create word_counts: word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

### 6.2.1 LDA on chapters
#?# create chapters_dtm: get dtm from word_counts
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

#?# create chapters_lda: LDA with k=4
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

#?# create chapter_topics: pull out beta matrix for the topics
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

#?# get top 5 terms by topics and plot
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


### 6.2.2 Per-document classification
#?# create chapters_gamma: pull out gamm matrix
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

#?# summarize gamma by book and topic and do classification
chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

#?# check the cases of misclassification
chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)


###6.2.3 By word assignments: augment

#?# create assignments: assign words in chapters_lda into each topic
assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

#?# update assignments: check correctness of assignments
assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments

#?# plot confusion matrix: how often the misclassification
assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")


#?# research on the wrong words
wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

word_counts %>%
  filter(word == "flopson")
wrong_words

###6.3 Alternative LDA implementations

#Use package mallet
library(mallet)

# create a vector with one string per chapter
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

# word-topic pairs
tidy(mallet_model)

# document-topic pairs
tidy(mallet_model, matrix = "gamma")

# column needs to be named "term" for "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)



