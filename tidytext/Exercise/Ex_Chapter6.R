
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(gutenbergr)
library(stringr)
library(tidyr)
library(scales)

#?# get data AssociatedPress
data(AssociatedPress)

#?# create ap_lda: conduct LDA with k=2
ap_lda = LDA(AssociatedPress, k=2, control=list(seed=1234))

#?# create ap_topics: tidy beta matrix
ap_topics = tidy(ap_lda, matrix="beta")

#?# create ap_top_terms: get top 10 words that define the two topics
ap_top_terms = ap_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

#?# plot the words
ap_top_terms %>% 
  mutate(term=reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill=as.factor(topic))) +
  geom_bar(stat="identity") +
  facet_wrap(~topic, scale="free") +
  coord_flip()

#?# find words making greatest difference between topic1 and topic2
#?#   difference defined as log2(topic2/topic1)
ap_topics %>% 
  mutate(topic=ifelse(topic==1, "topic1", "topic2")) %>% 
  spread(key=topic, value=beta, fill=0) %>% 
  filter(topic1 > 0.001 | topic2 > 0.001) %>% 
  mutate(log_ratio = log(topic1/topic2), sign=log_ratio>0) %>% 
  group_by(sign) %>% 
  top_n(10, abs(log_ratio)) %>% 
  arrange(sign, -abs(log_ratio))



### 6.1.2 Document-topic probabilities
#?# create ap_documents: from ap_lda get tidy version of gamma matrix 
#?#   gamma matrix: per-document-per-topic probability
ap_documents = tidy(ap_lda, matrix="gamma")
ap_documents

#?# check why document 6 has high topic 2 probability
tidy(AssociatedPress) %>% 
  filter(document==6) %>% 
  arrange(-count)

### 6.2 Example: the great library heist
#_# titles:
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

#?# create books: download from gutenberg:
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

#?# create by_chapter: divide into documents, each representing one chapter
by_chapter = books %>% 
  group_by(gutenberg_id) %>% 
  mutate(chapter=cumsum(str_detect(text, regex("^chapter", ignore_case=TRUE)))) %>% 
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

#?# create by_chapter_word: split into words
by_chapter_word = by_chapter %>% 
  unnest_tokens(word, text)
  
#?# create word_counts: word counts
word_counts = by_chapter_word %>% 
  anti_join(stop_words) %>% 
  group_by(document) %>% 
  count(word, sort=TRUE) %>% 
  ungroup()
word_counts

### 6.2.1 LDA on chapters
#?# create chapters_dtm: get dtm from word_counts
chapters_dtm = word_counts %>% 
  cast_dtm(document, word, n)

#?# create chapters_lda: LDA with k=4
chapters_lda = LDA(chapters_dtm, k=4, control=list(seed=1234))
chapters_lda

#?# create chapter_topics: pull out beta matrix for the topics
chapter_topics = tidy(chapters_lda, matrix="beta")

#?# get top 5 terms by topics and plot
chapter_topics %>% 
  group_by(topic) %>% 
  top_n(5, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(term=reorder(term, beta), topic=as.factor(topic)) %>% 
  ggplot(aes(term, beta, fill=topic)) +
  geom_bar(stat="identity") +
  facet_wrap(~topic, scale="free") +
  coord_flip()

### 6.2.2 Per-document classification
#?# create chapters_gamma: pull out gamma matrix
chapters_gamma = tidy(chapters_lda, matrix="gamma")
chapters_gamma

#?# summarize gamma by book and topic and do classification
chapters_gamma = chapters_gamma %>% 
  separate(document, c("book", "chapter"), sep="_")

chapters_gamma %>% 
  group_by(book, chapter) %>% 
  top_n(1, gamma) %>% 
  ungroup() %>% 
  mutate(chapter=as.numeric(chapter), topic=as.factor(topic)) %>% 
  ggplot(aes(chapter, gamma, fill=topic)) +
  geom_bar(stat="identity") +
  facet_wrap(~book, scale="free")

#?# check the cases of misclassification
chapter_classifications <- chapters_gamma %>%
  group_by(book, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

book_topics <- chapter_classifications %>%
  count(book, topic) %>%
  group_by(book) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = book, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(book != consensus)

###6.2.3 By word assignments: augment
#?# create assignments: assign words in chapters_lda into each topic
assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

#?# update assignments: check correctness of assignments


#?# plot confusion matrix: how often the misclassification

#?# research on the wrong words

###6.3 Alternative LDA implementations
library(mallet)
