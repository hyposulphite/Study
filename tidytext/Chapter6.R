library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(gutenbergr)
library(stringr)
library(tidyr)

#get data AssociatedPress
data("AssociatedPress")
AssociatedPress

#conduct LDA with k=2
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

#tidy LDA object
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

#get top 10 words that define the two topics
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#plot the words
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#find words making greatest difference between topic1 and topic2


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


#6.1.2
#get tidy version of gamma matrix: per-document-per-topic probability
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

#check why document 6 has high topic 2 probability
tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))



###6.2
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")


#########
#download has some issue
#https://github.com/ropenscilabs/gutenbergr/issues/8

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title"
                     , mirror = "http://mirrors.xmission.com/gutenberg/")

#########



# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

#6.2.1 LDA on chapters
#get dtm
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

#LDA with k=4
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

#pull out beta matrix for the topics
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

#get top 5 terms by topics
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms


#plot
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#6.2.2 Per-document classification
#pull out gamm matrix
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

#
chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)








