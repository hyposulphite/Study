library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)

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
library(tidyr)

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
