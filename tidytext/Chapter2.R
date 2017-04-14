library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)

###########################
### Chapter 2
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

text_df <- data_frame(line = 1:4, text = text)

text_df

text_df_tidy = text_df %>%  unnest_tokens(word, text)

text_df_tidy


# book of Jane Austen
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books


tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

# remove stop words
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

# count words

tidy_books %>%
  count(word, sort = TRUE) 


# plot word count
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()


### 2.4 Word Frequency

#Cannot download files. Adhoc solution only
source("~/R/study/TM/all_funcs.R")

library(gutenbergr)

#hgwells <- gutenberg_download(c(35, 36, 5230, 159))

hgwells = my_file_gen("balen10")["text"]

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

#bronte <- gutenberg_download(c(1260, 768, 969, 9182, 766))
bronte = my_file_gen("comed10", file_num_limit = 10)["text"]

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

tidy_both <- bind_rows(
  mutate(tidy_bronte, author = "Bront? Sisters"),
  mutate(tidy_hgwells, author = "H.G. Wells"))

austen_percent <- tidy_books %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(word) %>%
  transmute(word, austen = n / sum(n)) %>% 
  filter(!is.na(word))

frequency <- tidy_both %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  mutate(other = n / sum(n)) %>%
  left_join(austen_percent) %>%
  ungroup()

library(scales)

ggplot(frequency, aes(x = other, y = austen, color = abs(austen - other))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == "Bront? Sisters",], ~ other + austen)

cor.test(data = frequency[frequency$author == "H.G. Wells",], ~ other + austen)
