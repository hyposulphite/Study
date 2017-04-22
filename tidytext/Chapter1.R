library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)

###########################
### Chapter 1

#_#
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
#__#

text


#?# create text_df: line=line number and text=text
text_df <- data_frame(line = 1:4, text = text)
text_df

#?# create text_df_tidy: one word per row
text_df_tidy = text_df %>%  unnest_tokens(word, text)
text_df_tidy


## 1.3 Tidying the works of Jane Austen

#_# create original_books from austen_books(): text, book, linenumber, chapter
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()
#__#

original_books

#?# create tidy_books: one word per row
tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

#?# create tidy_books: remove stop words
data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)

#?# count words and sort
tidy_books %>% count(word, sort = TRUE) 

#?# plot word count
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()


### 1.4 Word Frequency

#?# from gutenbergr, download c(35,36,5230,159). Use mirror = "http://mirrors.xmission.com/gutenberg/"
library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159), mirror = "http://mirrors.xmission.com/gutenberg/")

#?# create tidy_hgwells: remove stop words and count words
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort = TRUE)

#_#
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 766))
#__#

#?# create tidy_bronte: remove stop words and count words
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)


#?# combine tidy_bronte, tidy_hgwells, tidy_books and get word frequency.
frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)


#?# ggplot word frequency: Bronte vs JaneAusten, H.G. Wells vs JaneAusten
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)