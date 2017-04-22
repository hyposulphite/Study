
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

#?# create text_df: line=line number and text=text
text_df = data_frame(line=1:length(text), text=text)

#?# create text_df_tidy: one word per row
text_df_tidy = text_df %>% 
  unnest_tokens(word, text)

#_# create original_books from austen_books(): text, book, linenumber, chapter
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()

#?# create tidy_books: one word per row
tidy_books <- original_books %>% unnest_tokens(word, text)

#?# create tidy_books: remove stop words
tidy_books <- tidy_books %>% anti_join(stop_words)

#?# count words and sort
tidy_books %>% count(word, sort=TRUE)

#?# plot word count
tidy_books %>% 
  count(word, sort=TRUE) %>%
  mutate(word=reorder(word, n)) %>% 
  filter(n>600) %>% 
  ggplot(aes(x=word, y=n)) +
  geom_bar(stat="identity") +
  coord_flip()

### 1.4 Word Frequency

#?# from gutenbergr, download c(35,36,5230,159). Use mirror = "http://mirrors.xmission.com/gutenberg/"
library(gutenbergr)
hgwells <- gutenberg_download(c(35,36,5230,159))

#?# create tidy_hgwells: remove stop words and count words
tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)
tidy_hgwells %>% count(word, sort=TRUE)
#_#
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 766))

#?# create tidy_bronte: remove stop words and count words
tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_bronte %>% count(word, sort=TRUE)

#?# combine tidy_bronte, tidy_hgwells, tidy_books and get word frequency.

#?# ggplot word frequency: Bronte vs JaneAusten, H.G. Wells vs JaneAusten
