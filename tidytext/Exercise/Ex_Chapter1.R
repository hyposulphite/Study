
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

#?# create text_df_tidy: one word per row
#_# create original_books from austen_books(): text, book, linenumber, chapter
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()

#?# create tidy_books: one word per row

#?# create tidy_books: remove stop words

#?# count words and sort

#?# plot word count

### 1.4 Word Frequency

#?# from gutenbergr, download c(35,36,5230,159). Use mirror = "http://mirrors.xmission.com/gutenberg/"
library(gutenbergr)

#?# create tidy_hgwells: remove stop words and count words
#_#
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 766))

#?# create tidy_bronte: remove stop words and count words

#?# combine tidy_bronte, tidy_hgwells, tidy_books and get word frequency.

#?# ggplot word frequency: Bronte vs JaneAusten, H.G. Wells vs JaneAusten
