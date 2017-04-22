
library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)

###########################

### Chapter 1

#?# create text_df: line=line number and text=text

#?# create text_df_tidy: one word per row

#?# create tidy_books: one word per row

#?# create tidy_books: remove stop words

#?# count words and sort

#?# plot word count

### 1.4 Word Frequency

#?# from gutenbergr, download c(35,36,5230,159). Use mirror = "http://mirrors.xmission.com/gutenberg/"
library(gutenbergr)

#?# create tidy_hgwells: remove stop words and count words

#?# create tidy_bronte: remove stop words and count words

#?# combine tidy_bronte, tidy_hgwells, tidy_books and get word frequency.

#?# ggplot word frequency: Bronte vs JaneAusten, H.G. Wells vs JaneAusten
