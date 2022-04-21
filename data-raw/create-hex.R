
#### CREATE HEX STICKER ####

library(regulationsgov)
library(tidytext)
library(dplyr)
library(regulationsgov)

cfpb_docs <- get_all_documents(docketId ="CFPB-2014-0019")
cf_text <- add_text(cfpb_docs)

comments <- get_all_comments(docketId = "FAA-2018-1084")
comments_text <- add_text(comments)

docs_text <- get_all_documents(docketId = "FAA-2018-1084")
documents_text <- add_text(docs_text)

# collapse text into a single string for each separate collection
all_text1 <- comments_text %>%
  mutate(across(c(data_comment, text),
                ~ifelse(is.na(.), "", .))) %>%
  mutate(text = paste(data_comment, text)) %>%
  pull(text) %>%
  paste(collapse = " ") %>%
  tolower() %>%
  gsub("[[:punct:]]", "", .)

all_text2 <- documents_text %>%
  pull(text) %>%
  paste(collapse = " ") %>%
  tolower() %>%
  gsub("[[:punct:]]", "", .)

all_text3 <-  cf_text %>%
  pull(text) %>%
  paste(collapse = " ") %>%
  tolower() %>%
  gsub("[[:punct:]]", "", .)

# get highest freqency words from each collection
all_text1 <- tibble(text = all_text1) %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  anti_join(tidytext::stop_words,
            by = c("word" = "word")) %>%
  mutate(word = gsub("[[:digit:]]", "", word)) %>%
  filter(word != "") %>%
  slice_max(count, n=200)

all_text2 <- tibble(text = all_text2) %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  anti_join(tidytext::stop_words,
            by = c("word" = "word")) %>%
  mutate(word = gsub("[[:digit:]]", "", word)) %>%
  filter(word != "") %>%
  slice_max(count, n=200)

all_text3 <- tibble(text = all_text3) %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  anti_join(tidytext::stop_words,
            by = c("word" = "word")) %>%
  mutate(word = gsub("[[:digit:]]", "", word)) %>%
  filter(word != "") %>%
  slice_max(count, n=20)

# plot a random sample from the highest frequency words in the combined data frame
set.seed(125)
p <- bind_rows(all_text1, all_text2, all_text3) %>%
  filter(!word %in% c("suas", "its")) %>%
  slice_max(count,n=100) %>%
  slice_sample(n=20) %>%
  mutate(col = factor(sample.int(6, size = 20, replace= TRUE))) %>%
  ggplot(aes(label = word,
             size = count/max(count),
             color = col)) +
  geom_text_wordcloud() +
  scale_radius(range = c(2, 16), limits = c(0, NA)) +
  theme_void() +
  scale_size_area(max_size = 20) +
    scale_discrete_manual(aesthetics = "color",
                          values = c("#EFF3F4","#E3E9ED", "#E3EDEA", "#DCEBE1", "#ECE8F4", "#E8E7EF"))

# save image to images
ggsave("./images/plot.png", p)

# create hex sticker
sticker("./plot.png", package="regulationsgov",  s_x=1, s_y=.75, p_y = 1.35,
        s_width=.8, s_height=.8, filename="./images/hex_sticker.png",
        h_fill = "#32758C", h_color = "#204271",
        spotlight = TRUE, p_size = 6.1, dpi = 600, l_width = 6, l_height = 6, h_size = 1.4)


