# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("gutenbergr")

library(tidyverse)
library(tidytext)
library(gutenbergr)

gutenberg_works(author == "Shakespeare, William") %>%
  filter(str_detect(title, "Romeo|Nothing")) %>%
  select(gutenberg_id, title)

shakespeare <- gutenberg_download(c(1513, 1519), meta_fields = "title")

shakespeare <- shakespeare %>%
  mutate(type = if_else(title == "Romeo and Juliet", "Tragedy", "Comedy"))

shakespeare %>% count(title, gutenberg_id, type)

tidy_shakespeare <- shakespeare %>% 
  group_by(title) %>% # Group by the titles of the plays
  mutate(linenumber = row_number()) %>% # Define a new column linenumber
  unnest_tokens(word, text) %>% # Transform the non-tidy text data to tidy text data, that is: split "text" into multiple words by words into "word"!
  ungroup()

sentiment_contributions <- tidy_shakespeare %>%
  count(title, word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(title) %>%   # Group by title
  mutate(contribution = score * n / sum(n)) %>% # Calculate a contribution for each word in each title. By multiplying each word's score by the times it is used in the play and divided by the total words in the play.
  ungroup()

sentiment_contributions %>% 
  group_by(title) %>% 
  top_n(12, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(x = word, y = contribution, fill = score > 0)) + 
  geom_col(show.legend = F) + 
  facet_wrap(~title, scales = "free") + 
  coord_flip()

tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(x = index, y = sentiment, fill = type)) + # Put index on x-axis, sentiment on y-axis, and map comedy/tragedy to fill
  geom_col() + # Make a bar chart with geom_col()
  facet_wrap(~title, scales = "free_x") # Separate panels for each title with facet_wrap(), scales = "free_x" so the x-axes behave nicely!
