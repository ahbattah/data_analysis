# 1. Library Installations --------------------------------------------------------------------
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("gutenbergr")

# 2. Loading Library --------------------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(gutenbergr)

# 3. Get the book titles ----------------------------------------------------------------------
gutenberg_works(author == "Shakespeare, William") %>%
  filter(str_detect(title, "Romeo|Nothing")) %>%
  select(gutenberg_id, title)
# This breaks into:

gutenberg_works()

gutenberg_works(author == "Shakespeare, William")

getwd()

gutenberg_works(author == "Shakespeare, William") %>%
  write_csv("booktitles.csv")

gutenberg_works(author == "Shakespeare, William") %>%
  filter(str_detect(title, "Romeo|Nothing"))

gutenberg_works(author == "Shakespeare, William") %>%
  filter(str_detect(title, "Romeo|Nothing")) %>%
  select(gutenberg_id, title)

# 4. Download the needed books ----------------------------------------------------------------
shakespeare <- gutenberg_download(c(1513, 1519), meta_fields = "title")
# breaks into:

gutenberg_download()

gutenberg_download(1513)

gutenberg_download(c(1513, 1519))

# *********************
# how array works
1
1, 2 # error!

c(1, 2)
#*********************

gutenberg_download(1513, meta_fields = "title")

shakespeare <- gutenberg_download(c(1513, 1519), meta_fields = "title")

# 5. Add type ---------------------------------------------------------------------------------
shakespeare <- shakespeare %>%
  mutate(type = if_else(title == "Romeo and Juliet", "Tragedy", "Comedy"))
# breaks into:

shakespeare

shakespeare %>%
  count(title)

shakespeare %>%
  mutate(type = "Tragedy")

shakespeare %>%
  mutate(type = "Tragedy") %>%
  count(title)

shakespeare %>%
  mutate(type = "Tragedy") %>%
  count(title, type)

shakespeare <- shakespeare %>%
  mutate(type = if_else(title == "Romeo and Juliet", "Tragedy", "Comedy"))

shakespeare %>% count(title, gutenberg_id, type)

# 6. Data wrangling / munging !!! -------------------------------------------------------------

# 7. tidy_shakespeare -------------------------------------------------------------------------
tidy_shakespeare <- shakespeare %>% 
  group_by(title) %>% 
  mutate(linenumber = row_number()) %>% 
  unnest_tokens(word, text) %>% # Transform the non-tidy text data to tidy text data, that is: split "text" into multiple words by words into column "word"!
  ungroup()
# This breaka into:

shakespeare
tidy_shakespeare %>%
  print(n = 20) # first look at the input & output

# So we want to break each line of text into columns with their respective line number

shakespeare %>% 
  mutate(linenumber = row_number()) # this is wrong!

shakespeare %>% 
  mutate(linenumber = row_number()) %>%
  filter(gutenberg_id == 1519)

shakespeare %>% 
  mutate(linenumber = row_number()) %>%
  tail()

shakespeare %>% 
  group_by(title) %>% 
  mutate(linenumber = row_number())

shakespeare %>% 
  group_by(title) %>% 
  mutate(linenumber = row_number()) %>%
  filter(gutenberg_id == 1519)

shakespeare %>% 
  group_by(title) %>% 
  mutate(linenumber = row_number()) %>% 
  tail()

# -- unnest_tokens -- look at the input and output
shakespeare
shakespeare %>% 
  unnest_tokens(word, text)

# -- ungroup, tibble, data.frame
shakespeare %>% 
  group_by(title) %>% 
  mutate(linenumber = row_number()) %>% 
  unnest_tokens(word, text)

tibble
data.frame

shakespeare %>% 
  group_by(title) %>% 
  mutate(linenumber = row_number()) %>% 
  unnest_tokens(word, text) %>%
  ungroup()

tidy_shakespeare <- shakespeare %>% 
  group_by(title) %>% 
  mutate(linenumber = row_number()) %>% 
  unnest_tokens(word, text) %>% # Transform the non-tidy text data to tidy text data, that is: split "text" into multiple words by words into column "word"!
  ungroup()

# Break ---------------------------------------------------------------------------------------
# To see how we are doing and for questions so far

# 8. sentiment_contributions ------------------------------------------------------------------
# Calculate a contribution for each word in each title. 
# By multiplying each word's score by the times it is used in the play and 
# divided by the total words in the play.
sentiment_contributions <- tidy_shakespeare %>%
  count(title, word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(title) %>%   # Group by title
  mutate(contribution = score * n / sum(n)) %>% 
  ungroup()
# Breaks into:

# 1st look at:
get_sentiments() # gets the lexicons - so far we have 4!
# - afinn:    gives score    : [-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5]
# - bing:     gives sentiment: [negative, positive]
# - nrc:      gives sentiment: [anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise, trust]
# - loughran: gives sentiment: [constraining, litigious, litigious, positive, superfluous, uncertainty]

get_sentiments("afinn")

tibble(word = c("hello"))

tibble(word = c("hello")) %>%
  inner_join(get_sentiments("afinn"))

tibble(word = c("hello")) %>%
  left_join(get_sentiments("afinn"))

tibble(word = c("great")) %>%
  inner_join(get_sentiments("afinn"))

tibble(word = c("great", "poor", "excellent")) %>%
  inner_join(get_sentiments("afinn"))

get_sentiments("afinn") %>% 
  filter(score == 5)

get_sentiments("afinn") %>% 
  filter(score == 4)

# 2nd look at needed task

# same as before look at the input and output

tidy_shakespeare

tidy_shakespeare %>%
  count(title, word, sort = TRUE)

tidy_shakespeare %>%
  count(title, word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"))

tidy_shakespeare %>%
  count(title, word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(title) %>%   # Group by title
  mutate(sum = sum(n), count = n(), contribution = score * n / sum(n))

tidy_shakespeare %>%
  count(title, word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(title) %>%   # Group by title
  mutate(contribution = score * n / sum(n))

# Finally
sentiment_contributions <- tidy_shakespeare %>%
  count(title, word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(title) %>%   # Group by title
  mutate(contribution = score * n / sum(n)) %>% 
  ungroup()

# 9. plot top Word contributions per play (data viz, factor) ----------------------------------
# Calculate a relative contribution for each word in each play.
# Please plot Word contributions by play (title), top 12 negative & positive contribution
sentiment_contributions %>% 
  group_by(title) %>% 
  top_n(12, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(x = word, y = contribution, fill = score > 0)) + 
  geom_col(show.legend = F) + 
  facet_wrap(~title, scales = "free") + 
  coord_flip()
# This breaks into:

# 1st ggplot - visualization

sentiment_contributions

sentiment_contributions %>%
  filter(title == "Romeo and Juliet") %>%
  ggplot(aes(x = title, y = score)) +
  geom_boxplot()

sentiment_contributions %>%
  filter(title == "Romeo and Juliet") %>%
  ggplot(aes(x = score)) +
  geom_histogram()

sentiment_contributions %>%
  filter(word == "love") %>%
  ggplot(aes(x = word, y = n)) +
  geom_col()

sentiment_contributions %>%
  filter(word == "love") %>%
  ggplot(aes(x = word, y = n, fill = title)) +
  geom_col()

sentiment_contributions %>%
  filter(word == "love") %>%
  ggplot(aes(x = word, y = n, fill = title)) +
  geom_col() +
  facet_wrap(~title)

# This is not enough to learn Data Visualzation in R using ggplot
#
# More basic introduciton is needed
#
# Where basics are:
# 1. Characterizing a single variable
# 2. two variables
# 3. More as complex
#
# Then ggplot introduction, like:
# Grammar of Graphis:
# 1. Graphics = distinct layers of grammatical elements
# 2. Meaningful plots through aesthetic mapping
#
# And the steps as Grammatical Elements:
# 1. Data:        The dataset being plotted.
# 2. Aesthetics:  The scales onto which we map our data.
# 3. Geometries:  The visual elements used for our data.
# 4. Facets:      Plotting small multiples.
# 5. Statistics:  Representations of our data to aid understanding.
# 6. Coordinates: The space on which the data will be plotted.
# 7. Themes:      All non-data ink.
#
# And we have to always remember:
# Always clearn / tidy your data! Look at (iris, iris.wide, iris.wide2, iris.tidy)!

# 2nd, look at the input & output
sentiment_contributions %>% 
  group_by(title) %>% 
  top_n(12, n) %>% 
  ungroup() %>%
  print(n = 30)

sentiment_contributions %>% 
  group_by(title) %>% 
  top_n(12, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, contribution))

# ****************************
# factor, categorical variables

c("fast", "slow", "fast", "slow", "normal", "slow")

factor(c("fast", "slow", "fast", "slow", "normal", "slow"))

factor(c("fast", "slow", "fast", "slow", "normal", "slow"), 
       ordered = T, 
       levels = c("slow", "normal", "fast"))

tibble(name = c("Ahmed", "Ali", "Mona", "Mohammed", "Suad", "Yusra"), 
       speed = c("fast", "slow", "fast", "slow", "normal", "slow"))

tibble(name = c("Ahmed", "Ali", "Mona", "Mohammed", "Suad", "Yusra"), 
       speed = c("fast", "slow", "fast", "slow", "normal", "slow")) %>%
  summarise(fast = max(speed))

tibble(name = c("Ahmed", "Ali", "Mona", "Mohammed", "Suad", "Yusra"), 
       speed = factor(c("fast", "slow", "fast", "slow", "normal", "slow"), 
                      ordered = T, 
                      levels = c("slow", "normal", "fast"))) %>%
  summarise(fast = max(speed))
# ****************************

sentiment_contributions %>% 
  group_by(title) %>% 
  top_n(12, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, contribution))

# Why to order word?

sentiment_contributions %>% 
  group_by(title) %>% 
  top_n(12, n) %>% 
  ungroup() %>% 
  # mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(x = word, y = contribution, fill = score > 0)) + 
  geom_col(show.legend = F) +
  facet_wrap(~title, scales = "free") + 
  coord_flip()

# Well, ggplot reorders in its own way, why?
# There are of course reasons, but the truth, life is not easy!

sentiment_contributions %>% 
  group_by(title) %>% 
  top_n(12, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(x = word, y = contribution, fill = score > 0)) + 
  geom_col(show.legend = F) + 
  facet_wrap(~title, scales = "free") + 
  coord_flip()

# Break ---------------------------------------------------------------------------------------
# Maybe another break!
# To see how we are doing and for questions so far

# 10. Visualizing narrative arcs --------------------------------------------------------------
tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(x = index, y = sentiment, fill = type)) + # Put index on x-axis, sentiment on y-axis, and map comedy/tragedy to fill
  geom_col() + # Make a bar chart with geom_col()
  facet_wrap(~title, scales = "free_x") # Separate panels for each title with facet_wrap(), scales = "free_x" so the x-axes behave nicely!
# Now this, you do it by yourself
# and lets open the room for questions
