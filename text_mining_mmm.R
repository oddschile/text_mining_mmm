library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(lubridate)
library(topicmodels)



mmm19 <- read.csv("http://oddschile.net/mmm101.csv")

mmm19$text <- as.character(mmm19$text)

mmm19$gender <- as.character(mmm19$gender)

mmm19$grade <- as.character(mmm19$grade)


mmm19$date <- as.Date(mmm19$date, format = "%m/%d/%Y")

tbl_df(mmm19)

head(mmm19)

mmm19 %>% 
  count(gender, grade)

mmm19 %>% 
  count(gender)

mmm19 %>%
  count(grade)

mmm19td <- mmm19 %>%
  group_by(gender) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

mmm19sent <- mmm19td %>%
  # Implement sentiment analysis with the "bing" lexicon
  inner_join(get_sentiments("bing") )

mmm19sent %>%
  # Find how many positive/negative words by gender
  count(gender,sentiment)

mmm19sent %>%
  # Find how many positive/negative words by grade
  count(grade,sentiment)

sentiment_counts <- mmm19td %>%
    # Implement sentiment analysis using the "bing" lexicon
    inner_join(get_sentiments("bing")) %>%
    # Count the number of words by gender, grade, and sentiment
    count(gender, grade, sentiment)

sentiment_counts %>%
    # Group by gender
    group_by(gender) %>%
    # Find the total number of words in each text
    mutate(total = sum(n),
    # Calculate the number of words divided by the total
           percent = n / total) %>%
    # Filter the results for only negative sentiment
    filter(sentiment == "negative") %>%
    arrange(percent)


word_counts <- mmm19td %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count by word and sentiment
  count(word, sentiment)

top_words <- word_counts %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top_words, aes(word, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()


sentiment_contributions <- mmm19td %>%
  # Count by grade and word
  count(gender, word, sort = TRUE) %>%
  # Implement sentiment analysis using the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>%
  # Group by grade
  group_by(gender) %>%
  # Calculate a contribution for each word
  mutate(contribution = score * n / sum(n)) %>%
  ungroup()
    
sentiment_contributions

sentiment_contributions %>%
  # Filter for female
  filter(gender == "Female") %>%
  # Arrange to see the most negative words
  arrange(contribution)

sentiment_contributions %>%
  # Filter for male
  filter(gender == "Male") %>%
  # Arrange to see the most positive words
  arrange(desc(contribution))


mmm19td %>%
  # Implement sentiment analysis using "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count using four arguments
  count(gender, grade, index = linenumber %/% 20, sentiment) %>%
spread(sentiment, n, fill = 0) %>%
  # Use mutate to find net sentiment
  mutate(sentiment = positive -  negative) %>%
ggplot(aes(index, sentiment, fill = gender)) +
  # Make a bar chart with geom_col()
  geom_col() +
  # Separate panels for grade with facet_wrap()
  facet_wrap(~grade, scales = "free_x")


library(lubridate)

senttime <- mmm19td %>%
    # Define a new column using floor_date()
    mutate(date = floor_date(date, unit = "6 months")) %>%
    # Group by date
    group_by(date) %>%
    mutate(total_words = n()) %>%
    ungroup() %>%
    # Implement sentiment analysis using the NRC lexicon
    inner_join(get_sentiments("nrc"))

senttime %>%
    # Filter for positive and negative words
    filter(sentiment %in% c("positive", "negative")) %>%
    # Count by date, sentiment, and total_words
    count(date, sentiment, total_words) %>%
    ungroup() %>%
    mutate(percent = n / total_words) %>%
    # Set up the plot with aes()
    ggplot(aes(date, percent, color = sentiment)) +
    geom_line(size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, lty = 2) +
    expand_limits(y = 0)

mmm19td %>%
    # Define a new column that rounds each date to the nearest 6 months
    mutate(date = floor_date(date, unit = "6 month")) %>%
    filter(word %in% c("hard", "work", "problem",
                       "important", "critical", "good")) %>%
    # Count by date and word
    count(date, word) %>%
    ungroup() %>%
    # Set up your plot with aes()
    ggplot(aes(date, n, color = word)) +
    # Make facets by word
    facet_wrap(~word) +
    geom_line(size = 1.5, show.legend = FALSE) +
    expand_limits(y = 0)

#create a document term matrix (dtm)
dtm <- mmm19 %>% 
	unnest_tokens(input=text, output=word) %>% 
    anti_join(stop_words) %>% 
    count(grade, word) %>% 
    cast_dtm(document=grade, term=word, value=n)

# create a LDA model to establish 2 topics
mod <- LDA(x=dtm, k=2, method="Gibbs", 
           control=list(alpha=1, seed=10005))

# Display top 5 words of each topic
terms(mod, k=5)

# Display the words whose probability is above the threshold
terms(mod, threshold=0.0075)

# Display topic prevalance in documents as a table
tidy(mod, "gamma") %>% spread(topic, gamma)  
# Define which words we want to examine
my_terms = c("goals", "skills", "lead", "management", "staff", "relatonships", "effective","communication")

# Make a tidy table
t <- tidy(mod, "beta") %>% filter(term %in% my_terms)

# Make a stacked column chart of word probabilities
ggplot(t, aes(x=term, y=beta)) + geom_col(aes(fill=factor(topic))) +
  theme(axis.text.x=element_text(angle=90))




