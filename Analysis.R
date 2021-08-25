library(tidyverse)
library(stringr)
library(tidytext)
library(plotly)
library(data.table)
library(visNetwork)
library(igraph)
library(sentimentr)
library(textstem)
library(topicmodels)


d1 <- readRDS("HIMYM_data.rds")


data %>% 
  group_by(season) %>% 
  summarise(num_episodes = length(unique(episode)))

# There are some issues with the scraped data. There are a lot of episodes where there were no actors included for the lines. Sadly I needed to exclude them from this analysis.
d1 <- d1 %>% 
  group_by(season, episode) %>% 
  mutate(lines = n()) %>% 
  ungroup()

# Exclude episodes with less than 150 lines
d1 <- d1 %>% 
  filter(lines >= 100)


length(unique(d1$title)) # 133 episodes remain

data %>% 
  group_by(season) %>% 
  summarise(num_episodes = length(unique(episode))) 

length(unique(d1$actor))

## Lets only keep the lines for the main actors
data %>% 
  group_by(actor) %>% 
  summarise(lines = n()) %>% 
  arrange(desc(lines))

length(unique(full_df$actor))
# EXPLORING DATA

data %>% group_by(actor) %>% 
  summarize(lines = n()) %>% 
  arrange(desc(lines)) %>% 
  top_n(10) %>%
  ggplot(aes(reorder(actor, lines), lines)) +
  geom_col(fill = 'cyan4', alpha = 0.8) +
  geom_text(aes(label = lines), size = 4, position = position_stack(vjust = 0.5)) +
  labs(title = 'Number of lines by charachters (top 10)',
       x = NULL, y = NULL) +
  coord_flip() +
  theme_light()


## Lets only keep the lines for the main actors

data <- data %>% filter(actor %in% c("Ted", "Barney", "Marshall", "Robin", "Lily")) 

data %>% 
  group_by(season, actor) %>% 
  summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  group_by(season) %>% 
  top_n(5) %>% 
  ungroup() %>% 
  mutate(season = as.factor(season),
         actor = as.factor(actor),
         actor = reorder_within(actor, Count, season)) %>% 
  ggplot(aes(actor, Count, fill = season)) +
  geom_col(show.legend = F) +
  labs(title = 'Number of lines by charachters in each season',
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~season, scales = 'free') +
  theme_light()


# TOKENIZATION


# before unnestinf, remove text between spec chars like [] or (), those
# aren't spoken by anyone, those just indicate the environment, or some
# external influencers (over the phone, yelling, throws paper, etc...)

data$text <- trimws(str_replace_all(data$text, '\\[(.*?)\\]', ''))


# unnest and remove stopwords

data %>% 
  group_by(actor) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')))) %>%  
  count(actor, word, sort = T) %>% 
  group_by(actor) %>% 
  top_n(10, wt = n) %>%
  ungroup() %>% 
  mutate(actor = as.factor(actor),
         word = reorder_within(word, n, actor)) %>% 
  ggplot(aes(word, n, fill = actor)) +
  geom_col(show.legend = F) +
  labs(title = 'Top words by people',
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~actor, scales = 'free') +
  theme_light()



## Get out names
data %>% 
  group_by(actor) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'hey', 'wait', 'gonna', 'guy', 'guys', 'time', 'lot', 'uh', "night", "god", "um")),
                      data.frame(word = c(tolower(unique(data$actor)))))) %>%  
  count(actor, word, sort = T) %>% 
  group_by(actor) %>% 
  top_n(10, wt = n) %>%
  ungroup() %>% 
  mutate(actor = as.factor(actor),
         word = reorder_within(word, n, actor)) %>% 
  ggplot(aes(word, n, fill = actor)) +
  geom_col(show.legend = F) +
  labs(title = 'Top words by people without names',
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~actor, scales = 'free') +
  theme_light()



# top words wordcloud

words <- data %>% 
  group_by(actor) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'hey', 'wait', 'gonna', 'guy', 'guys', 'time', 'lot', 'uh', "night", "god", "um")),
                      data.frame(word = c(tolower(unique(data$actor)))))) %>%  
  count(actor, word, sort = T) 


library(wordcloud)

wordcloud(words = (words %>% filter(actor == 'Ted'))$word,
          freq = (words %>% filter(actor== 'Ted'))$n, 
          min.freq = 10, 
          max.words = 20, colors = brewer.pal(10, 'OrRd'))



# tf-idf on words by characters

data %>% 
  group_by(actor) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  mutate(word = lemmatize_words(word)) %>% 
  count(actor, word, sort = T) %>% 
  ungroup() %>% 
  bind_tf_idf(term = word, document = actor, n = n) %>% 
  group_by(actor) %>% 
  top_n(8, wt = tf_idf) %>%
  ungroup() %>% 
  mutate(actor = as.factor(actor),
         word = reorder_within(word, tf_idf, actor)) %>% 
  ggplot(aes(word, tf_idf, fill = actor)) +
  geom_col(show.legend = F) +
  labs(title = "Amazing tool!",
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~actor, scales = 'free') +
  theme_bw()



# top bigrams by character

my_stops <- bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')))

data %>% 
  group_by(actor) %>% 
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>% 
  ungroup() %>% 
  separate(bigram, c('word_1', 'word_2'), sep = ' ') %>%
  filter((!word_1 %in% my_stops$word) & 
           (!word_2 %in% my_stops$word) & (word_1 != word_2)) %>% 
  unite(bigram, word_1, word_2, sep = ' ') %>% 
  count(actor, bigram, sort = T) %>% 
  group_by(actor) %>% 
  top_n(6, wt = n) %>% 
  ungroup() %>% 
  mutate(actor = as.factor(actor),
         bigram = reorder_within(bigram, n, actor)) %>% 
  ggplot(aes(bigram, n, fill = actor)) +
  geom_col(show.legend = F) +
  labs(title = "",
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~actor, scales = 'free') +
  theme_bw()


# SENTIMENTS


# pos-neg
data %>% 
  group_by(actor) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  mutate(word = lemmatize_words(word)) %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'hey', 'wait', 'gonna', 'guy', 'guys', 'time', 'lot', 'uh', "night", "god", "um")),
                      data.frame(word = c(tolower(unique(data$actor)))))) %>%  
  inner_join(sentiments) %>% 
  count(actor, word, sentiment, sort = T) %>% 
  ungroup() %>% 
  group_by(actor, sentiment) %>% 
  top_n(5, wt = n) %>% 
  ungroup() %>% 
  mutate(n = ifelse(sentiment == 'positive', n, -n)) %>% 
  mutate(actor = as.factor(actor),
         word = reorder_within(word, n, actor)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = F) +
  labs(title = 'Top positive words are used more frequently on average than top negative words',
       subtitle = "Top positive and negative words by each character",
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~actor, scales = 'free') +
  theme_bw()


# using afinn and multiplying by count
data %>% 
  group_by(actor) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  mutate(word = lemmatize_words(word)) %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'hey', 'wait', 'gonna', 'guy', 'guys', 'time', 'lot', 'uh', "night", "god", "um")),
                      data.frame(word = c(tolower(unique(data$actor)))))) %>% 
  count(actor, word, sort = T) %>% 
  ungroup() %>% 
  inner_join(get_sentiments(lexicon = 'afinn')) %>% 
  mutate(sentiment_value = n * value) %>% 
  group_by(actor) %>% 
  top_n(10, wt = abs(sentiment_value)) %>% 
  ungroup() %>% 
  mutate(sentiment = ifelse(value < 0, 'negative', 'positive'),
         name = as.factor(actor),
         word = reorder_within(word, sentiment_value, actor)) %>% 
  ggplot(aes(word, sentiment_value, fill = sentiment)) +
  geom_col(show.legend = F) +
  labs(title = "Angela, Darryl & Dwight seem to 'contribute' most negativity to their languages",
       x = NULL, y = 'Contributed sentiment score (score * count)') +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~actor, scales = 'free') +
  theme_bw()



# sentiment trend

data_with_sentiments <- data %>%
  group_by(season, episode) %>% 
  mutate(episode_num = cur_group_id()) %>% 
  ungroup() %>% 
  select(season, episode, episode_num, actor, text) %>% 
  mutate(sentiment = sentimentr::sentiment_by(text)$ave_sentiment)

data_with_sentiments %>% 
  filter(sentiment != 0) %>% 
  group_by(episode_num, actor) %>% 
  filter((sentiment != min(sentiment)) & (sentiment != max(sentiment))) %>% # taking the min and max out of all episodes (usually errors, like whoa whoa whoa etc...)
  summarize(min_sentiment = min(sentiment),
            max_sentiment = max(sentiment),
            avg_sentiment = mean(sentiment)) %>% # least and most positive episodes taken after minmax deletions
  ungroup() %>% 
  mutate(actor = as.factor(actor)) %>% 
  ggplot(aes(x = episode_num), fill = 'blue', color = 'blue') +
  geom_line(aes(y = avg_sentiment), show.legend = F, alpha = 0.5) +
  geom_ribbon(aes(ymin = min_sentiment, ymax = max_sentiment), fill = 'skyblue', alpha = 1/3, show.legend = F, color = NA) +
  labs(title = "Sentiment trend analysis by person by episode does not offer insights: lots of volatility",
       x = '# of episode', y = NULL) +
  facet_wrap(~actor) +
  theme_bw()


data_with_sentiments %>% 
  filter(sentiment != 0) %>% 
  group_by(season = as.numeric(season), actor) %>% 
  summarize(median_sentiment = median(sentiment)) %>% # least and most positive episodes taken after minmax deletions
  ungroup() %>% 
  mutate(actor = as.factor(actor)) %>% 
  ggplot(aes(x = season), fill = 'blue', color = 'blue') +
  geom_line(aes(y = median_sentiment), show.legend = F, alpha = 0.5) +
  geom_point(aes(y = median_sentiment), show.legend = F, color = 'blue', fill = 'blue') +
  labs(title = "By-season trend may hint at Andy's firing, but no more insightful than by-episode trend",
       x = '# of season', y = NULL) +
  scale_x_continuous(breaks = seq(1, 9, by = 1)) +
  facet_wrap(~actor) +
  theme_bw()





# topics modeling

top_words <- data %>% 
  group_by(actor) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  select(actor, word) %>% 
  mutate(word = lemmatize_words(word)) %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'hey', 'wait', 'gonna', 'guy', 'guys', 'time', 'lot', 'uh', "night", "god", "um")),
                      data.frame(word = c(tolower(unique(data$actor)))))) %>% 
  count(actor, word, sort = T) %>% 
  ungroup()

top_words_dtm <- top_words %>% cast_dtm(actor, word, n)


# 12 clusters 

top_words_dtm_lda <- top_words_dtm %>% LDA(k = 5, control = list(seed = 8080))

top_words_dtm_lda_gammas <- tidy(top_words_dtm_lda, matrix = 'gamma')

top_words_dtm_lda_gammas %>%  
  rename('actor' = 'document') %>% 
  mutate(topic = as.factor(topic),
         actro = as.factor(actor)) %>% 
  ggplot(aes(topic, gamma, fill = actor)) + 
  geom_point(show.legend = F, color = 'black', shape = 8) +
  facet_wrap(~actor, scales = 'free') + 
  labs(title = "Only Michael & Dwight don't have 'one clear vocabulary'",
       x = '5 topics (clusters) from LDA algo',
       y = '% of being assigned to one cluster') +
  theme_light()



person <- "Barney"

x <- data %>% 
  group_by(actor) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>%
  filter(actor == person) %>% 
  select(actor, word) %>% 
  mutate(word = lemmatize_words(word)) %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'hey', 'wait', 'gonna', 'guy', 'guys', 'time', 'lot', 'uh', "night", "god", "um")),
                      data.frame(word = c(tolower(unique(data$actor)))))) %>% 
  count(actor, word, sort = T) %>% 
  ungroup()



x_dtm <- x %>% cast_dtm(actor, word, n)

x_lda <- x_dtm %>% LDA(k = 2, control = list(seed = 123))

x_lda_betas <- tidy(x_lda, matrix = 'beta')


x_lda_betas %>% 
  mutate(topic = paste0('topic_', topic)) %>% 
  spread(topic, beta) %>% 
  mutate(log_ratio = log2(topic_2 / topic_1),
         pos_neg = ifelse(log_ratio < 0, 'neg', 'pos')) %>% 
  group_by(pos_neg) %>% 
  top_n(15, abs(log_ratio)) %>% 
  ungroup() %>% 
  mutate(term = as.factor(term),
         term = reorder(term, log_ratio)) %>% 
  ggplot(aes(term, log_ratio, fill = pos_neg, color = pos_neg)) + 
  geom_col(show.legend = F, width = 2/3) +
  coord_flip() +
  labs(title = 'Largest beta-differences somewhat outline two topics',
       subtitle = paste0("Running LDA on s words"),
       x = 'Top terms',
       y = 'Log-ratio') +
  theme_bw()



x_lda_betas %>% 
  group_by(topic) %>% 
  top_n(30, beta) %>% 
  ungroup() %>% 
  mutate(term = as.factor(term),
         term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = F, width = 2/3) +
  facet_wrap(~topic, scales = 'free') + 
  coord_flip() + 
  scale_x_reordered() +
  labs(title = 'Top words for 2 clusters created for ',
       subtitle = paste0("Running LDA ons words"),
       x = 'Top terms',
       y = 'Beta') +
  theme_bw()





