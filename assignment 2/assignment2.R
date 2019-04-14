data <- fread("~/MBA/MBAD 6211/2- assignments/assignment 2/psychcentral_data.csv",
                 strip.white=T, sep=",", header=T, na.strings=c("NA","NaN","","?"))

nrow(data)
colnames(data)

tidy_text <- data %>%
  unnest_tokens(word, "q_content")
tidy_text[1:5]

data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)

tidy_text %>%
  count(word, sort = TRUE)

tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

tidy_text2 <- data %>%
  unnest_tokens(words, q_content) %>%
  mutate(word = wordStem(words))

data(stop_words)
tidy_text2 <- tidy_text %>%
  anti_join(stop_words)

tidy_text2 %>%
  count(word, sort = TRUE)

tidy_text2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

tidy_text2 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

tidy_text2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

########################################################################################

tidy_text_a <- data %>%
  unnest_tokens(word, "answers")
tidy_text_a[1:5]

data(stop_words)
tidy_text_a <- tidy_text_a %>%
  anti_join(stop_words)

tidy_text_a %>%
  count(word, sort = TRUE)

tidy_text_a %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

tidy_text_a2 <- data %>%
  unnest_tokens(words, "answers") %>%
  mutate(word = wordStem(words))

data(stop_words)
tidy_text_a2 <- tidy_text %>%
  anti_join(stop_words)

tidy_text_a2 %>%
  count(word, sort = TRUE)

tidy_text_a2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 6000) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

tidy_text_a2 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

tidy_text_a2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

data <- data[1:1000,] # Perform LDA on the first 1,000 rows of data
corpus <- Corpus(VectorSource(data$q_content),readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2,
removeNumbers = TRUE, removePunctuation = TRUE, stemDocument = TRUE))
rowTotals <- apply(dtm, 1, sum)  #Find the sum of words in each document
dtm.new <- dtm[rowTotals>0,] #remove all docs without words
lda <- LDA(dtm.new, k = 5) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#################################################################################

data <- data[1:1000,] # Perform LDA on the first 1,000 rows of data
corpus <- Corpus(VectorSource(data$q_content),readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2,
                                                 removeNumbers = TRUE, removePunctuation = TRUE, stemDocument = TRUE))
rowTotals <- apply(dtm, 1, sum)  #Find the sum of words in each document
dtm.new <- dtm[rowTotals>0,] #remove all docs without words
lda <- LDA(dtm.new, k = 2) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

##########################################

data <- data[1:1000,] # Perform LDA on the first 1,000 rows of data
corpus <- Corpus(VectorSource(data$q_content),readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2,
                                                 removeNumbers = TRUE, removePunctuation = TRUE, stemDocument = TRUE))
rowTotals <- apply(dtm, 1, sum)  #Find the sum of words in each document
dtm.new <- dtm[rowTotals>0,] #remove all docs without words
lda <- LDA(dtm.new, k = 3) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

###################

data <- data[1:1000,] # Perform LDA on the first 1,000 rows of data
corpus <- Corpus(VectorSource(data$q_content),readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2,
                                                 removeNumbers = TRUE, removePunctuation = TRUE, stemDocument = TRUE))
rowTotals <- apply(dtm, 1, sum)  #Find the sum of words in each document
dtm.new <- dtm[rowTotals>0,] #remove all docs without words
lda <- LDA(dtm.new, k = 4) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

###################

data <- data[1:1000,] # Perform LDA on the first 1,000 rows of data
corpus <- Corpus(VectorSource(data$q_content),readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2,
                                                 removeNumbers = TRUE, removePunctuation = TRUE, stemDocument = TRUE))
rowTotals <- apply(dtm, 1, sum)  #Find the sum of words in each document
dtm.new <- dtm[rowTotals>0,] #remove all docs without words
lda <- LDA(dtm.new, k = 10) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

###################

data <- data[1:1000,] # Perform LDA on the first 1,000 rows of data
corpus <- Corpus(VectorSource(data$answers),readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2,
                                                 removeNumbers = TRUE, removePunctuation = TRUE, stemDocument = TRUE))
rowTotals <- apply(dtm, 1, sum)  #Find the sum of words in each document
dtm.new <- dtm[rowTotals>0,] #remove all docs without words
lda <- LDA(dtm.new, k = 10) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
#####################################

lda <- LDA(dtm.new, k = 2) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
#####################################

lda <- LDA(dtm.new, k = 8) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
#####################################

lda <- LDA(dtm.new, k = 11) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
#####################################

lda <- LDA(dtm.new, k = 14) #k is the number of topics to be found

lda_td <- tidy(lda)
lda_td

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
#####################################
