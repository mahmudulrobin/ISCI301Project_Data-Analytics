

# Install the necessary packages
install.packages("tidytext")
install.packages("dplyr")
install.packages("tidyverse") 
install.packages("ggplot2")
install.packages("textdata")


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(readr)  # For reading external files
library(textdata)


#------------------ Happy Song analysis Starts from here ----------------------


# (For csv file use read_csv(), for txt file or any other plain text file use read_file())

#read file
happy_text_data <- read_file("happy_song_lyrics.txt")     #(For csv file use read_csv(), for txt file or any other plain text file use read_file())
happy_text_data

happy_text_data <- data.frame(happy_text_data)
names(happy_text_data)

# Step 4: Extract Sentences               - (Already done since it is already a text file only - still stored in another variable for better understanding)
sentences <- data.frame(happy_text_data$happy_text_data)
print(sentences)
View(sentences)

names(sentences)

#Step 5: Use of Tokenization
words_token <- data.frame(sentences) %>% unnest_tokens(word, happy_text_data.happy_text_data)
print(words_token)
View(words_token)



#Step 6: Use of Stop Words.                       
words_token <- anti_join(words_token, get_stopwords())
View(words_token)     
#See the result of not using Stop Words - of, in, an, the, and etc. has been considered as words and makes the data noise.


# -----At this point we know we have converted the text data into words ------#



# Step 7: Create a table of word frequencies

word_frequencies <- table(words_token)
print(word_frequencies)
View(word_frequencies)

#Step-8: Descending Order sort
word_frequencies <- data.frame(word_frequencies)  %>% arrange(desc(Freq))

#View the word frequencies
View(word_frequencies)      

names(word_frequencies)



#-----------Bing Sentiments to Count positive and negative sentiments Comparison ------------#

#Question-1:
#Bing Sentiment Analysis
#Count Sentiments (positive and negative) from the texts/words
bing_sentiment_count <- words_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment)

View(bing_sentiment_count)

names(bing_sentiAnalysisnt)

#Step-8: Barchart to plot and visualize sentiments (positive or negative) using word count
ggplot(bing_sentiment_count, aes(x = bing_sentiment_count$sentiment, y = bing_sentiment_count$n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Happy Song's Sentiment Analysis with Bing Lexicon", y = "Count", x = "Sentiment") +
  theme_minimal()


#Result: Happy song has 63 Positive sentiment & only 4 Negative Sentiment








# -------------------Question-2:-------------------


#Question-2:

# Afinn Sentiment Analysis

#Count how many the texts/words have same Sentiments scores (-5 to +5)
afinn_sentiment_count <- words_token %>%
  inner_join(get_sentiments("afinn")) %>% 
  count(value)  # Count words by sentiment score (AFINN's 'value' column)
# Count words by sentiment score (Must type value inside of count() because value is the column name of AFINN lexicon by default that holds the sentiment score)


View(afinn_sentiment_count)


names(afinn_sentiment_count) #Very important to get the column names to use it in graph

#Barchart to plot and visualize sentiments (positive or negative) using word count
ggplot(afinn_sentiment_count, aes(x = afinn_sentiment_count$value, y = afinn_sentiment_count$n, fill = value)) +
  geom_bar(stat = "identity") +
  labs(title = "Happy song Analysis with AFINN Sentiments", y = "Word Count", x = "Sentiment Score") +
  theme_minimal()





#LINE GRAPH For Visualizing Emotional Progress using AFINN Sentiment Analysis***
# Optional Step: Line Graph draw to see the progress of emotions
afinn_sentiment <- words_token %>%
  inner_join(get_sentiments("afinn"))  # Join with AFINN lexicon for sentiment scores

# Create a column for word number
afinn_sentiment <- afinn_sentiment %>%
  mutate(word_number = row_number())  # Sequential word number

View(afinn_sentiment)

names(afinn_sentiment)

# Plot the sentiment scores by word number using geom_line
ggplot(afinn_sentiment, aes(x = afinn_sentiment$word_number, y = afinn_sentiment$value)) +
  geom_line(color = "red", size = 1) +  # Line graph for sentiment score per word
  labs(title = "Happy song's AFINN Sentiment Scores by Word Number",
       x = "Word Number",
       y = "Sentiment Score") +
  theme_minimal()





#NRC Sentiment Analysis

nrc_sentiment <- words_token %>%
  inner_join(get_sentiments("nrc")) 
View(nrc_sentiment)

names(nrc_sentiment)

# Count NRC sentiment categories for words/texts
nrc_sentiment_count <- nrc_sentiment %>% count(sentiment)
View(nrc_sentiment_count)

names(nrc_sentiment_count)

# Barchart showing word count and NRC Sentiment categories
ggplot(nrc_sentiment_count, aes(x = nrc_sentiment_count$sentiment, y = nrc_sentiment_count$n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Happy song's Sentiment Analysis of Emotions with NRC Lexicon", y = "Word Count", x = "Sentiment Category (Emotions)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-------------------Happy Song analysis ends here --------------------












# -------------------Beatless Song analysis starts here --------------------



# (For csv file use read_csv(), for txt file or any other plain text file use read_file())

#read file
beatless_text_data <- read_file("beatless_song_lyrics.txt")     #(For csv file use read_csv(), for txt file or any other plain text file use read_file())
beatless_text_data

beatless_text_data <- data.frame(beatless_text_data)
names(beatless_text_data)

#Extract Sentences               - (Already done since it is already a text file only - still stored in another variable for better understanding)
beatless_sentences <- data.frame(beatless_text_data$beatless_text_data)
print(beatless_sentences)
View(beatless_sentences)

names(beatless_sentences)

#Use of Tokenization
beatless_words_token <- data.frame(beatless_sentences) %>% unnest_tokens(word, beatless_text_data.beatless_text_data)
print(beatless_words_token)
View(beatless_words_token)



#Use of Stop Words.                       
beatless_words_token <- anti_join(beatless_words_token, get_stopwords())
View(beatless_words_token)     
#See the result of not using Stop Words - of, in, an, the, and etc. has been considered as words and makes the data noise.


# -----At this point we know we have converted the text data into words ------#



#Create a table of word frequencies

beatless_word_frequencies <- table(beatless_words_token)
print(beatless_word_frequencies)
View(beatless_word_frequencies)

#Descending Order sort
beatless_word_frequencies <- data.frame(beatless_word_frequencies)  %>% arrange(desc(Freq))

#View the word frequencies
View(beatless_word_frequencies)      

names(beatless_word_frequencies)




#-----------Bing Sentiments to Count positive and negative sentiments Comparison ------------#

#Question-1:
#Bing Sentiment Analysis
#Count Sentiments (positive and negative) from the texts/words
beatless_bing_sentiment_count <- beatless_words_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment)

View(beatless_bing_sentiment_count)

names(beatless_bing_sentiment_count)

#Step-8: Barchart to plot and visualize sentiments (positive or negative) using word count
ggplot(beatless_bing_sentiment_count, aes(x = beatless_bing_sentiment_count$sentiment, y = beatless_bing_sentiment_count$n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Beatless Song's Sentiment Analysis with Bing Lexicon", y = "Count", x = "Sentiment") +
  theme_minimal()


#Result: Happy song has 63 Positive sentiment & only 4 Negative Sentiment








# -------------------Question-2:-------------------


#Question-2:

# Afinn Sentiment Analysis
#Count how many the texts/words have same Sentiments scores (-5 to +5)
beatless_afinn_sentiment_count <- beatless_words_token %>%
  inner_join(get_sentiments("afinn")) %>% 
  count(value)  # Count words by sentiment score (AFINN's 'value' column)
# Count words by sentiment score (Must type value inside of count() because value is the column name of AFINN lexicon by default that holds the sentiment score)


View(beatless_afinn_sentiment_count)


names(beatless_afinn_sentiment_count) #Very important to get the column names to use it in graph

#Barchart to plot and visualize sentiments (positive or negative) using word count
ggplot(beatless_afinn_sentiment_count, aes(x = beatless_afinn_sentiment_count$value, y = beatless_afinn_sentiment_count$n, fill = value)) +
  geom_bar(stat = "identity") +
  labs(title = "Beatless song Analysis with AFINN Sentiments", y = "Word Count", x = "Sentiment Score") +
  theme_minimal()





#LINE GRAPH For Visualizing Emotional Progress using AFINN Sentiment Analysis***
# Optional Step: Line Graph draw to see the progress of emotions
beatless_afinn_sentiment <- beatless_words_token %>%
  inner_join(get_sentiments("afinn"))  # Join with AFINN lexicon for sentiment scores

# Create a column for word number
beatless_afinn_sentiment <- beatless_afinn_sentiment %>%
  mutate(word_number = row_number())  # Sequential word number

View(beatless_afinn_sentiment)

names(beatless_afinn_sentiment)

# Plot the sentiment scores by word number using geom_line
ggplot(beatless_afinn_sentiment, aes(x = beatless_afinn_sentiment$word_number, y = beatless_afinn_sentiment$value)) +
  geom_line(color = "red", size = 1) +  # Line graph for sentiment score per word
  labs(title = "Beatless song's AFINN Sentiment Scores by Word Number",
       x = "Word Number",
       y = "Sentiment Score") +
  theme_minimal()





#NRC Sentiment Analysis

beatless_nrc_sentiment <- beatless_words_token %>%
  inner_join(get_sentiments("nrc")) 
View(beatless_nrc_sentiment)

names(beatless_nrc_sentiment)

# Count NRC sentiment categories for words/texts
beatless_nrc_sentiment_count <- beatless_nrc_sentiment %>% count(sentiment)
View(beatless_nrc_sentiment_count)

names(beatless_nrc_sentiment_count)

# Barchart showing word count and NRC Sentiment categories
ggplot(beatless_nrc_sentiment_count, aes(x = beatless_nrc_sentiment_count$sentiment, y = beatless_nrc_sentiment_count$n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Beatless song's Sentiment Analysis of Emotions with NRC Lexicon", y = "Word Count", x = "Sentiment Category (Emotions)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









