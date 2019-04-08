install.packages("NLP")
library(tm)
library(tidytext)

#collapse into a one line text and switch text to lower case
review_text <- paste(Sample$full)
review_text <- tolower(review_text)
review_text

#split the vector at non-word characters 
split <- unlist(strsplit(review_text, "\\W+"))
split

#generate a table of frequncies and see how many times each word type occurs. Table() sorts in alphabetical order.
table.split <- table(split)
head(table.split)

#sort in decreasing order.
sorted.table.split <- sort(table.split, decreasing = TRUE)
head(sorted.table.split, 15)
#the 15 most frequent words are not interesting because they consist of closed-class words, i.e. words that serve a grammatical function.
#Getting rid of grammatical words would have the advantage of focusing on open class words (lexical words).
tail(sorted.table.split, 15)
#the bottom of the list consists of unusual characters, rare words or figures. 

#strplit() splits at non-words character. So words like "can't" are split into "can" and "t". 

#________________________________________
library(dplyr)
reviews <- tibble(Sample)

#break the text into individual tokens
reviews <- Sample %>%
  unnest_tokens(word, full)
head(sort(table(reviews), decreasing = TRUE), 15) #the 15 most frequent words are not interesting because they consist of closed-class words, 
#i.e. words that serve a grammatical function.Getting rid of grammatical words would have the advantage of focusing on open class words (lexical words).


reviews <- reviews %>% filter(!word %in% stopwords("italian"))
frequency <- reviews %>% count(word, sort = TRUE)

library(scales)
library(ggplot2)

reviews %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
