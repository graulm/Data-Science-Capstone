# Source: https://www.r-bloggers.com/generate-text-using-markov-chains-sort-of/

library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)

# Convert corpus into a dataframe
my_corpus_df <- data.frame(text=unlist(sapply(my_corpus, `[`, "content")),stringsAsFactors=F)
dim(my_corpus_df)

# Extracting bigrams and trigrams 
bigrams_data <- my_corpus_df %>%
        unnest_tokens(bigram, text, token = "ngrams",to_lower = TRUE, n= 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        count(word1, word2, sort = TRUE) %>%
        filter(n > 10)

trigrams_data <- my_corpus_df %>%
        unnest_tokens(trigram, text, token = "ngrams",to_lower = TRUE, n= 3) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        count(word1, word2,word3, sort = TRUE) %>%
        filter(n > 10)

################################################
# Creating a markov chain 
################################################

#Find the next words
return_third_word <- function( woord1, woord2){
        woord <- trigrams_data %>% filter_(~word1 == woord1, ~word2 == woord2)
        if(nrow(woord) > 0){
                woord <- sample_n(woord, 1, weight = n) %>% .[["word3"]]
        } else {
                woord <- filter_(bigrams_data, ~word1 == woord2)
                if (nrow(woord) > 0) {
                        woord <- sample_n(woord, 1, weight = n) %>% .[["word2"]]        
                } else {
                        woord <- "Miau..."
                }        
        }
        woord
}



