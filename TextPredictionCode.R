# Source: https://www.r-bloggers.com/generate-text-using-markov-chains-sort-of/

library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)

# Convert corpus into a dataframe
my_corpus_df <- data.frame(text=unlist(sapply(my_corpus, `[`, "content")),stringsAsFactors=F)
dim(my_corpus_df)
rm(my_corpus)
object.size(my_corpus_df)

# Extracting N-grams 
unigrams_data <- my_corpus_df %>%
        unnest_tokens(unigram, text, token = "ngrams",to_lower = TRUE, n= 1) %>%
        separate(unigram, "word1", sep = " ") %>%
        count(word1, sort = TRUE) %>%
        filter(n > 1)
object.size(unigrams_data)

bigrams_data <- my_corpus_df %>%
        unnest_tokens(bigram, text, token = "ngrams",to_lower = TRUE, n= 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        count(word1, word2, sort = TRUE) %>%
        filter(n > 1)
object.size(bigrams_data)

trigrams_data <- my_corpus_df %>%
        unnest_tokens(trigram, text, token = "ngrams",to_lower = TRUE, n= 3) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        count(word1, word2,word3, sort = TRUE) %>%
        filter(n > 1)
object.size(trigrams_data)

cuagrams_data <- my_corpus_df %>%
        unnest_tokens(cuagram, text, token = "ngrams",to_lower = TRUE, n= 4) %>%
        separate(cuagram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
        count(word1, word2,word3,word4, sort = TRUE) %>%
        filter(n > 1)
object.size(cuagrams_data)

################################################
# Creating a markov chain 
################################################

#Find the next words
return_next_word <- function( woord1, woord2="", woord3=""){
        woord <- ""
        print("1")
        if (nchar(woord1) > 0 & nchar(woord2) > 0 & nchar(woord3) > 0) {
                print("1.1")
                woord <- cuagrams_data %>% filter_(~word1 == woord1, ~word2 == woord2, ~word3 == woord3)
                if (nrow(woord) > 0) {
                        woord <- sample_n(woord, 1, weight = n) %>% .[["word4"]]
                }
                else {
                        woord <- ""
                }
        }
        print("2")
        if (nchar(woord) == 0 & nchar(woord2) > 0 & nchar(woord3) > 0 ) {
                print("2.1")
                woord <- return_third_word(woord2, woord3)
                if (nchar(woord) == 0) {woord <- ""}
        }
        print("3")
        if (nchar(woord) == 0 & nchar(woord1) > 0 & nchar(woord2) > 0 & nchar(woord3) == 0 ) {
                print("3.1")
                woord <- return_third_word(woord1, woord2)
                if (nchar(woord) == 0) {woord <- ""}
        }
        print("4")
        if (nchar(woord) == 0 & nchar(woord1) > 0 & nchar(woord2) == 0 & nchar(woord3) == 0  ) {
                print("4.1")
                woord <- return_third_word(woord1)
        }
        woord
}


return_third_word <- function( woord1, woord2){
        woord <- trigrams_data %>% filter_(~word1 == woord1, ~word2 == woord2)
        if(nrow(woord) > 0){
                woord <- sample_n(woord, 1, weight = n) %>% .[["word3"]]
        } else {
                woord <- filter_(bigrams_data, ~word1 == woord2)
                if (nrow(woord) > 0) {
                        woord <- sample_n(woord, 1, weight = n) %>% .[["word2"]]        
                } else {
                        woord <- return_uni_word(woord2)
                }        
        }
        woord
}


return_uni_word <- function(woord1=""){
        woord <- unigrams_data %>% filter_(~word1 == woord1)
        if(nrow(woord) > 0){
                woord <- sample_n(woord, 1, weight = n) %>% .[["word1"]]
        }
        if (nchar(woord) == 0) {
                woord <- unigrams_data[["word1"]][1]
        }
        woord
}


#Calculate the uni-score
return_uni_score <- function(woord1){
        uni_score <- 0.0
        uni_word1      <- unigrams_data %>% filter_(~word1 == woord1)
        count_uniwords <- nrow(unigrams_data)
        
        if(count_uniwords > 0){
                if (nrow(uni_word1) > 0) {
                        uni_score <- uni_word1[['n']] / count_uniwords
                }        
        }
        uni_score
}


#Calculate the proba
return_second_word_proba <- function( woord1, woord2){
        second_word_proba <- 0.0
        word_1_list   <- bigrams_data %>% filter_(~word1 == woord1)
        word_1_2_list <- bigrams_data %>% filter_(~word1 == woord1, ~word2 == woord2)
        
        if(nrow(word_1_list) > 0){
                if (nrow(word_1_2_list) == 1) {
                        second_word_proba <- word_1_2_list["n"] / nrow(word_1_list)
                }        
        }
        second_word_proba
}

return_third_word_proba <- function( woord1, woord2, woord3){
        third_word_proba <- 0.0
        word_1_2_list   <- trigrams_data %>% filter_(~word1 == woord1, ~word2 == woord2)
        word_1_2_3_list <- trigrams_data %>% filter_(~word1 == woord1, ~word2 == woord2, ~word3 == woord3)
        
        if(nrow(word_1_2_list) > 0){
                if (nrow(word_1_2_3_list) == 1) {
                        third_word_proba <- word_1_2_3_list["n"] / nrow(word_1_2_list)
                }        
        }
        third_word_proba
}

# Create the corpus a text
line_corpus <- VCorpus(VectorSource("When you breathe, I want to be the air for you. I’ll be there for you, I’d live and I’d"))

# Just to inspect the corpus
inspect(line_corpus[1])
writeLines(as.character(line_corpus[1]))

# Transformation using "tm" functions
line_corpus <- tm_map(line_corpus, removeNumbers)
line_corpus <- tm_map(line_corpus, tolower)
line_corpus <- tm_map(line_corpus, removePunctuation)
line_corpus <- tm_map(line_corpus, removeWords, stopwords("english"))
line_corpus <- tm_map(line_corpus, stripWhitespace)
line_corpus <- tm_map(line_corpus, stemDocument)
line_corpus <- tm_map(line_corpus, PlainTextDocument)

# Convert corpus into a dataframe
line_corpus_df <- data.frame(text=unlist(sapply(line_corpus, `[`, "content")),stringsAsFactors=F)
dim(line_corpus_df)
object.size(line_corpus_df)
line_corpus_df[1,1]