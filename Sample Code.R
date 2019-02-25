###############################################
# This file is used only to keep some code samples
# used in this project.
###############################################
# 0- Libraries
# 1- Load the data
# 2- Transformation
#    White space removal
#    Stemming
#    Stopword removal


# 2- Tokenization:
#    Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. 
#    Writing a function that takes a file as input and returns a tokenized version of it.
# 3- Profanaty filtering:
#    Removing profanity and other words you do not want to predict.

####################################################################
#0#  Load the libraries:
####################################################################
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
library(tokenizers)
library(RWeka)
library(ggplot2)
library(formattable)
set.seed(201902)

####################################################################
#1# Load the three text files:
####################################################################
#   Note that due the size of the data files (200MG, 196MG, and 159MG) 
#   I will use a smaller size of the files selecting lines randomly (using rbiom) 
#file_path  <- "C:/Users/raulm/Documents/John Hopkins University/#10 Data Science Capstone/Coursera-SwiftKey/final/en_US/"
file_path  <- "C:/Users/rlmartinez/Documents/John Hopkins University/#10 Data Science Capstone/Coursera-SwiftKey/final/en_US/"
file_blogs <- "en_US.blogs.txt"
file_news  <- "en_US.news.txt"
file_twitter <- "en_US.twitter.txt"

# Load the "blogs" data file
incon <- file(paste(file_path, file_blogs ,sep=""),"r")
file <- readLines(incon, encoding="UTF-8", skipNul=TRUE)
data_blogs <- file[rbinom(length(file), 1, 0.1) == 1]
close(incon)
len_blogs <- length(file)
size_blogs <- object.size(file)
max_line_blogs <- max(nchar(file))
avg_line_blogs <- mean(nchar(file))
len_blogs_sample <- length(data_blogs)
len_blogs; len_blogs_sample; len_blogs_sample/len_blogs

# Load the "news" data file
incon <- file(paste(file_path, file_news ,sep=""),"rb")
file <- readLines(incon, encoding="UTF-8", skipNul=TRUE)
data_news <- file[rbinom(length(file), 1, 0.1) == 1]
close(incon)
len_news <- length(file)
size_news <- object.size(file)
max_line_news <- max(nchar(file))
avg_line_news <- mean(nchar(file))
len_news_sample <- length(data_news)
len_news; len_news_sample; len_news_sample/len_news

# Load the "twitter" data file
incon <- file(paste(file_path, file_twitter ,sep=""),"r")
file <- readLines(incon, encoding="UTF-8", skipNul=TRUE)
data_twitter <- file[rbinom(length(file), 1, 0.05) == 1]
close(incon)
len_twitter <- length(file)
size_twitter <- object.size(file)
max_line_twitter <- max(nchar(file))
avg_line_twitter <- mean(nchar(file))
len_twitter_sample <- length(data_twitter)
len_twitter; len_twitter_sample; len_twitter_sample/len_twitter

# Now remove the temp file to reselase memory
rm(file)

# Basic metrics about the files
data_metrics <- data.frame(file_name = c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"),
                         size = c(format(size_blogs, units = "auto"), 
                                  format(size_news, units = "auto"), 
                                  format(size_twitter, units = "auto")),
                         lines = c(format(len_blogs, big.mark=","),
                                   format(len_news, big.mark=","),
                                   format(len_twitter, big.mark=",")),
                         Average_line_length = c(round(avg_line_blogs,0), 
                                                 round(avg_line_news,0), 
                                                 round(avg_line_twitter,0)),
                         max_line_length = c(format(max_line_blogs, big.mark=","), 
                                             format(max_line_news, big.mark=","), 
                                             format(max_line_twitter, big.mark=","))
                         )
# summary table
colnames(data_metrics) <- c('File Name', 'File Size', 'Number of Lines', 'Average Length of Lines', 'Maimun Length of a line') 
formattable(data_metrics)

####################################################################
# TRANSFORMATION
####################################################################

# Make sure all characters are ASCII
sum(nchar(data_blogs))
data_blogs <- iconv(data_blogs,  to="ASCII", sub="")
sum(nchar(data_blogs))

sum(nchar(data_news))
data_news <- iconv(data_news,  to="ASCII", sub="")
sum(nchar(data_news))

sum(nchar(data_twitter))
data_twitter <- iconv(data_twitter,  to="ASCII", sub="")
sum(nchar(data_twitter))

# Create the corpus to use "tm" packages
my_corpus <- VCorpus(VectorSource(paste(data_blogs, data_news, data_twitter)))

# Remove original doc to release memory
rm(data_blogs, data_news, data_twitter)

# Just to inspect the corpus
inspect(my_corpus[1])
writeLines(as.character(my_corpus[1]))

# Transformation using "tm" functions
my_corpus <- tm_map(my_corpus, removeNumbers)
my_corpus <- tm_map(my_corpus, tolower)
my_corpus <- tm_map(my_corpus, removePunctuation)
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))
my_corpus <- tm_map(my_corpus, stripWhitespace)
#my_corpus <- tm_map(my_corpus, stemDocument)
my_corpus <- tm_map(my_corpus, PlainTextDocument)


####################################################################
# Explore the corpus
####################################################################

# Create the "term-document-matrix"      
dtm <- DocumentTermMatrix(my_corpus) 
dtm
dim(dtm)
inspect(dtm[1:5, 1:4])
inspect(dtm[1000:1015, 21150:21154])

tdm <- TermDocumentMatrix(my_corpus) 
tdm
dim(tdm)
inspect(tdm[1:5, 1:4])
inspect(tdm[8000:8005, 1:4])

# Retrive the top 100 words 
dtm_top <- dtm[,findFreqTerms(x = dtm, lowfreq=100, highfreq=Inf)]
freqency <- colSums(as.matrix(dtm_top))
# select the top 100 words that appear most frequently
topWords <- as.data.frame(freqency[head(order(freqency, decreasing = T), 100)])


####################################################################
# Data Exploration
####################################################################
# World Cloud
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(my_corpus, max.words = 200, random.order = FALSE, colors=brewer.pal(8,"Dark2"))

####################################################################
# 1-gram tokenization
####################################################################
one_gramTokenizer <- function(x) NGramTokenizer(x=x, control=Weka_control(min = 1, max = 1))
one_dtm <- DocumentTermMatrix(my_corpus, control = list(tokenize = one_gramTokenizer))

dim(one_dtm)
one_dtm_sparse <- removeSparseTerms(one_dtm, sparse=0.99)
dim(one_dtm_sparse)

one_dtm_freq <- sort(colSums(as.matrix(one_dtm_sparse)),decreasing = TRUE)
one_dtm_freq_df <- data.frame(word = names(one_dtm_freq), frequency = one_dtm_freq)
head(one_dtm_freq_df, 10)

one_dtm_plot <- subset(one_dtm_freq_df, frequency > 2000)

ggplot(one_dtm_plot, aes(x=reorder(word, frequency), y=frequency)) +
       geom_bar(stat = "identity") +  coord_flip() +
       theme(legend.title=element_blank()) +
       xlab("1-Gram") + ylab("Frequency") +
       labs(title = "Frequency of 1-Grams > 2000")

####################################################################
# 2-gram tokenization
####################################################################
two_gramTokenizer <- function(x) NGramTokenizer(x=x, control=Weka_control(min = 2, max = 2))
two_dtm <- DocumentTermMatrix(my_corpus, control = list(tokenize = two_gramTokenizer))

dim(two_dtm)
two_dtm_sparse <- removeSparseTerms(two_dtm, sparse=0.99999)
dim(two_dtm_sparse)

two_dtm_freq <- sort(colSums(as.matrix(two_dtm_sparse)),decreasing = TRUE)
two_dtm_freq_df <- data.frame(word = names(two_dtm_freq), frequency = two_dtm_freq)
head(two_dtm_freq_df, 10)
dim(two_dtm_freq_df)

two_dtm_plot <- subset(two_dtm_freq_df, frequency > 100)

ggplot(two_dtm_plot, aes(x=reorder(word, frequency), y=frequency)) +
        geom_bar(stat = "identity") +  coord_flip() +
        theme(legend.title=element_blank()) +
        xlab("2-Gram") + ylab("Frequency") +
        labs(title = "Frequency of 2-Grams > 100")


####################################################################
# 3-gram tokenization
####################################################################
three_gramTokenizer <- function(x) NGramTokenizer(x=x, control=Weka_control(min = 3, max = 3))
three_dtm <- DocumentTermMatrix(my_corpus, control = list(tokenize = three_gramTokenizer))

dim(three_dtm)
three_dtm_sparse <- removeSparseTerms(three_dtm, sparse=0.9998)
dim(three_dtm_sparse)


three_dtm_freq <- sort(colSums(as.matrix(three_dtm_sparse)),decreasing = TRUE)
three_dtm_freq_df <- data.frame(word = names(three_dtm_freq), frequency = three_dtm_freq)
head(three_dtm_freq_df, 10)
dim(three_dtm_freq_df)

three_dtm_plot <- subset(three_dtm_freq_df, frequency > 10)

ggplot(three_dtm_plot, aes(x=reorder(word, frequency), y=frequency)) +
        geom_bar(stat = "identity") +  coord_flip() +
        theme(legend.title=element_blank()) +
        xlab("3-Gram") + ylab("Frequency") +
        labs(title = "Frequency of 3-Grams > 10")



####################################################################
# Calculate the size of the object using 50% or 90% of the ngrams
####################################################################
two_gramTokenizer <- function(x) NGramTokenizer(x=x, control=Weka_control(min = 2, max = 2))
two_dtm <- DocumentTermMatrix(my_corpus, control = list(tokenize = two_gramTokenizer))

dim(two_dtm)
two_dtm_sparse <- removeSparseTerms(two_dtm, sparse=0.9997)
dim(two_dtm_sparse)

two_dtm_freq <- sort(colSums(as.matrix(two_dtm_sparse)),decreasing = TRUE)
length(two_dtm_freq)
two_dtm_freq_df <- data.frame(word = names(two_dtm_freq), frequency = two_dtm_freq)
head(two_dtm_freq_df, 10)
dim(two_dtm_freq_df)
object.size(two_dtm_freq_df)

# Basic metrics for the bigrams dataframe
bigrams_metrics <- data.frame(file_name = c("Bigrams 50%","Bigrams 75%","Bigrams 90%","Bigrams 100%"),
                           size = c(format(object.size(two_dtm_freq_df[1:round(nrow(two_dtm_freq_df)*0.5),]), units="auto"), 
                                    format(object.size(two_dtm_freq_df[1:round(nrow(two_dtm_freq_df)*0.75),]),units="auto"), 
                                    format(object.size(two_dtm_freq_df[1:round(nrow(two_dtm_freq_df)*0.90),]),units="auto"), 
                                    format(object.size(two_dtm_freq_df[1:round(nrow(two_dtm_freq_df)*1.0),]), units="auto")
                                    )
                                   )

# summary table
colnames(bigrams_metrics) <- c('Bigrams Data', 'File Size') 
formattable(bigrams_metrics)



####################################################################
####################################################################


my_corpus_df <- data.frame(text=unlist(sapply(my_corpus, `[`, "content")),stringsAsFactors=F)

qq <- trigrams_data %>% filter_(~word1 == "about", ~word2 == "this")
woord <- trigrams_data %>% filter_(~word1 == "about", ~word2 == "this") 
woord <- trigrams_data %>% filter_(~word1 == "imag", ~word2 == "will") 
woord <- sample_n(woord, 1, weight = n) %>% .[["word3"]]

trigrams_data %>% filter_(~word1 == "id", ~word2 == "give")
trigrams_data %>% filter_(~word1 == "id", ~word2 == "give") %>% .[["n"]]
bigrams_data %>% filter_(~word1 == "id", ~word2 == "give") %>% .[["n"]]
bigrams_data %>% filter_(~word1 == "id", ~word2 == "give")

bigrams_data %>% filter_(~word1 == "id")
bigrams_data %>% filter_(~word1 == "id", ~word2 == "give")

return_third_word("idx", "idx")


####################################################################
####################################################################
####################################################################
####################################################################



# Count the number of lines
length(data_blogs); length(data_news); length(data_twitter)

# Find the longest line
max(nchar(data_blogs)); max(nchar(data_news)); max(nchar(data_twitter))

# Find number of occurences of a word
twitter_love <- grep("love", data_twitter)
twitter_hate <- grep("hate", data_twitter)
length(twitter_love)/length(twitter_hate)

# Show the  lines that has a specific word
data_twitter[grep("biostats", data_twitter)]

# count the lines that has a specific words
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", data_twitter))









# Sample Tokenize

data("crude")
Boost_tokenizer(crude[[1]])
MC_tokenizer(crude[[1]])
scan_tokenizer(crude[[1]])
strsplit_space_tokenizer <- function(x)
        unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(crude[[1]])
