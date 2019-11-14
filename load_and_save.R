# Change the working directory to where you initially saved the txt files
setwd("C:/Users/Chris Han/wordprediction/final/en_US")

library(quanteda)
library(dplyr)
library(tidyr)
library(data.table)
require(readtext)

# GOAL: train on 70% of data, 20% hold out, 10% test

# read in data
twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8")
blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8")
news <- readLines("en_US.news.txt", encoding = "UTF-8")

# sample 70% only
set.seed(1510)
twitter_index <- sample(1:length(twitter), length(twitter) * 0.7)
twitter_index_matrix <- split(twitter_index, 1:7)

set.seed(2000)
blogs_index <- sample(1:length(blogs), length(blogs) * 0.7)
blogs_index_matrix <- split(blogs_index, 1:7)

set.seed(20)
news_index <- sample(1:length(news), length(news) * 0.7)
news_index_matrix <- split(news_index, 1:7)

make_corpus <- function(index = 1){
    twitter_corpus <- corpus(twitter[twitter_index_matrix[[index]]])
    blogs_corpus <- corpus(blogs[blogs_index_matrix[[index]]])
    news_corpus <- corpus(news[news_index_matrix[[index]]])
    
    # consolidate documents into one for each
    twitter_corpus <- corpus(texts(twitter_corpus, groups = rep(1, ndoc(twitter_corpus))))
    blogs_corpus <- corpus(texts(blogs_corpus, groups = rep(1, ndoc(blogs_corpus))))
    news_corpus <- corpus(texts(news_corpus, groups = rep(1, ndoc(news_corpus))))
    
    combined_corpus <- corpus(c(twitter_corpus, blogs_corpus, news_corpus), 
                              docnames = c("twitter", "blogs", "news"))
    combined_corpus
}

# create dfm for 1,2,3,4,5-gram
# tok_no_stop <- tokens_remove(tok, stopwords())

create_token <- function(input_corpus){
    tok <- tokens(input_corpus, 
              remove_numbers = TRUE,
              remove_punct = TRUE,
              remove_hyphens = TRUE,
              remove_symbols = TRUE,
              remove_twitter = TRUE,
              remove_url = TRUE
              )
    tok <- tokens_remove(tok, stopwords())
}

# monogram

make_monogram_freq <- function(tok, filename){
    en_dfm <- dfm(tok)
    monogram_freq <- textstat_frequency(en_dfm) %>%
        filter(frequency > 1) %>%
            select(feature, frequency)

    fwrite(monogram_freq, filename)

    rm(en_dfm, monogram_freq)
}

make_bigram_freq <- function(tok, filename){
    en_dfm_2 <- dfm(tok,
                    ngrams = 2
    )
    
    bigram_freq <- as.data.frame(textstat_frequency(en_dfm_2)) %>% 
        filter(frequency > 1) %>%
        select(feature, frequency) %>% 
        separate(feature, c("first", "second"), sep = "_")
    
    fwrite(bigram_freq, filename)
    
    rm(en_dfm_2, bigram_freq)
}

make_trigram_freq <- function(tok, filename){
    en_dfm_3 <- dfm(tok,
                    ngrams = 3
    )
    
    trigram_freq <- as.data.frame(textstat_frequency(en_dfm_3)) %>% 
        filter(frequency > 1) %>%
        select(feature, frequency) %>%
        separate(feature, c("first", "second", "third"), sep = "_")
    
    fwrite(trigram_freq, filename)
    
    rm(en_dfm_3, trigram_freq)
}

make_fourgram_freq <- function(tok, filename){
    en_dfm_4 <- dfm(tok,
                    ngrams = 4
    )
    
    fourgram_freq <- as.data.frame(textstat_frequency(en_dfm_4)) %>%
        filter(frequency > 1) %>%
        select(feature, frequency) %>%
        separate(feature, c("first", "second", "third", "fourth"), sep = "_")
    
    fwrite(fourgram_freq, filename)
    
    rm(en_dfm_4, fourgram_freq)
}

make_fivegram_freq <- function(tok, filename){
    en_dfm_5 <- dfm(tok,
                    ngrams = 5
    )
    
    fivegram_freq <- as.data.frame(textstat_frequency(en_dfm_5)) %>%
        filter(frequency > 1) %>%
        select(feature, frequency) %>%
        separate(feature, c("first", "second", "third", "fourth", "fifth"), 
                 sep = "_")
    
    fwrite(fivegram_freq, filename)
    rm(fivegram_freq, en_dfm_5)
}

# skipgram

make_skipgram_freq <- function(tok, filename){
    skip_dfm_2 <- dfm(tok,
                skip = 1,
                ngrams = 2)

    skip_freq <- as.data.frame(textstat_frequency(skip_dfm_2)) %>% 
    filter(frequency > 1) %>%
        select(feature, frequency) %>%
            separate(feature, c("first", "second"), sep = "_")

    fwrite(skip_freq, filename)

    rm(skip_dfm_2, skip_freq)
}

# repeat 7 times

# Change this to where you want to save your files
setwd("C:/Users/Chris Han/wordprediction/data_no")

one <- c("mono1.csv", "bi1.csv", "tri1.csv", "four1.csv", "five1.csv")
two <- c("mono2.csv", "bi2.csv", "tri2.csv", "four2.csv", "five2.csv")
three <- c("mono3.csv", "bi3.csv", "tri3.csv", "four3.csv", "five3.csv")
four <- c("mono4.csv", "bi4.csv", "tri4.csv", "four4.csv", "five4.csv")
five <- c("mono5.csv", "bi5.csv", "tri5.csv", "four5.csv", "five5.csv")
six <- c("mono6.csv", "bi6.csv", "tri6.csv", "four6.csv", "five6.csv")
seven <-c("mono7.csv", "bi7.csv", "tri7.csv", "four7.csv", "five7.csv")


# function to create data

create_data <- function(index, filenames){
    combined_corpus <- make_corpus(index)
    tok <- create_token(combined_corpus)
    
    make_monogram_freq(tok, filenames[1])
    gc()
    make_bigram_freq(tok, filenames[2])
    gc()
    make_trigram_freq(tok, filenames[3])
    gc()
    make_fourgram_freq(tok, filenames[4])
    gc()
    make_fivegram_freq(tok, filenames[5])
    gc()
}


create_data(1, one)
gc()

create_data(2, two)
gc()

create_data(3, three)
gc()

create_data(4, four)
gc()

create_data(5, five)
gc()

create_data(6, six)
gc()

create_data(7, seven)
gc()

# function to merge files

merge_files <- function(i){
    a <- fread(one[i])
    b <- fread(two[i])
    c <- fread(three[i])
    d <- fread(four[i])
    e <- fread(five[i])
    f <- fread(six[i])
    g <- fread(seven[i])
    
    comb <- Reduce(function(...) merge(..., all = T), list(a,b,c,d,e,f,g))
    comb
}

# monograms

mono_comb <- merge_files(1)
mono_comb <- mono_comb %>%
    group_by(feature) %>%
    summarise(frequency = sum(frequency))

fwrite(mono_comb, "monogram.csv")

# bigrams
bi_comb <- merge_files(2)
bi_comb <- bi_comb %>%
    group_by(first, second) %>%
    summarise(frequency = sum(frequency))

fwrite(bi_comb, "bigram.csv")

# trigrams
tri_comb <- merge_files(3)
tri_comb <- tri_comb %>%
    group_by(first, second, third) %>%
        summarise(frequency = sum(frequency))
fwrite(tri_comb, "trigram.csv")

# fourgrams

four_comb <- merge_files(4) %>%
    group_by(first, second, third, fourth) %>%
        summarise(frequency = sum(frequency))
fwrite(four_comb, "fourgram.csv")
# fivegrams

five_comb <- merge_files(5) %>%
    group_by(first, second, third, fourth, fifth) %>%
        summarise(frequency = sum(frequency))
fwrite(five_comb, "fivegram.csv")

# test set

test_blogs <- blogs[-blogs_index]
test_news <- news[-news_index]
test_twitter <- twitter[-twitter_index]





