library(quanteda)
library(dplyr)
library(tidyr)
library(data.table)

# WITHOUT STOPWORDS load in the frequency tables

# import normalized frequency tables
#### Make sure to change the file paths to where you saved the files ####

mono_no <- fread("data_no/mono_norm.csv")
bi_no <- fread("data_no/bi_norm.csv")
tri_no <- fread("data_no/tri_norm.csv")
four_no <- fread("data_no/four_norm.csv")
five_no <- fread("data_no/five_norm.csv")

colnames(bi_no) <- c("first2", "second2", "frequency")
colnames(tri_no) <- c("first3", "second3", "third3", "frequency")
colnames(four_no) <- c("first4", "second4", "third4", "fourth4", "frequency")
colnames(five_no) <- c("first5", "second5", "third5", "fourth5", "fifth5", "frequency")

# WITH STOPWORDS: load in the frequency tables

# import normalized frequency tables
#### Make sure to change the file paths to where you saved the files ####

mono <- fread("data/mono_norm.csv")
bi <- fread("data/bi_norm.csv")
tri <- fread("data/tri_norm.csv")
four <- fread("data/four_norm.csv")
five <- fread("data/five_norm.csv")

colnames(bi) <- c("first2", "second2", "frequency")
colnames(tri) <- c("first3", "second3", "third3", "frequency")
colnames(four) <- c("first4", "second4", "third4", "fourth4", "frequency")
colnames(five) <- c("first5", "second5", "third5", "fourth5", "fifth5", "frequency")

mono <- arrange(mono, desc(frequency))
mono_no <- arrange(mono_no, desc(frequency))

# create bigram_pred, trigram_pred, monogram_pred, and fivegram_pred

bigram_pred <- function(input_token, freq_matrix){
    words_temp <- input_token
    
    # based on word, choose the bigram with highest frequency
    # (since our data frame is sorted by frequency, the first one will have
    # the highest frequency)
    
    nextword <- freq_matrix[first2 == input_token]
    
    if(length(nextword[,frequency]) == 0){
        nextword = 0
    }
    # return next word
    else{
        nextword <- arrange(nextword, desc(frequency))
        nextword <- nextword[, c("second2", "frequency")]
        colnames(nextword) <- c("Prediction", "Calculated Probability")
        nextword
    }
}

trigram_pred <- function(input_token, freq_matrix){
            
        # based on word, choose the trigram with highest frequency
        # (since our data frame is sorted by frequency, the first one will have
        # the highest frequency)
        
        nextword <- freq_matrix[first3 == input_token[1] & second3 == input_token[2]
                                ]
        if(length(nextword[,frequency]) == 0){
            nextword = 0
        }
        # return next word
        else{
            nextword <- arrange(nextword, desc(frequency))
            nextword <- nextword[, c("third3", "frequency")]
            colnames(nextword) <- c("Prediction", "Calculated Probability")
            nextword
        }
}
    
fourgram_pred <- function(input_token, freq_matrix){
    
    # based on word, choose the trigram with highest frequency
    # (since our data frame is sorted by frequency, the first one will have
    # the highest frequency)
    nextword <- freq_matrix[first4 == input_token[1] &
                            second4 == input_token[2] &
                            third4 == input_token[3]
                            ]
    if(length(nextword[,frequency]) == 0){
        nextword = 0
    }
    # return next word
    else{
        nextword <- arrange(nextword, desc(frequency))
        nextword <- nextword[, c("fourth4", "frequency")]
        colnames(nextword) <- c("Prediction", "Calculated Probability")
        nextword
    }
}

fivegram_pred <- function(input_token, freq_matrix){

    # based on word, choose the trigram with highest frequency
    # (since our data frame is sorted by frequency, the first one will have
    # the highest frequency)

    nextword <- freq_matrix[first5 == input_token[1] &
                            second5 == input_token[2] &
                            third5 == input_token[3] &
                            fourth5 == input_token[4]
                            ]
    if(length(nextword[,frequency]) == 0){
        nextword = 0
    }
    # return next word
    else{
        nextword <- arrange(nextword, desc(frequency))
        nextword <- nextword[, c("fifth5", "frequency")]
        colnames(nextword) <- c("Prediction", "Calculated Probability")
        nextword
    }
    
    
}

# backoff model

backoff_stop <- function(inputtext){
    tok <- tokens(char_tolower(inputtext),
                  remove_punct = TRUE,
                  remove_twitter = TRUE,
                  remove_symbols = TRUE,
                  remove_url = TRUE
    )
    
    count <- length(tok$text1)
    result = 0
    
    if(count == 0){
        mono
    }
    else{
        if(count >= 4){
            last_four <- tail(tok$text1, 4)
            result = fivegram_pred(last_four, five)
        }
        
        if(count == 3 || result == 0){
            last_three <- tail(tok$text1, 3)
            result = fourgram_pred(last_three, four)
        }
        
        if(count == 2 || result == 0){
            last_two <- tail(tok$text1, 2)
            result = trigram_pred(last_two, tri)
        }
        
        
        if(count == 1 || result == 0){
            last_one <- tail(tok$text1, 1)
            result = bigram_pred(last_one, bi)
        }
        
        if(result == 0){
            mono
        }
        else{
            result
        }
    }
}

backoff_nostop <- function(inputtext){
    tok <- tokens(char_tolower(inputtext),
                  remove_punct = TRUE,
                  remove_twitter = TRUE,
                  remove_symbols = TRUE,
                  remove_url = TRUE
    )
    tok <- tokens_remove(tok, stopwords())
    
    count <- length(tok$text1)
    result = 0
    
    if(count == 0){
        mono
    }
    else{
        if(count >= 4){
            last_four <- tail(tok$text1, 4)
            result = fivegram_pred(last_four, five)
        }
        
        if(count == 3 || result == 0){
            last_three <- tail(tok$text1, 3)
            result = fourgram_pred(last_three, four_no)
        }
        
        if(count == 2 || result == 0){
            last_two <- tail(tok$text1, 2)
            result = trigram_pred(last_two, tri_no)
        }
        
        
        if(count == 1 || result == 0){
            last_one <- tail(tok$text1, 1)
            result = bigram_pred(last_one, bi_no)
        }
        
        if(result == 0){
            mono
        }
        else{
            result
        }
    }
}

