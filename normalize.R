# Normalize the Frequency Tables

library(quanteda)
library(dplyr)
library(tidyr)
library(data.table)

# load in the frequency tables
# Change this to where you saved the frequency tables
setwd("C:/Users/Chris Han/finalproject/data_no")

monogram <- fread("monogram.csv")[frequency > 4]
mono_norm <- fread("monogram.csv")[frequency > 4]
mono_norm <- mono_norm[, frequency := frequency / sum(frequency)]
fwrite(mono_norm, "mono_norm.csv")

bigram <- fread("bigram.csv")[frequency > 4]
colnames(bigram) <- c("first1", "second1", "frequency")
bi_norm <- fread("bigram.csv")[frequency > 4]
bi_norm$frequency <- as.double(bi_norm$frequency)

bi_norm[, frequency := frequency / monogram[feature == first, frequency], by = first]
fwrite(bi_norm, "bi_norm.csv")


trigram <- fread("trigram.csv")[frequency > 4]
colnames(trigram) <- c("first1", "second1", "third1", "frequency")
tri_norm <- fread("trigram.csv")[frequency > 4]
tri_norm$frequency <- as.double(tri_norm$frequency)
tri_norm[, frequency := frequency / bigram[first1 == first & second1 == second, frequency], by = c("first","second")]
fwrite(tri_norm, "tri_norm.csv")

fourgram <- fread("fourgram.csv")[frequency > 4]
colnames(fourgram) <- c("first1", "second1", "third1", "fourth1", "frequency")
four_norm <- fread("fourgram.csv")[frequency > 4]
four_norm$frequency <- as.double(four_norm$frequency)
four_norm[, frequency := frequency / trigram[first1 == first & second1 == second & third1 == third, frequency], 
          by = c("first","second", "third")]
fwrite(four_norm, "four_norm.csv")


fivegram <- fread("fivegram.csv")[frequency > 4]
five_norm <- fread("fivegram.csv")[frequency > 4]
five_norm$frequency <- as.double(five_norm$frequency)
five_norm[, frequency := frequency / fourgram[first1 == first & 
                                                  second1 == second & 
                                                  third1 == third & 
                                                  fourth1 == fourth, frequency], 
          by = c("first","second", "third", "fourth")]
fwrite(five_norm, "five_norm.csv")
