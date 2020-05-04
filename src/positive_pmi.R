library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(reshape)
library(schoolmath)

#load the entire data
chess_data <- fread("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00",data.table = FALSE,
                    showProgress = TRUE)
#select only successful attempts
passed = subset(chess_data,is_passed == 1)

#training data in the format of problem_id as the word (w) and user_hash as the context (c)
train_dat = select(passed,tactics_problem_id,user_hash)

#count #(w,c)
trans_data <- train_dat %>% group_by(tactics_problem_id, user_hash) %>% mutate(count= n()) #everything is 1???
#count #(w)
trans_data_vocab <- train_dat %>% group_by(tactics_problem_id) %>% mutate(count_vocab = n())
#count #(c)
trans_data_context <- train_dat %>% group_by(user_hash) %>% mutate(count_context = n())

#check whether there are any NA in trans_data_vocab$count_vocab
vec <- is.na(trans_data_vocab$count_vocab)
vec <- !vec
all(vec) #TRUE

#check whether there are any NA in trans_data_context$count_context
vec1 <- is.na(trans_data_context$count_context)
vec1 <- !vec1
all(vec1) #TRUE

#no NA in both trans_data_vocab$count_vocab and trans_data_context$count_context

#compute |D|
cardinality_D <- sum(as.numeric(trans_data_vocab$count_vocab)) + sum(as.numeric(trans_data_context$count_context))
#make a vector of |D| with the length of trans_data's row
vec_cardinality_D <- rep(cardinality_D, nrow(trans_data))
#convert the abovementioned vector into a dataframe
cardinality_D_data <- data.frame("car_D" = vec_cardinality_D)

#compute the PMI. Property : log(a*b/(c*d)) = log(a*b) - log(c*d) = log(a) + log(b) - (log(c) + log(d)) = log(a) + log(b) - log(c) - log(d)
pmi_data <- log(trans_data$count) + log(cardinality_D_data$car_D) - log(trans_data_vocab$count_vocab) - log(trans_data_context$count_context)
#convert the pmi_data into a data frame
pmi_data_frame <- data.frame("PMI_value" = pmi_data)
#if there any negative values, replace them with 0
pmi_data_frame$PMI_value[pmi_data_frame$PMI_value < 0] <- 0 #indeed, there are some negative values

#merge the pmi data with the training data
complete_data <- train_dat %>% mutate(PPMI_value = pmi_data_frame$PMI_value)
