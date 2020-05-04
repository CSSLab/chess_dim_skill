library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(reshape)
library(schoolmath)

chess_data <- fread("/w/225/1/chess/tactics/glicko_user_tactics_problem.csv_00",data.table = FALSE,
                    showProgress = TRUE)

passed = subset(chess_data,is_passed == 1)
train_dat = select(passed,tactics_problem_id,user_hash)

trans_data <- train_dat %>% group_by(tactics_problem_id, user_hash) %>% mutate(count= n()) #everything is 1???
trans_data_vocab <- train_dat %>% group_by(tactics_problem_id) %>% mutate(count_vocab = n())
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

cardinality_D <- sum(as.numeric(trans_data_vocab$count_vocab)) + sum(as.numeric(trans_data_context$count_context))
vec_cardinality_D <- rep(cardinality_D, nrow(trans_data))
cardinality_D_data <- data.frame("car_D" = vec_cardinality_D)
pmi_data <- log(trans_data$count) + log(cardinality_D_data$car_D) - log(trans_data_vocab$count_vocab) - log(trans_data_context$count_context)
pmi_data_frame <- data.frame("PMI_value" = pmi_data)
pmi_data_frame$PMI_value[pmi_data_frame$PMI_value < 0] <- 0 #indeed, there are some negative values

complete_data <- train_dat %>% mutate(PPMI_value = pmi_data_frame$PMI_value)
