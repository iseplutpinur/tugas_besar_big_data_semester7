# clear console
cat("\014")

# Step 1: Import the data ======================================================
# titanic = read.csv('D:\\Kampus\\big data titanic\\titanic.csv')
titanic = read.csv('https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv')
head(titanic) # data paling atas
tail(titanic) # data paling bawah
str(titanic) # strukutr data
summary(titanic) # tingkasan data

# data nya di acak terlebih dahulu
shuffle_index <- sample(1:nrow(titanic))
# sample(1:nrow(titanic)): Generate a random list of index from 1 to 1309 (i.e. the maximum number of rows).
head(shuffle_index)


titanic <- titanic[shuffle_index, ]
head(titanic)


# Step 2) Clean the dataset ====================================================
library(dplyr)

# mengatasi missing value dengan mean value
for(i in 1:ncol(titanic)){
  titanic[is.na(titanic[,i]),i]<- mean(titanic[,i],na.rm = TRUE)
}

# Drop variables
clean_titanic <- titanic %>%
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  #Convert to factor level
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()
glimpse(clean_titanic)

# ganti tipe data yang character menjadi factor
clean_titanic <- clean_titanic %>% 
  mutate(across(where(is.character), as.factor))


# Step 3) Create train/test set ================================================
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample < - 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE)
dim(data_train)


prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

# Step 4) Build the model ======================================================
library(rpart)
library(rpart.plot)
data_train <- data_train %>% 
  mutate(across(where(is.character), as.factor))


fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)
