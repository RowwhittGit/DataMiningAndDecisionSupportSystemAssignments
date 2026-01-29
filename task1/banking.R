## Load required libraries
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
install.packages("caret")


##Load bankin dataset
bank <- read_csv("bank.csv")
head(bank)

##dataset summary evidence 1
str(bank)
summary(bank)


## identify numerical columns
numeric_cols <- sapply(bank, is.numeric)
numeric_cols



### Replace NA values with mean (NUMERIC ONLY)
bank_clean <- bank

bank_clean[, numeric_cols] <- lapply(
  bank_clean[, numeric_cols],
  function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  }
)


##  NA removal verification
colSums(is.na(bank_clean))


##deposit is the target column
bank_clean$deposit <- factor(bank_clean$deposit, levels = c("no", "yes"))


##Train and split data

set.seed(123)
index <- createDataPartition(bank_clean$deposit, p = 0.7, list = FALSE)
train <- bank_clean[index, ]
test  <- bank_clean[-index, ]



##builidng decision tree
tree_model <- rpart(
  deposit ~ .,
  data = train,
  method = "class"
)

##Showing tree
rpart.plot(tree_model)


##prediction and evaluation

pred <- predict(tree_model, test, type = "class")

conf_mat <- confusionMatrix(pred, test$deposit)
conf_mat
conf_mat$table
