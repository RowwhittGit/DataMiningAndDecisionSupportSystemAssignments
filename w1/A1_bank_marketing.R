## --------------------------------------------------------------
##  A1_bank_marketing.R  –  Bank Marketing Decision-Tree
##  (Works in Posit Cloud, RStudio, or any plain R console)
## --------------------------------------------------------------

# 1. Load the data ------------------------------------------------
Mydata <- read.csv("bank-additional.csv", sep = ";")

# 2. Quick visual checks -----------------------------------------
#   Job distribution
barplot(table(Mydata$job), 
        main = "Job Distribution", 
        col  = "steelblue", 
        las  = 2)          # rotate x-labels

#   Outcome (y) distribution
barplot(table(Mydata$y), 
        main = "Call Outcome (y)", 
        col  = c("salmon","lightgreen"))

# 3. Remove the "duration" variable -------------------------------
Mydata <- subset(Mydata, select = -duration)

# 4. Train / validation split (80 % / 20 %) ----------------------
set.seed(123)                                   # reproducibility
n      <- nrow(Mydata)
train_idx <- sort(sample(n, round(0.8 * n)))     # 80 % rows

Train <- Mydata[ train_idx, ]
Val   <- Mydata[-train_idx, ]

cat("Training rows :", nrow(Train), "\n")
cat("Validation rows:", nrow(Val),   "\n")

# 5. Decision-Tree model -----------------------------------------
library(rpart)                                   # decision trees

mtree <- rpart(y ~ ., 
               data    = Train, 
               method  = "class",
               control = rpart.control(minsplit = 2, cp = 0.001))

# 5a. Display complexity table
printcp(mtree)

# 5b. Plot the tree (basic)
plot(mtree, uniform = TRUE, margin = 0.1)
text(mtree, use.n = TRUE, all = TRUE, cex = 0.8)

# 6. Predictions on validation set -------------------------------
Yt <- predict(mtree, newdata = Val, type = "class")

# 7. Confusion matrix --------------------------------------------
conf_matrix <- table(Actual = Val$y, Predicted = Yt)
rownames(conf_matrix) <- paste("Actual:", rownames(conf_matrix))
colnames(conf_matrix) <- paste("Pred:",   colnames(conf_matrix))
print(conf_matrix)

# 8. Performance metrics -----------------------------------------
accuracy <- mean(Val$y == Yt)                     # overall accuracy

# True-positive rate (recall for "yes")
tp_rate <- sum(Val$y == "yes" & Yt == "yes") / sum(Val$y == "yes")

# True-negative rate (recall for "no")
tn_rate <- sum(Val$y == "no" & Yt == "no")   / sum(Val$y == "no")

# False-positive rate (1 - specificity)
fp_rate <- sum(Val$y == "no" & Yt == "yes")  / sum(Val$y == "no")

# False-negative rate (1 - recall for "yes")
fn_rate <- sum(Val$y == "yes" & Yt == "no")  / sum(Val$y == "yes")

cat("\n=== Performance ===\n")
cat("Accuracy          :", round(accuracy, 4), "\n")
cat("True-positive rate:", round(tp_rate,   4), "\n")
cat("True-negative rate:", round(tn_rate,   4), "\n")
cat("False-positive rate:",round(fp_rate,   4), "\n")
cat("False-negative rate:",round(fn_rate,   4), "\n")
