downloadcsv <- function(url, nastrings) {
        temp <- tempfile()
        download.file(url, temp, method = "curl")
        data <- read.csv(temp, na.strings = nastrings)
        unlink(temp)
        return(data)
}

trainurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
train <- downloadcsv(trainurl, c("", "NA", "#DIV/0!"))

testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
test <- downloadcsv(testurl, c("", "NA", "#DIV/0!"))

## Partition Data

install.packages("caret")
library(caret)

set.seed(123456)
trainset <- createDataPartition(train$classe, p = 0.8, list = FALSE)
Training <- train[trainset, ]
Validation <- train[-trainset, ]

# Feature Selection

# exclude near zero variance features
nzvcol <- nearZeroVar(Training)
Training <- Training[, -nzvcol]

# exclude columns with m40% ore more missing values exclude descriptive
# columns like name etc

cntlength <- sapply(Training, function(x) {
        sum(!(is.na(x) | x == ""))
})
nullcol <- names(cntlength[cntlength < 0.6 * length(Training$classe)])
descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                 "cvtd_timestamp", "new_window", "num_window")
excludecols <- c(descriptcol, nullcol)
Training <- Training[, !names(Training) %in% excludecols]

## Train Model

install.packages("randomForest")
library(randomForest)

rfModel <- randomForest(as.factor(classe) ~ ., data = Training, importance = TRUE, ntrees = 10)

#Model Validation

#Taining set accuracy

Training$classe<- factor(Training$classe)
install.packages("e1071")
library(e1071)

ptraining <- predict(rfModel, Training)
print(confusionMatrix(ptraining, Training$classe))

#Validation Set Accuracy

Validation$classe<- factor(Validation$classe)

pvalidation <- predict(rfModel, Validation)
print(confusionMatrix(pvalidation, Validation$classe))

# TEst set prediction

ptest <- predict(rfModel, test)
ptest

answers <- as.vector(ptest)

pml_write_files = function(x) {
        n = length(x)
        for (i in 1:n) {
                filename = paste0("problem_id_", i, ".txt")
                write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                            col.names = FALSE)
        }
}

pml_write_files(answers)















































