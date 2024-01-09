# Φόρτωση των απαραίτητων βιβλιοθηκών
library(caret)
library(pROC)
library(ROSE)
library(kknn)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 6) {
  stop("Δεν δόθηκαν αρκετές παράμετροι. Χρησιμοποιήστε --param1=value1 --param2=value2 --param3=value3 --param4=value4 --param5=value5 --param6=value6")
}

# Ανάλυση των παραμέτρων
param1 <- NULL
param2 <- NULL
param3 <- NULL
param4 <- NULL
param5 <- NULL
param6 <- NULL

for (arg in args) {
  parts <- strsplit(arg, "=")[[1]]
  param_name <- tolower(parts[1])
  param_value <- parts[2]
  
  if (param_name == "--param1") {
    param1 <- param_value
  } else if (param_name == "--param2") {
    param2 <- param_value
  } else if (param_name == "--param3") {
    param3 <- param_value
  }else if (param_name == "--param4") {
    param4 <- as.numeric(param_value)
  } else if (param_name == "--param5") {
    param5 <- as.numeric(param_value)
  }else if (param_name == "--param6") {
    param6 <- param_value
  }
}

if (is.null(param1) || is.null(param2) || is.null(param3)  || is.null(param4) || is.null(param5) || is.null(param6) ) {
  stop("Λείπουν ορισμένες παράμετροι. Χρησιμοποιήστε --param1=value1 --param2=value2 --param3=value3 --param4=value4 --param5=value5 --param6=value6")
} 

file_pathtraining <- param1

if (file.exists(file_pathtraining)) {
  datatraining <- read.csv(file_pathtraining, header = TRUE, stringsAsFactors = FALSE)
  print("Τα δεδομένα διαβάστηκαν επιτυχώς.")
} else {
  print("Το αρχείο δεν βρέθηκε.")
}
file_pathtest <- param2

if (file.exists(file_pathtest)) {
  datatest <- read.csv(file_pathtest, header = TRUE, stringsAsFactors = FALSE)
  print("Τα δεδομένα διαβάστηκαν επιτυχώς.")
} else {
  print("Το αρχείο δεν βρέθηκε.")
}

datatraining$REFACTORED[datatraining$REFACTORED == 0] <- 'no'
datatraining$REFACTORED[datatraining$REFACTORED == 1] <- 'yes'

datatest$REFACTORED[datatest$REFACTORED == 0] <- 'no'
datatest$REFACTORED[datatest$REFACTORED == 1] <- 'yes'

# Υποδείγματος (undersampling) για ισορροπημένο σύνολο δεδομένων
indices_no <- which(datatraining$REFACTORED == 'no')
indices_yes <- which(datatraining$REFACTORED == 'yes')

le <- min(length(indices_yes),length(indices_no))
set.seed(12)
indices_no_undersampled <- sample(indices_no, le)
indices_yes <- sample(indices_yes, le)

data_b <- rbind(datatraining[indices_yes, ], datatraining[indices_no_undersampled, ])
set.seed(1234)  # Ορίζουμε seed για ανακατανομή των αποτελεσμάτων
data_b <- data_b[sample(nrow(data_b)), ]

data_btraining <- data_b[, -c(1:4)]  # Αφαίρεση των στηλών projectName, SHA, file, rank
data_btraining$REFACTORED <- as.factor(data_btraining$REFACTORED)

test <- datatest[, -c(1:4)]  # Αφαίρεση των στηλών projectName, SHA, file, rank
test$REFACTORED <- as.factor(test$REFACTORED)

file_name <- paste0(param3, "_kknn.json")

sink(file_name, type = "output")

#the cross-validation
trctrl <- trainControl(method = 'repeatedcv', number = 3, repeats = 3)

#kknn model
tuneGrid <- expand.grid(kmax = param4,            
                        distance = param5,        
                        kernel = c(param6))

set.seed(56)
kknn_fit <- train(REFACTORED ~ ., 
                  data = data_btraining,
                  method = 'kknn',
                  trControl = trctrl,
                  tuneGrid = tuneGrid,
                  tuneLength = 10)
#knn model
knn.best <- kknn(REFACTORED ~ .,
                 train=data_btraining,
                 test = test,
                 k=param4,
                 distance = param5,
                 kernel = param6)


#print(knn.best$C)
write.csv(data_b, "trainingdata.csv", row.names = FALSE)
sorted_results <- kknn_fit$results[order(-kknn_fit$results$Accuracy), ]
print(sorted_results)


# Πρόβλεψη και αξιολόγηση του τελικού μοντέλου στα δεδομένα ελέγχου
pred <- predict(kknn_fit, newdata = test)

results_df <- data.frame(
  Test = test$REFACTORED,
  Predicted = pred
)

# Εμφάνιση των προβλέψεων για κάθε παρατήρηση
print(results_df)

# Υπολογισμός της πίνακα σύγχυσης
conf_matrix <- confusionMatrix(pred, test$REFACTORED, positive = 'yes')
accuracy <- conf_matrix$overall['Accuracy']
kappa <- conf_matrix$overall['Kappa']
f1 <- 2 * (conf_matrix$byClass['Sensitivity'] * conf_matrix$byClass['Pos Pred Value']) / 
  (conf_matrix$byClass['Sensitivity'] + conf_matrix$byClass['Pos Pred Value'])

# Εκτύπωση των μετρικών
print(paste("Accuracy: ", accuracy,"Kappa: ", kappa,"F1-score: ", f1))
conf_matrix

sink()
save(kknn_fit, file = "kknn_model.RData")

save(knn.best, file = "knnmodel.RData")

