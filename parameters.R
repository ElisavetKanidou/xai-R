# Φόρτωση των απαραίτητων βιβλιοθηκών
library(caret)
library(pROC)
library(ROSE)
library(kknn)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Δεν δόθηκαν αρκετές παράμετροι. Χρησιμοποιήστε --param1=value1 --param2=value2 --param3=value3")
}

# Ανάλυση των παραμέτρων
param1 <- NULL
param2 <- NULL
param3 <- NULL

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
  }
}

if (is.null(param1) || is.null(param2) || is.null(param3) ) {
  stop("Λείπουν ορισμένες παράμετροι. Χρησιμοποιήστε --param1=value1 --param2=value2 --param3=value3 ")
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
datatraining <- subset(datatraining, select = -NOCC)
datatest <- subset(datatest, select = -NOCC)
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

file_name <- paste0(param3, "_parameters.json")

# Χρησιμοποιήστε τη συνάρτηση sink με το όνομα του αρχείου που δημιουργήσατε
sink(file_name, type = "output")

# Tune the cross-validation
trctrl <- trainControl(method = 'repeatedcv', number = 3, repeats = 3)


# Tune kknn parameteres
tuneGrid <- expand.grid(kmax = 1:20,            # allows to test a range of k values
                        distance = 1:5,        # allows to test a range of distance values
                        kernel = c('gaussian',  # different weighting types in kknn
                                   'triangular',
                                   'rectangular',
                                   'epanechnikov',
                                   'optimal'))

# Βρίσκουμε τη βέλτιστη τιμή του k
set.seed(56)
kknn_fit <- train(REFACTORED ~ ., 
                  data = data_btraining, 
                  method = 'kknn',
                  trControl = trctrl,
                  tuneGrid = tuneGrid,
                  tuneLength = 10)



sorted_results <- kknn_fit$results[order(-kknn_fit$results$Accuracy), ]
first_row <- head(sorted_results, 1)
print(first_row)

# Εκτύπωση του τελικού μοντέλου
plot(kknn_fit)

# Πρόβλεψη και αξιολόγηση του τελικού μοντέλου στα δεδομένα ελέγχου

pred <- predict(kknn_fit, newdata = test)

results_df <- data.frame(
  Test = test$REFACTORED,
  Predicted = pred
)

# Εμφάνιση των προβλέψεων για κάθε παρατήρηση
#print(results_df)

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


