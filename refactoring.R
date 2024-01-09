# Φόρτωση των απαραίτητων βιβλιοθηκών
library(caret)
library(pROC)
library(ROSE)
library(kknn)
library(lime)


args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4) {
  stop("Δεν δόθηκαν αρκετές παράμετροι. Χρησιμοποιήστε --param1=value1 --param2=value2 --param3=value3 --param4=value4")
}

# Ανάλυση των παραμέτρων
param1 <- NULL
param2 <- NULL
param3 <- NULL
param4 <- NULL

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
  } else if (param_name == "--param4") {
    param4 <- param_value
  }
}

if (is.null(param1) || is.null(param2) || is.null(param3) || is.null(param4)) {
  stop("Λείπουν ορισμένες παράμετροι. Χρησιμοποιήστε --param1=value1 --param2=value2 --param3=value3 --param4=value4")
}


# Διάβασμα των δεδομένων από το αρχείο CSV 
file_path <- param1

if (file.exists(file_path)) {
  datat <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  print("Τα δεδομένα διαβάστηκαν επιτυχώς.")
} else {
  print("Το αρχείο δεν βρέθηκε.")
}

load(param2)

test=datat[, -c(1:4)]


explainer <- readRDS(file = param3)

file_name <- paste0(param4, "_refactoring.json")

sink(file_name, type = "output")

pred <- predict(kknn_fit, newdata = test)
datat <- datat[, -ncol(datat)]
results_df <- data.frame(
  datat,
  Predicted = pred
)
newdata_yes <- subset(results_df, Predicted == "yes")
# Εμφάνιση των προβλέψεων για κάθε παρατήρηση
print("File")
print(results_df$file)
print("Predicted")
print(results_df$Predicted)

newdata_yes<-newdata_yes[, -c(1:4)]

explanation <- explain(newdata_yes, explainer, n_labels = 1, n_features = 1)

print("Probability")
print(explanation$label_prob)

sink()

