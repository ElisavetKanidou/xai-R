# Φόρτωση των απαραίτητων βιβλιοθηκών
library(caret)
library(pROC)
#library(ROSE)
library(kknn)
library(plotly)
library(htmlwidgets)



args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 8) {
  stop("Δεν δόθηκαν αρκετές παράμετροι. Χρησιμοποιήστε --param1=value1 --param2=value2 --param3=value3 --param4=value4 --param5=value5 --param6=value6 --param7=value7 --param8=value8")
}

# Ανάλυση των παραμέτρων
param1 <- NULL
param2 <- NULL
param3 <- NULL
param4 <- NULL
param5 <- NULL
param6 <- NULL
param7 <- NULL
param8 <- NULL

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
  }else if (param_name == "--param5") {
    param5 <- param_value
  } else if (param_name == "--param6") {
    param6 <- param_value
  } else if (param_name == "--param7") {
    param7 <- param_value
  }else if (param_name == "--param8") {
    param8 <- param_value
  }
}

if (is.null(param1) || is.null(param2) || is.null(param3) || is.null(param4) || is.null(param5) || is.null(param6) || is.null(param7) || is.null(param8) ) {
  stop("Λείπουν ορισμένες παράμετροι. Χρησιμοποιήστε --param1=value1 --param2=value2 --param3=value3 --param4=value4 --param5=value5 --param6=value6 --param7=value7 --param8=value8")
}


# Διάβασμα των δεδομένων από το αρχείο CSV 
file_path <- param1

if (file.exists(file_path)) {
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  print("Τα δεδομένα διαβάστηκαν επιτυχώς.")
} else {
  print("Το αρχείο δεν βρέθηκε.")
}
# Διάβασμα των δεδομένων από το αρχείο CSV 
file_pathtrain <- param8

if (file.exists(file_pathtrain)) {
  datatrain <- read.csv(file_pathtrain, header = TRUE, stringsAsFactors = FALSE)
  print("Τα δεδομένα διαβάστηκαν επιτυχώς.")
} else {
  print("Το αρχείο δεν βρέθηκε.")
}
load(param3)
data$REFACTORED[data$REFACTORED == 0] <- 'no'
data$REFACTORED[data$REFACTORED == 1] <- 'yes'

datatrain$REFACTORED[datatrain$REFACTORED == 0] <- 'no'
datatrain$REFACTORED[datatrain$REFACTORED == 1] <- 'yes'

data$REFACTORED <- as.factor(data$REFACTORED)
datatrain$REFACTORED <- as.factor(datatrain$REFACTORED)


#print(knn.best$C)
# Εκτύπωση του τελικού μοντέλου
#plot(kknn_fit)
selected_row_indices <- which(data$file == param2)

selneighbors <- knn.best$C[selected_row_indices, ]

result_df <- data.frame()
# Εμφανίστε τα στοιχεία όλων των γειτόνων
for (i in 1:length(selneighbors)) {
  test_data <- datatrain[selneighbors[i], ]
  result_df <- rbind(result_df, test_data)
  
}
selected_column <- result_df$file

file_name <- paste0(param4, "_neighbours.json")

# Χρησιμοποιήστε τη συνάρτηση sink με το όνομα του αρχείου που δημιουργήσατε
sink(file_name, type = "output")

print(selected_column)

# Ορισμός του plot
res <- plot_ly() %>%
  add_trace(
    x = result_df[result_df$REFACTORED == "yes", param5],
    y = result_df[result_df$REFACTORED == "yes", param6],
    z = result_df[result_df$REFACTORED == "yes", param7],
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 10, color = "blue"),
    text = result_df[result_df$REFACTORED == "yes", "REFACTORED"],
    name = "Yes"  # Τίτλος για το trace των "yes"
  ) %>%
  add_trace(
    x = result_df[result_df$REFACTORED == "no", param5],
    y = result_df[result_df$REFACTORED == "no", param6],
    z = result_df[result_df$REFACTORED == "no", param7],
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 10, color = "black"),
    text = result_df[result_df$REFACTORED == "no", "REFACTORED"],
    name = "No"  # Τίτλος για το trace των "no"
  ) %>%
  add_trace(
    x = data[[param5]][selected_row_indices],
    y = data[[param6]][selected_row_indices],
    z = data[[param7]][selected_row_indices],
    type = "scatter3d",
    mode = "markers+text",
    text = "SelectedClass",
    marker = list(size = 15, color = "red"),
    name = "Selected Class"  # Τίτλος για το trace των επιλεγμένων σημείων
  ) %>%
  layout(scene = list(
    xaxis = list(title = param5),
    yaxis = list(title = param6),
    zaxis = list(title = param7)
  ))
htmlwidgets::saveWidget(res, file = "my_plot.html")
# Αποθήκευση ως HTML

sink()






