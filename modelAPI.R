# modelAPI.R

# Load necessary libraries
library(readr)
library(plumber)
library(randomForest)

# read in the diabetes data set
diabetes <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# fit my best model

# Set up cross-validation control with 3-fold CV
control <- trainControl(method = "cv", 
                        number = 3, 
                        classProbs = TRUE, 
                        summaryFunction = mnLogLoss)

# Set seed for reproducibility
set.seed(123)

# Fit Random Forest model
rf_fit <- train(Diabetes_binary ~ HighBP + HighChol + HeartDiseaseorAttack + PhysActivity + GenHlth,
                data = diabetes,
                method = "rf",
                trControl = control,
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(mtry = 1:5),
                ntree = 100)

# Calculate mean values for default inputs
default_values <- colMeans(data[, c("PhysActivity", "GenHlth")])

#* @apiTitle Random Forest Prediction API

#* Predict endpoint
#* @param HighBP The value of HighBP (default: 0)
#* @param HighChol The value of HighChol (default: 0)
#* @param HeartDiseaseorAttack The value of HeartDiseaseorAttack (default: 0)
#* @param PhysActivity The value of PhysActivity (default: mean)
#* @param GenHlth The value of GenHlth (default: mean)
#* @post /pred
function(HighBP = 0,
         HighChol = 0,
         HeartDiseaseorAttack = 0,
         PhysActivity = default_values["PhysActivity"],
         GenHlth = default_values["GenHlth"]) {
  
  # Create a new data frame for prediction
  new_data <- data.frame(HighBP = as.factor(as.numeric(HighBP)),
                         HighChol = as.factor(as.numeric(HighChol)),
                         HeartDiseaseorAttack = as.factor(as.numeric(HeartDiseaseorAttack)),
                         PhysActivity = as.numeric(PhysActivity),
                         GenHlth = as.numeric(GenHlth))
  
  # Predict using the best model
  prediction <- predict(rf_fit, new_data, type = "prob")
  
  return(list(prediction = prediction))
}

#* Info endpoint
#* @get /info
function() {
  list(
    name = "Jie Chen",
    url = "https://your-github-pages-site.github.io"
  )
}

# Example function calls to check that it works:
# curl -X POST "http://localhost:8000/pred?HighBP=1&HighChol=0&HeartDiseaseorAttack=0&PhysActivity=1&GenHlth=3"
# curl -X POST "http://localhost:8000/pred"
# curl -X GET "http://localhost:8000/info"