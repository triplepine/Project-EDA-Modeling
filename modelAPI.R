# modelAPI.R

# Load necessary libraries
library(readr)
library(plumber)
library(rpart)
library(dplyr)

# Read in the processed diabetes dataset
diabetes <- readRDS("processed_diabetes.rds")

# Load the best model
logRegFit_1 <-readRDS("logRegFit_1.rds")

# Train the classification tree model using the entire dataset
final_model <- rpart(Diabetes_binary~ HighBP+ HighChol+ HeartDiseaseorAttack+ PhysActivity+GenHlth + MentHlth, data = diabetes, control = rpart.control(cp = 0))

# Define the selected 6 predictors
selected_predictors <- c("HighBP", "HighChol", "HeartDiseaseorAttack", "PhysActivity", "GenHlth", "MentHlth")

# Make predictions on the entire dataset using the logRegFit_1 model
final_predictions <- predict(logRegFit_1, newdata = select(diabetes, all_of(selected_predictors)), type = "prob")

# Calculate default values
default_values <- diabetes %>% 
  summarise(
    HighBP = names(which.max(table(HighBP))),
    HighChol = names(which.max(table(HighChol))),
    HeartDiseaseorAttack = names(which.max(table(HeartDiseaseorAttack))),
    PhysActivity = names(which.max(table(HeartDiseaseorAttack))),
    GenHlth = names(which.max(table(GenHlth))),
    MentHlth = mean(as.numeric(MentHlth), na.rm = TRUE) 
  )

#* @apiTitle Diabetes Prediction API

#* Make a prediction
#* @param HighBP Default: most prevalent class
#* @param HighChol Default: most prevalent class
#* @param HeartDiseaseorAttack Default: most prevalent class
#* @param PhysActivity Default: most prevalent class
#* @param GenHlth Default: most prevalent class
#* @post /pred
function(HighBP = default_values$HighBP,
         HighChol = default_values$HighChol,
         HeartDiseaseorAttack = default_values$HeartDiseaseorAttack,
         PhysActivity = default_values$PhysActivity,
         GenHlth = default_values$GenHlth,
         MentHlth = default_values$MentHlth) {
  
  # Create a new data frame for prediction
  pred_data <- data.frame(HighBP = HighBP,
                         HighChol = HighChol,
                         HeartDiseaseorAttack = HeartDiseaseorAttack,
                         PhysActivity = PhysActivity,
                         GenHlth = GenHlth,
                         MentHlth = as.numeric(MentHlth))
  
  # Predict using the best model (logRegFit_1)
  prediction <- predict(logRegFit_1, pred_data, type = "prob")
  
  return(list(prediction = prediction))
}


#* Info endpoint
#* @get /info
function() {
  list(
    name = "Jie Chen",
    url = "https://triplepine.github.io/Project-EDA-Modeling/"
  )
}

# Example function calls:
# curl -X POST "http://localhost:8000/pred?HighBP=Yes&HighChol=Yes&HeartDiseaseorAttack=No&PhysActivity=Yes&GenHlth=Good&MentHlth=15"

# curl -X POST "http://localhost:8000/pred?HighBP=No&HighChol=No&HeartDiseaseorAttack=Yes&PhysActivity=No&GenHlth=Poor&MentHlth=30"

# curl -X POST "http://localhost:8000/pred?HighBP=Yes&HighChol=Yes&HeartDiseaseorAttack=Yes&PhysActivity=Yes&GenHlth=Fair&MentHlth=3"