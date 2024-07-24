# modelAPI.R

# Load necessary libraries
library(readr)
library(plumber)
library(rpart)

# Read in the processed diabetes dataset
diabetes <- readRDS("processed_diabetes.rds")

# Load the best model
load("cl_tree_fit.RData")

# Train the classification tree model using the entire dataset
final_model <- rpart(Diabetes_binary~ HighBP+ HighChol+ HeartDiseaseorAttack+ PhysActivity+GenHlth + MentHlth, data = diabetes, control = rpart.control(cp = 0))

# Define the selected predictors
selected_predictors <- c("HighBP", "HighChol", "HeartDiseaseorAttack", "PhysActivity", "GenHlth", "MentHlth")

# Make predictions on the entire dataset using the cl_tree_fit model
final_predictions <- predict(cl_tree_fit, newdata = select(diabetes, all_of(selected_predictors)), type = "prob")

# Calculate default values
default_values <- diabetes %>% 
  summarise(
    HighBP = names(which.max(table(HighBP))),
    HighChol = names(which.max(table(HighChol))),
    HeartDiseaseorAttack = names(which.max(table(HeartDiseaseorAttack))),
    PhysActivity = names(which.max(table(HeartDiseaseorAttack))),
    GenHlth = names(which.max(table(GenHlth))),
    MentHlth = mean(MentHlth, na.rm = TRUE)
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
                         MentHlth = MentHlth)
  
  # Predict using the best model (cl_fit)
  prediction <- predict(cl_fit, pred_data, type = "prob")
  
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

# Example function calls to check that it works:
# curl -X POST "http://localhost:8000/pred?HighBP=1&HighChol=0&HeartDiseaseorAttack=0&PhysActivity=1&GenHlth=3"
# curl -X POST "http://localhost:8000/pred"
# curl -X GET "http://localhost:8000/info"