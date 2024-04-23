## Load data----
rm(list =ls())
setwd_R = function(){setwd(dirname(rstudioapi::getSourceEditorContext()$path))}
setwd_R()
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}
# load  packages:
pacman::p_load(readxl,dplyr,caret,caTools,e1071,rpart,dplyr,class,table1)
# please put "XRfunctions2.R" in the same directory as this file
source("./XRfunctions2.R")

data <- read_excel("data.xlsx")
# transfrom the variables into numeric
data[] <- lapply(data, as.numeric)

## Determine if suffering from metabolic syndrome----
has_metabolic_syndrome <- function(row) {
  # Extract relevant variable values
  gender <- row[["gender"]]  # Gender
  WC <- row[["WC"]]          # Waist circumference
  FPG <- row[["FPG"]]        # Fasting plasma glucose
  systolic_bp <- row[["SBP"]]  # Systolic blood pressure
  diastolic_bp <- row[["DBP"]] # Diastolic blood pressure
  tg <- row[["TG"]]            # Triglycerides
  HDL_C <- row[["HDL-C"]]      # High-density lipoprotein cholesterol
  # Conditions evaluation
  conditions <- 0
  # Abdominal obesity
  if ((gender == 0 && WC >= 90) || (gender == 1 && WC >= 85)) {  # If male and waist circumference is greater than or equal to 90, or female and waist circumference is greater than or equal to 85
    conditions <- conditions + 1
  }
  # High blood sugar or diabetes
  if (FPG >= 6.1 ) {  # Fasting plasma glucose is greater than or equal to 6.1
    conditions <- conditions + 1
  }
  # Hypertension
  if (systolic_bp >= 130 || diastolic_bp >= 85 ) {  # Systolic blood pressure is greater than or equal to 130 or diastolic blood pressure is greater than or equal to 85
    conditions <- conditions + 1
  }
  # High triglycerides
  if (tg >= 1.7) {  # Triglycerides is greater than or equal to 1.7
    conditions <- conditions + 1
  }
  # Low HDL-C cholesterol
  if (HDL_C < 1.04) {  # High-density lipoprotein cholesterol is less than 1.04
    conditions <- conditions + 1
  }
  # Determine if suffering from metabolic syndrome
  if (conditions >= 3) {  # If the number of conditions met is greater than or equal to 3
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Apply the function and add a new column
data$response <- apply(data, 1, has_metabolic_syndrome)  # Apply the function to the data and add a new column



## Set up XY----
# Standardize
Y <- as.integer(data$response)
X_Nor <- data %>%
  select("gender", "WC", "FPG", "SBP", "DBP", "TG", "HDL-C") %>%
  mutate(WC = ifelse(gender == 0, scale(WC), WC),  # Standardize samples where gender equals 0
         WC = ifelse(gender == 1, scale(WC), WC)) %>%  # Standardize samples where gender equals 1
  select(-gender) %>%  # Remove the original gender variable
  mutate_at(vars(FPG, SBP, DBP, TG, `HDL-C`), scale)  # Standardize selected columns



## Randomly divide the dataset into training and testing sets
data_divide <- data.frame(Y, X_Nor)

# Set a random seed for reproducibility
set.seed(1)

# Shuffle the indices of the dataset randomly
n <- nrow(data_divide)
train_size <- round(n * 0.7)
train_indices <- sample(1:n, train_size, replace = FALSE)
test_indices <- setdiff(1:n, train_indices)

# Divide the dataset
training <- data_divide[train_indices, ] # Training set
testing <- data_divide[test_indices, ] # Testing set

Y <- training$Y
X_Nor <- training[, !names(training) %in% "Y"]

Y_test <- testing$Y
X_Nor_test <- testing[, !names(training) %in% "Y"]


model_Nor<- XReg(Y,X_Nor,varlist = list(c(1,2,3),c(1,2,4),c(1,2,5),c(1,2,6),
                                        c(1,3,5),c(1,3,6),
                                        c(1,4,5),c(1,4,6),
                                        c(1,5,6),
                                        c(2,3,5),c(2,3,6),
                                        c(2,4,5),c(2,4,6),
                                        c(2,5,6),
                                        c(3,5,6),
                                        c(4,5,6)))


##Output----
model_Nor$beta0list

