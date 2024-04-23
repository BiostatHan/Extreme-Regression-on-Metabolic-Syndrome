library(ggplot2)
## WC_1----
# Define model parameters
beta0 <- 0.05870281
beta1 <- 0.53374038 # Try changing this value to positive or negative to see different shading effects
# Generate data
y <- seq(0, 1, by = 0.0001)
x <- ((log(y / (1 - y)) - beta0) / beta1) * 8.291353 + 73.86148

# Create data frame
data <- data.frame(x, y)

# Generate plot
p_WC_1 <- ggplot(data, aes(x = y, y = x)) +
  geom_line(color = "blue") +
  labs(x = "predicted", y = "WC_1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(size = 20, face = "bold"),  # Adjust the x-axis label
        axis.title.y = element_text(size = 20, face = "bold"))

# Add shading area based on the value of beta1
if (beta1 > 0) {
  p_WC_1 <- p_WC_1 + geom_ribbon(aes(ymax = x), ymin = Inf, fill = "grey", alpha = 0.5)
} else {
  p_WC_1 <- p_WC_1 + geom_ribbon(aes(ymin = x), ymax = -Inf, fill = "grey", alpha = 0.5)
}

# Limit the display range of the y-axis
p_WC_1 <- p_WC_1 + coord_cartesian(ylim = c(-10, 150), xlim = c(0, 1))

# Calculate the x-coordinate for a specific y value
specific_y <- 0.5885
specific_x <- ((log(specific_y / (1 - specific_y)) - beta0) / beta1) * 8.291353 + 73.86148

# Add specific point
p_WC_1 <- p_WC_1 + geom_point(aes(x = specific_y, y = specific_x), color = "red", size = 3)

# Add text label
p_WC_1 <- p_WC_1 + geom_text(aes(x = specific_y, y = specific_x, label = sprintf("(%.4f, %.4f)", specific_y, specific_x)),
                             vjust = -1, color = "red", fontface = "bold", size = 7)

# Display the plot
print(p_WC_1)

# Similar translation for other sections: WC_0, FPG, systolic_blood_pressure, diastolic_blood_pressure, TG, HDL-C


## WC_0----
# Define model parameters
beta0 <- 0.05870281
beta1 <- 0.53374038 # Try changing this value to positive or negative to see different shading effects
# Generate data
y <- seq(0, 1, by = 0.0001)
x <- ((log(y / (1 - y)) - beta0) / beta1) * 8.1212064 + 84.670497

# Create data frame
data <- data.frame(x, y)

# Generate plot
p_WC_0 <- ggplot(data, aes(x = y, y = x)) +
  geom_line(color = "blue") +
  labs(x = "predicted", y = "WC_0") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(size = 20, face = "bold"),  # Adjust the x-axis label
        axis.title.y = element_text(size = 20, face = "bold"))

# Add shading area based on the value of beta1
if (beta1 > 0) {
  p_WC_0 <- p_WC_0 + geom_ribbon(aes(ymax = x), ymin = Inf, fill = "grey", alpha = 0.5)
} else {
  p_WC_0 <- p_WC_0 + geom_ribbon(aes(ymin = x), ymax = -Inf, fill = "grey", alpha = 0.5)
}

# Limit the display range of the y-axis
p_WC_0 <- p_WC_0 + coord_cartesian(ylim = c(-10, 150), xlim = c(0, 1))

# Calculate the x-coordinate for a specific y value
specific_y <- 0.5885
specific_x <- ((log(specific_y / (1 - specific_y)) - beta0) / beta1) * 8.1212064 + 84.670497

# Add specific point
p_WC_0 <- p_WC_0 + geom_point(aes(x = specific_y, y = specific_x), color = "red", size = 3)

# Add text label
p_WC_0 <- p_WC_0 + geom_text(aes(x = specific_y, y = specific_x, label = sprintf("(%.4f, %.4f)", specific_y, specific_x)),
                             vjust = -1, color = "red", fontface = "bold", size = 7)

# Display the plot
print(p_WC_0)

## FPG----
# Define model parameters
beta0 <- 0.04452709
beta1 <- 0.17469278 # Try changing this value to positive or negative to see different shading effects
# Generate data
y <- seq(0, 1, by = 0.0001)
x <- ((log(y / (1 - y)) - beta0) / beta1) * 0.9028723 + 5.068996

# Create data frame
data <- data.frame(x, y)

# Generate plot
p_FPG <- ggplot(data, aes(x = y, y = x)) +
  geom_line(color = "blue") +
  labs(x = "predicted", y = "FPG") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(size = 20, face = "bold"),  # Adjust the x-axis label
        axis.title.y = element_text(size = 20, face = "bold"))

# Add shading area based on the value of beta1
if (beta1 > 0) {
  p_FPG <- p_FPG + geom_ribbon(aes(ymax = x), ymin = Inf, fill = "grey", alpha = 0.5)
} else {
  p_FPG <- p_FPG + geom_ribbon(aes(ymin = x), ymax = -Inf, fill = "grey", alpha = 0.5)
}

# Limit the display range of the y-axis
p_FPG <- p_FPG + coord_cartesian(ylim = c(-15, 25), xlim = c(0, 1))

# Calculate the x-coordinate for a specific y value
specific_y <- 0.5885
specific_x <- ((log(specific_y / (1 - specific_y)) - beta0) / beta1) * 0.9028723 + 5.068996

# Add specific point
p_FPG <- p_FPG + geom_point(aes(x = specific_y, y = specific_x), color = "red", size = 3)

# Add text label
p_FPG <- p_FPG + geom_text(aes(x = specific_y, y = specific_x, label = sprintf("(%.4f, %.4f)", specific_y, specific_x)),
                           vjust = -1, color = "red", fontface = "bold", size = 7)

# Display the plot
print(p_FPG)


# systolic_blood_pressure
# Define model parameters
beta0 <- 0.2276435
beta1 <- 0.5333921 # Try changing this value to positive or negative to see different shading effects
# Generate data
y <- seq(0, 1, by = 0.0001)
x <- ((log(y / (1 - y)) - beta0) / beta1) * 15.8100775 + 121.182535

# Create a data frame
data <- data.frame(x, y)

# Generate plot
p_systolic_blood_pressure <- ggplot(data, aes(x = y, y = x)) +
  geom_line(color = "blue") +
  labs(x = "predicted", y = "SBP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(size = 20, face = "bold"),  # Adjust the x-axis title
        axis.title.y = element_text(size = 20, face = "bold"))  # Adjust the y-axis title

# Add shaded area based on the value of beta1
if (beta1 > 0) {
  p_systolic_blood_pressure <- p_systolic_blood_pressure + geom_ribbon(aes(ymax = x), ymin = Inf, fill = "grey", alpha = 0.5)
} else {
  p_systolic_blood_pressure <- p_systolic_blood_pressure + geom_ribbon(aes(ymin = x), ymax = -Inf, fill = "grey", alpha = 0.5)
}

# Limit the display range of the y-axis
p_systolic_blood_pressure <- p_systolic_blood_pressure + coord_cartesian(ylim = c(-10, 240), xlim = c(0, 1))

# Calculate the x-coordinate for a specific y-value
specific_y <- 0.5885
specific_x <- ((log(specific_y / (1 - specific_y)) - beta0) / beta1) * 15.8100775 + 121.182535

# Add a specific point
p_systolic_blood_pressure <- p_systolic_blood_pressure + geom_point(aes(x = specific_y, y = specific_x), color = "red", size = 3)

# Add a text label
p_systolic_blood_pressure <- p_systolic_blood_pressure + geom_text(aes(x = specific_y, y = specific_x, label = sprintf("(%.4f, %.4f)", specific_y, specific_x)),
                                                                   vjust = -1, color = "red", fontface = "bold", size = 7)

# Display the plot
print(p_systolic_blood_pressure)

## diastolic_blood_pressure----
# Define model parameters
beta0 <- 0.1759036
beta1 <- 0.2303554 # Try changing this value to positive or negative to see different shading effects
# Generate data
y <- seq(0, 1, by = 0.0001)
x <- ((log(y / (1 - y)) - beta0) / beta1) * 11.225369 + 75.619527

# Create a data frame
data <- data.frame(x, y)

# Generate plot
p_diastolic_blood_pressure <- ggplot(data, aes(x = y, y = x)) +
  geom_line(color = "blue") +
  labs(x = "predicted", y = "DBP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(size = 20, face = "bold"),  # Adjust the x-axis title
        axis.title.y = element_text(size = 20, face = "bold"))  # Adjust the y-axis title

# Add shaded area based on the value of beta1
if (beta1 > 0) {
  p_diastolic_blood_pressure <- p_diastolic_blood_pressure + geom_ribbon(aes(ymax = x), ymin = Inf, fill = "grey", alpha = 0.5)
} else {
  p_diastolic_blood_pressure <- p_diastolic_blood_pressure + geom_ribbon(aes(ymin = x), ymax = -Inf, fill = "grey", alpha = 0.5)
}

# Limit the display range of the y-axis
p_diastolic_blood_pressure <- p_diastolic_blood_pressure + coord_cartesian(ylim = c(-100, 250), xlim = c(0, 1))

# Calculate the x-coordinate for a specific y-value
specific_y <- 0.5885
specific_x <- ((log(specific_y / (1 - specific_y)) - beta0) / beta1) * 11.225369 + 75.619527

# Add a specific point
p_diastolic_blood_pressure <- p_diastolic_blood_pressure + geom_point(aes(x = specific_y, y = specific_x), color = "red", size = 3)

# Add a text label
p_diastolic_blood_pressure <- p_diastolic_blood_pressure + geom_text(aes(x = specific_y, y = specific_x, label = sprintf("(%.4f, %.4f)", specific_y, specific_x)),
                                                                     vjust = -1, color = "red", fontface = "bold", size = 7)

# Display the plot
print(p_diastolic_blood_pressure)

## TG----
# Define model parameters
beta0 <- 0.3118878
beta1 <- 0.6170168 # Try changing this value to positive or negative to see different shading effects
# Generate data
y <- seq(0, 1, by = 0.0001)
x <- ((log(y / (1 - y)) - beta0) / beta1) * 1.3612834 + 1.560076

# Create a data frame
data <- data.frame(x, y)

# Generate plot
p_TG <- ggplot(data, aes(x = y, y = x)) +
  geom_line(color = "blue") +
  labs(x = "predicted", y = "TG") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(size = 20, face = "bold"),  # Adjust the x-axis title
        axis.title.y = element_text(size = 20, face = "bold"))  # Adjust the y-axis title

# Add shaded area based on the value of beta1
if (beta1 > 0) {
  p_TG <- p_TG + geom_ribbon(aes(ymax = x), ymin = Inf, fill = "grey", alpha = 0.5)
} else {
  p_TG <- p_TG + geom_ribbon(aes(ymin = x), ymax = -Inf, fill = "grey", alpha = 0.5)
}

# Limit the display range of the y-axis
p_TG <- p_TG + coord_cartesian(ylim = c(-10, 10), xlim = c(0, 1))

# Calculate the x-coordinate for a specific y-value
specific_y <- 0.5885
specific_x <- ((log(specific_y / (1 - specific_y)) - beta0) / beta1) * 1.3612834 + 1.560076
# Add a specific point
p_TG <- p_TG + geom_point(aes(x = specific_y, y = specific_x), color = "red", size = 3)

# Add a text label
p_TG <- p_TG + geom_text(aes(x = specific_y, y = specific_x, label = sprintf("(%.4f, %.4f)", specific_y, specific_x)),
                         vjust = -1, color = "red", fontface = "bold", size = 7)

# Display the plot
print(p_TG)



## HDL-C----
# Define model parameters
beta0 <- -0.1007159
beta1 <- -0.6607324 # Try changing this value to positive or negative to see different shading effects
# Generate data
y <- seq(0, 1, by = 0.01)
x <- ((log(y / (1 - y)) - beta0) / beta1) * 0.3606654 + 1.358829

# Create a data frame
data <- data.frame(x, y)

# Generate plot
p_HDL_C <- ggplot(data, aes(x = y, y = x)) +
  geom_line(color = "blue") +
  labs(x = "predicted", y = "HDL-C") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(size = 20, face = "bold"),  # Adjust the x-axis title
        axis.title.y = element_text(size = 20, face = "bold"))  # Adjust the y-axis title

# Add shaded area based on the value of beta1
if (beta1 > 0) {
  p_HDL_C <- p_HDL_C + geom_ribbon(aes(ymax = x), ymin = Inf, fill = "grey", alpha = 0.5)
} else {
  p_HDL_C <- p_HDL_C + geom_ribbon(aes(ymin = x), ymax = -Inf, fill = "grey", alpha = 0.5)
}

# Limit the display range of the y-axis
p_HDL_C <- p_HDL_C + coord_cartesian(ylim = c(-2.5, 5), xlim = c(0, 1))

# Calculate the x-coordinate for a specific y-value
specific_y <- 0.5885
specific_x <- ((log(specific_y / (1 - specific_y)) - beta0) / beta1) * 0.3606654 + 1.358829

# Add a specific point
p_HDL_C <- p_HDL_C + geom_point(aes(x = specific_y, y = specific_x), color = "red", size = 3)

# Add a text label
p_HDL_C <- p_HDL_C + geom_text(aes(x = specific_y, y = specific_x, label = sprintf("(%.4f, %.4f)", specific_y, specific_x)),
                               vjust = -1, color = "red", fontface = "bold", size = 7)

# Display the plot
print(p_HDL_C)


## Violin plots----
# Create violin plots using ggplot2, grouped by Diagnosis and Gender, and distinguish between male and female
data <- data %>%
  rename(Diagnosis = response)
# Add labels 0 and 1 for Gender
data$gender <- factor(data$gender, labels = c("Female", "Male"))

# Plot violin for waistline by Gender and Diagnosis
P1 <- ggplot(data, aes(x = Diagnosis, y = waistline, fill = Diagnosis)) +
  geom_violin(show.legend = T) + # Adjust transparency
  labs(x = "Diagnosis", y = "WC") + # WC likely stands for Waist Circumference
  ggtitle("WC by Diagnosis and Gender") +
  facet_wrap(~gender) + # Create separate facets for each gender
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

# Similar plots for other health metrics
P2 <- ggplot(data, aes(x = Diagnosis, y = FPG, fill = Diagnosis)) + # FPG stands for Fasting Plasma Glucose
  geom_violin(show.legend = F) + # Remove the legend as the fill is not by Diagnosis
  labs(x = "Diagnosis", y = "FPG") +
  ggtitle("FPG by Diagnosis") +
  theme(plot.title = element_text(hjust = 0.5))

P3 <- ggplot(data, aes(x = Diagnosis, y = systolic_blood_pressure, fill = Diagnosis)) +
  geom_violin(show.legend = F) +
  labs(x = "Diagnosis", y = "SBP") + # SBP stands for Systolic Blood Pressure
  ggtitle("SBP by Diagnosis") +
  theme(plot.title = element_text(hjust = 0.5))

P4 <- ggplot(data, aes(x = Diagnosis, y = diastolic_blood_pressure, fill = Diagnosis)) +
  geom_violin(show.legend = F) +
  labs(x = "Diagnosis", y = "DBP") + # DBP stands for Diastolic Blood Pressure
  ggtitle("DBP by Diagnosis") +
  theme(plot.title = element_text(hjust = 0.5))

P5 <- ggplot(data, aes(x = Diagnosis, y = TG, fill = Diagnosis)) + # TG stands for Triglycerides
  geom_violin(show.legend = F) +
  labs(x = "Diagnosis", y = "TG") +
  ggtitle("TG by Diagnosis") +
  theme(plot.title = element_text(hjust = 0.5))

P6 <- ggplot(data, aes(x = Diagnosis, y = HDL, fill = Diagnosis)) + # HDL stands for High-Density Lipoprotein
  geom_violin(show.legend = F) +
  labs(x = "Diagnosis", y = "HDL-C") + # HDL-C stands for HDL Cholesterol
  ggtitle("HDL-C by Diagnosis") +
  theme(plot.title = element_text(hjust = 0.5))