
#-----Translate the comments into English
#-----Provide the model's accuracy and sensitivity (recall) based on the model results.

#Input variables:

#y: response variable;
#x: explanatory variable;
#modelresult: the result of the model
#Output variables:

#accuracy: the number of extreme cases under threshold q;
#sensitivity: the proportion of extreme cases in the sample;
#threshold: the threshold q that optimizes the model performance according to the ROC curve;
#y_hat: the estimated values of y by the model

#mian
modelevaluation=function(y,x,modelresult){
  N <- nrow(x)
  y_hat <- rep(0,N)
  xseries <- t(combn(1:6, 3)) # Positions of each variable in the model
  
  rows_to_remove <- which(apply(xseries, 1, function(row) all(c(3, 4) %in% row)))
  
  # Remove these rows
  xseries <- xseries[-rows_to_remove, ]
  
  # Convert coefficients into matrix form
  betalist <- modelresult$beta0list
  # Create empty lists to store extracted beta0 and beta1
  beta0_list <- list()
  beta1_list <- list()
  
  # Loop through each list element
  for (i in 1:length(betalist)) {
    # Extract the first row and second row, and convert into matrix
    beta0 <- matrix(betalist[[i]][1,], nrow = 1)
    beta1 <- matrix(betalist[[i]][2,], nrow = 1)
    
    # Add the extracted matrices to the lists
    beta0_list[[i]] <- beta0
    beta1_list[[i]] <- beta1
  }
  
  # Convert lists into matrices
  beta0 <- do.call(rbind, beta0_list)
  beta1 <- do.call(rbind, beta1_list)
  
  TP <- 0 # True Positive
  TN <- 0 # True Negative
  FN <- 0 # False Negative
  for (i in 1:N) {
    temp <- -Inf
    # Loop to obtain f(x) = max(min,...,min)
    for (j in 1:nrow(xseries)) {
      temp <- max(temp, min(beta0[j,1]+beta1[j,1]*x[i,xseries[j,1]],
                            beta0[j,2]+beta1[j,2]*x[i,xseries[j,2]],
                            beta0[j,3]+beta1[j,3]*x[i,xseries[j,3]]))
    }
    y_hat[i] <- 1/(1+exp(-temp))
  }
library(pROC)
# plot the ROC curve
roc_curve <- roc(y, y_hat)


# plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

coords <- coords(roc_curve, x = "best", best.method = "closest.topleft")

threshold <- coords$threshold

plot(
  main="ROC-AUC curve of model applied with training set", # Title
  roc_curve, col="red", # Color
  legacy.axes=T, # Y-axis
  print.auc=FALSE, # Do not display AUC area
  print.thres=FALSE, # Do not add cutoff points and 95% CI
  lwd=3, # Thickened curve
  lty=1, # Solid line
  xlab="1 - Specificity", ylab="Sensitivity", # X-axis and Y-axis labels
  font.main=1.5, font.axis=1, # Set the boldness of the title and labels
  cex.main=1.5, cex.lab=1.5) # Set the size of the labels

# Add AUC value and set font size
text(0.5, 0.5, paste("AUC =", round(auc(roc_curve), 3)), col="black", cex=2)

# Calculate true positive, true negative, false negative
for (i in 1:N) {  
  if(y[i]==1 & y_hat[i]>=threshold){TP<-TP+1}
  if(y[i]==0 & y_hat[i]<threshold){TN<-TN+1}
  if(y[i]==1 & y_hat[i]<threshold){FN<-FN+1}
}

accuracy <- (TP+TN)/N
sensitivity <- TP/(TP+FN)
cat('threshold = ',threshold, '\n','accuracy = ',accuracy ,'\n', 'sensitivity = ', sensitivity)
return(list(accuracy=accuracy,sensitivity=sensitivity,threshold=threshold,y_hat=y_hat))
}


#----- gives the model's ACCURACY and SENSITIVITY based on the model's results (recall)

# input variables:
# y: response variable;
# x: explanatory variable;
# modelresult: the result of the model

# output variables:
# accuracy: number of extreme populations at threshold q;
# sensitivity: extreme population as a proportion of the sample;
# y_hat: y model estimate

modelevaluation_test=function(y,x,modelresult,threshold){
  N <- nrow(x)
  y_hat <- rep(0,N)
  xseries <- t(combn(1:6, 3)) # Positions of each variable in the model
  
  rows_to_remove <- which(apply(xseries, 1, function(row) all(c(3, 4) %in% row)))
  
  # Remove these rows
  xseries <- xseries[-rows_to_remove, ]
  
  # Convert coefficients into matrix form
  betalist <- modelresult$beta0list
  # Create empty lists to store extracted beta0 and beta1
  beta0_list <- list()
  beta1_list <- list()
  
  # Loop through each list element
  for (i in 1:length(betalist)) {
    # Extract the first row and second row, and convert into matrix
    beta0 <- matrix(betalist[[i]][1,], nrow = 1)
    beta1 <- matrix(betalist[[i]][2,], nrow = 1)
    
    # Add the extracted matrices to the lists
    beta0_list[[i]] <- beta0
    beta1_list[[i]] <- beta1
  }
  
  # Convert lists into matrices
  beta0 <- do.call(rbind, beta0_list)
  beta1 <- do.call(rbind, beta1_list)
  
  TP <- 0 # True Positive
  TN <- 0 # True Negative
  FN <- 0 # False Negative
  for (i in 1:N) {
    temp <- -Inf
    # Loop to obtain f(x) = max(min,...,min)
    for (j in 1:nrow(xseries)) {
      temp <- max(temp, min(beta0[j,1]+beta1[j,1]*x[i,xseries[j,1]],
                            beta0[j,2]+beta1[j,2]*x[i,xseries[j,2]],
                            beta0[j,3]+beta1[j,3]*x[i,xseries[j,3]]))
    }
    y_hat[i] <- 1/(1+exp(-temp))
  }
  
  library(pROC)
  # Calculate ROC curve
  roc_curve <- roc(y, y_hat)
  
  plot(
    main="ROC-AUC curve of model applied with testing set", # Title
    roc_curve, col="red", # Color
    legacy.axes=T, # Y-axis formatting
    print.auc=FALSE, # Do not display AUC area
    print.thres=FALSE, # Do not add cutoff points and 95% CI
    lwd=3, # Thickened curve
    lty=1, # Solid line
    xlab="1 - Specificity", ylab="Sensitivity", # X-axis and Y-axis labels
    font.main=1.5, font.axis=1, # Set the boldness of the title and labels
    cex.main=1.5, cex.lab=1.5) # Set the size of the labels
  
  # Add AUC value and set font size
  text(0.5, 0.5, paste("AUC =", round(auc(roc_curve), 3)), col="black", cex=2)
  
  
  # Calculate true positive, true negative, false negative
  for (i in 1:N) {  
    if(y[i]==1 & y_hat[i]>=threshold){TP<-TP+1}
    if(y[i]==0 & y_hat[i]<threshold){TN<-TN+1}
    if(y[i]==1 & y_hat[i]<threshold){FN<-FN+1}
  }
  
  accuracy <- (TP+TN)/N
  sensitivity <- TP/(TP+FN)
  cat('accuracy = ',accuracy ,'\n', 'sensitivity = ', sensitivity)
  return(list(accuracy=accuracy,sensitivity=sensitivity,y_hat=y_hat))
}

result_train<-modelevaluation(y=Y,x=X_Nor,modelresult=model_Nor)

result_test<-modelevaluation_test(y=Y_test,x=X_Nor_test,modelresult=model_Nor,threshold=result_train$threshold)


