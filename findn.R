#Based on the model parameters determined, calculate the proportion of the total sample that satisfies the condition based on the given threshold q

# input variables:
# y: response variable;
# x: explanatory variable;
# modelresult: the result of the model
# q: the set threshold

# outputvariables:
# n_ex: number of extreme populations at threshold q;
# rate_ex: proportion of extreme populations in the sample;
# sample_ex: inclusion of extreme populations
# c_q: explanatory variable thresholds

findn = function(y, x, modelresult, q){
  xseries <- t(combn(1:6, 3)) # For each variable in the model, determine its position
  rows_to_remove <- which(apply(xseries, 1, function(row) all(c(3, 4) %in% row)))
  # Remove these rows
  xseries <- xseries[-rows_to_remove, ]
  n_comb <- nrow(xseries)
  c_q <- matrix(0, nrow=n_comb, ncol=3) # Initialize the threshold for all explanatory variables
  N <- length(y) # Total sample size
  sample_ex <- matrix(0, nrow=N) # Initialize a vector to record extreme samples
  q_x <- log(q/(1-q))
  
  # Convert coefficients to matrix form
  betalist <- modelresult$beta0list
  # Create an empty list to store extracted beta0 and beta1
  beta0_list <- list()
  beta1_list <- list()
  
  # Loop through each element of the list
  for (i in 1:length(betalist)) {
    # Extract the first and second rows and convert to a matrix
    beta0 <- matrix(betalist[[i]][1,], nrow = 1)
    beta1 <- matrix(betalist[[i]][2,], nrow = 1)
    
    # Add the extracted matrices to the list
    beta0_list[[i]] <- beta0
    beta1_list[[i]] <- beta1
  }
  
  # Convert the list to a matrix
  beta0 <- do.call(rbind, beta0_list)
  beta1 <- do.call(rbind, beta1_list)
  
  # Loop to get all c_q for the corresponding threshold q
  for(i in 1:n_comb){
    for (j in 1:3) {
      c_q[i,j] <- (q_x-beta0[i,j])/beta1[i,j]
    }
  }
  
  # Determine the cases in the sample that fit the extreme population
  for (i in 1:N) {
    condition <- 0
    for (j in 1:n_comb) {
      if(sign(beta1[j,1])*x[i,xseries[j,1]] > sign(beta1[j,1])*c_q[j,1] & 
         sign(beta1[j,2])*x[i,xseries[j,2]] > sign(beta1[j,2])*c_q[j,2] &
         sign(beta1[j,3])*x[i,xseries[j,3]] > sign(beta1[j,3])*c_q[j,3]){
        condition <- condition+1
      }
    }
    # If any 'min' condition is met, consider the sample as fitting the extreme population
    if(condition > 0){
      sample_ex[i] <-1 # Record the sample as an extreme population
    }
  }
  n_ex <- sum(sample_ex)
  rate_ex <- n_ex / N
  rate_1 <- n_ex / sum(Y)
  cat('The number of extreme population samples at the threshold', q, 'is', n_ex, '\n',
      'Proportion of the total sample is', rate_ex, '\n',
      'Proportion of the total patient population is', rate_1)
  return(list(n_ex=n_ex, rate_ex=rate_ex, rate_1=rate_1, sample_ex=sample_ex, c_q=c_q))
}

findnresult <- findn(y = Y,x = X_Nor,modelresult = model_Nor,q = 0.6657)

