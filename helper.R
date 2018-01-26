####################################################################################
                        ## Helper Functions ##
####################################################################################

#Fuction to load packages

pack = c("caret","plyr","foreach","e1071","sparklyr","plotROC","pROC",
         "kernlab","doParallel","compiler", "dplyr","ggplot2",
         "stringr", "tidyr", "tibble", "corrplot", "klaR")

lapply(pack, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(pack, library, character.only = TRUE)

# Evaluation metric
rmse = function(actual, pred) {
  error = sqrt(mean((actual - pred)^2))
  return(error)
}

user <- function(actual, pred) {
  rsse = sqrt(sum((actual - pred)^2))
  return(rsse)
}

# Interactive evaluation function 
evaluate = function(model,modelname,data, actual) {
  
  print("This is an interactive function. Please type in your responses in the console :")  
  
  #Loading packages
  if (!require("e1071")) install.packages("e1071"); library("e1071")
    
    classifier = readline(prompt = "Type in the name of the classifier : (eg. lr, xgb)  ")
    
    if (classifier == "lr") {
      probabilities = predict(model, newdata = data,type="response")
    } else {
      probabilities = predict(model, newdata = data)
    }
    
    stats = summary(probabilities)
    print("Descriptive statistics for the prediction probabilities : ")
    print(stats)
    
    pos <- readline(prompt = "Enter the positive level of target variable : ")
    neg <- readline(prompt = "Enter the negative level of target variable : ")
    
    threshold = readline(prompt = "Do you want to use a specific threshold for classification?
                         Type in the numerical threshold value if not type 'no'")
    
    if (threshold == "no") {
      print("You have chosen to use a default threshold")
      print("Classification using 0.5,mean of the probabilities as thresholds")
      
      pred_mean = ifelse(probabilities >= stats[3], pos, neg)
      pred_median = ifelse(probabilities>=stats[4],pos, neg)
      pred_3quad = ifelse(probabilities>=stats[5],pos, neg)
      
      # Display results
      print("Actual")
      print(table(actual))
      
      print("Threshold - Mean")
      print(table(pred_mean))
      
      print("Confusion Matrix - Mean")
      cm.mean <- confusionMatrix(test$order,pred_mean)
      print(cm.mean)
      cmplot.mean <- fourfoldplot(cm.mean$table, color = c("#0000FF","#FF0000"),
                                  conf.level = 0, margin = 1, main = "Threshold value -mean")
      
      print("Threshold - median")
      print(table(pred_median))
      
      print("Confusion Matrix - median")
      cm.median <- confusionMatrix(test$order,pred_median)
      print(cm.median)
      cmplot.median <- fourfoldplot(cm.median$table, color = c("#0000FF","#FF0000"),
                                    conf.level = 0, margin = 1, main = "Threshold value -median")
      
      print("Threshold - 3quad")
      print(table(pred_3quad))
      
      print("Confusion Matrix - 3quad")
      cm.3quad <- confusionMatrix(test$order,pred_3quad)
      print(cm.3quad)
      cmplot.3quad <- fourfoldplot(cm.3quad$table, color = c("#0000FF","#FF0000"),
                                   conf.level = 0, margin = 1, main = "Threshold value -3quad")
      
    } else {
      
      pred_user = as.factor(ifelse(probabilities >= as.numeric(threshold), pos, neg))
      
      # Display results
      cat(paste0("\n", "Classification with user defined threshold","\t" ,as.numeric(threshold)))
      print(table(pred_user))
      print("Threshold - user")
      print(table(pred_user))
      
      print("Confusion Matrix - user")
      cm.user <- confusionMatrix(test$order,pred_user)
      print(cm.user)
      cmplot.user <- fourfoldplot(cm.user$table, color = c("#0000FF","#FF0000"),
                                  conf.level = 0, margin = 1, main = "Threshold value -user")
      
    }}
    

errordecomposition <- function(actual,pred){
      cat("\n", "The descriptive statistics of the predicted values", "\n")
      print(summary(pred))
      cat("\n", "The descriptive statistics of the actual values", "\n")
      print(summary(actual))
      
      # Metric
      cat("\n", "Which metric would you like to use for error analysis?","\n",
          "If you want to use a specific metric please add it to the helper function and name it as user.")
      metric = readline(prompt =  "Choose metric - rmse,user : ")
      #Error handling
      if(metric!="rmse"&metric!="user"){
        stop("Choose an appropriate method. Check for typos!")}
      
      if (metric == "rmse") {
        
        error = rmse(actual, pred)
        cat(paste0("\n", "The prediction error is : ", error))
      } else {
        error = user(actual, pred)
      }
      
      
      # Error decomposition
      mse = mean((actual - pred)^2)
      bias = (mean(pred) - mean(actual))
      var = mse - (bias^2) #(Decomposition of mse)
      percent.var = var/mse
      actual.skew = skewness(actual)  # to check the distributions
      predicted.skew = skewness(pred)
      
      # Saving results
      
      error.matrix = cbind(error, mse, bias,var,percent.var,actual.skew, predicted.skew)
      cat("\n", "Decomposing the error : ", "\n")
      print(error.matrix)
      
      # Visualization - Plotting the normal curve
      
      x1 = actual
      y1 = dnorm(actual, mean = mean(actual), sd = sd(actual))
      
      x2 = pred
      y2 = dnorm(pred, mean = mean(pred), sd = sd(pred))
      
      cat("\n", "Plotting the distributions of the actual and predicted values - green actual,red predicted")
      
      dist.plot = plot(x1, y1, col = "green", xlim = range(c(x1, x2)), ylim = range(c(y1, 
                                                                                      y2)), xlab = "Sales", ylab = "Density")
      points(x2, y2, col = "red")
      title(main = as.character(modelname))
      legend("topright", legend = c("Actual", "Predicted"), fill = c("green", "red"))
      
      dist.plot
  
      return(error.matrix)}
  
  






