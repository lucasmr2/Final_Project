
########################################################################
#read in data
#note to user the file that is read in sould have the Y variable in the first column
#please enter in csv file
#instructions should say only 1 response variable per data file

##mydata <- read.csv("499data.csv", header=TRUE)
#mydata[,1] <- NULL
#head(mydata)
#data <- data.frame(mydata)


#######################################################################
#function 1 
#determine the number of explanitory variables 
#check normality of data by plotting histograms
#return a pdf of the histograms with the normality laid over

normality <- function(df){
  
  num_exp_var <- ncol(df) - 1
  num_exp_var
  
  num <- message("Your data has ", num_exp_var, " explanitory varriables.")
  
  #plotting histograms with normal distribution overlay
  #require("ggplot2")
  
  #making a pdf of the plots
  pdf(file ='normality.pdf')
  
  for (i in 1:ncol(df)){
    
    gg <- ggplot(df, aes(x = df[,i])) 
    gg <- gg +  geom_histogram(binwidth = (max(df[,i]) -min(df[,i]))/20,
                               colour = "black",
                               aes_string(y = "..density..", fill = "..count..")) +
      xlab(colnames(df[i])) + ylab("Density") +
      scale_fill_gradient("Count", low = "blue4", high = "deeppink") +
      stat_function(fun = dnorm, color = "green3", lwd=2,
                    args = list(mean = mean(df[,i]), 
                                sd = sd(df[,i])))
    
    print(gg)
  }
  
  #plotting QQ plots
  #install.packages("ggpubr")
  #require(ggpubr)
  
  for (i in 1:ncol(df)){
    
    print(ggqqplot(df[,i], xlab = (colnames(df[i]))))
    
  }
  
  dev.off()
  
  message("In the QQ plots, If your data points deviate from the line that is a sign of normality being broken.")
  
  #statictical test for normality
  for (i in 1:ncol(df)){
    
    print(shapiro.test(df[,i]))
    
  }
  message("If a p-value is < 0.05, it means that the data is sig diff from normal.")
  message("If your assumption of normality is broken, consider transforming your data.")
  
}

###########################################################################  
# function 2 
# check for issues with the data such as correlation or outliers

data_check <- function(df){
  
  cor.mat <- cor(df)
  message("Correlation Matrix.")
  print(cor.mat)
  
  #remove correlations with response variable
  #we want high correlations of Y by the x's
  no.Y.mat <- cor.mat[2:nrow(cor.mat), 2:ncol(cor.mat)]
  
  #remove duplicate correlations 
  cor.vect <- data.frame(arrayInd(1:prod(dim(no.Y.mat)), dim(no.Y.mat)), value = c(no.Y.mat))
  cor.sort <- t(apply(cor.vect, 1, sort))
  uniq.corr<- cor.vect[!duplicated(cor.sort), ]
  
  #I do not car about correlations of X's with itself
  final.cor <- uniq.corr[uniq.corr$value < 1,]
  message("Unique correlations among predictor variables.")
  print(final.cor)
  
  #inform the user of any large correlations
  for (i in 1:nrow(final.cor)){
    
    if(final.cor[i,3] > 0.5){
      print("The following predictor variables had greater than 0.5 correlation")
      print("Their corelation is shown")
      answer <- unname(final.cor[i, ])
      print(answer[ ,2:3])
      print("Multicolinearity might be an issue")
      
    }
  }
  
  #outlier stats
  
  pdf(file = 'data_check.pdf')
  
  for (i in 1:ncol(df)){
    
    print(boxplot(df[ ,i], xlab = (colnames(df[i]))))
    
  }
  
  dev.off()
  
  message("Outliers are represented as points outside of the wiskers.")
  message("If outliers are present, consider calculating leverage values.")
  message("only remove outlier points if leverage value is an order of magnitude larger.")
  
  mylist <- list(cor.mat, answer[,2:3])
  return(mylist)
  
}

###########################################################################
#function 3 

#note to user, this does not look at higher order or interaction terms
# make a model and return to the user
#my model will be anthesis date = leaf number + height +  leaf area 
# test how good your model is
#via r2 adj r2, Aic and BIC 

model_quality <- function(df){
  
  num_exp_var <- ncol(df) - 1
  num_exp_var
  
  #see how good any 1 predictor is to predicting your response variable
  pdf(file='model_quality.pdf')
  
  for (i in 2:ncol(df)){
    
    scatter.smooth(x = df[ ,i], y = df[ ,1], 
                   main = paste(colnames(df)[1], "by", colnames(df)[i]),
                   xlab = (colnames(df[i])),
                   ylab = (colnames(df)[1]))
  }
  
  #see how good using all predictors is 
  #for more than 1 explanitory variables
  
  model = lm(df[,1] ~ as.matrix(df[ ,-1]), data = df)
  
  message("Your model is:")
  print(model)
  print(summary(model))
  
  #shows how well predicted values are to actual values
  plot(model,2)
  
  dev.off()
  
  message("You want your points to follow the line closely.")
  message("In the example data, the points deviate at the far ends of the QQ plot.")
  message("This means I would not trust predictions for extreme values.")
  
  #testing the quality of your model
  message("The adjusted Rsquared tells you how much variability the model is explaining.")
  print(summary(model)$adj.r.squared)
  
  # quality scores
  message("The folloing are two quality scores for comparing this model to other models.")
  message("A better score is a lower one.")
  print(AIC(model))
  print(BIC(model))
  
  mylist2 <- list(summary(model)$adj.r.squared, AIC(model), BIC(model))
  return(mylist2)
  
}




