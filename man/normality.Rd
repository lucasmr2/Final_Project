\name{normality}
\alias{normality}
\alias{data_check}
\alias{model_quality}
\title{
Data Analysis and Simple Model Building
}
\description{
Function to check normality of data, calculate and report correlations, plot outliers, build a simple model and report quality statistics about the model.
}
\usage{
normality(df)
data_check(df)
model_quality(df)
}
\arguments{
  \item{df}{
This is the data fram that the model will be build off of.  See details for more stipulations. 
}
}
\details{
The file uploaded should be a CSV file where the first column is the Y or response varibale you are interested in predicting.  This package works to only analyze data sets with one Y variable. The X or explanitory variabels are in columns 2 through n. This package only looks at simple linear regression models using all X variables in the data file.  If you feel an explanitory variable does not explain more varibility of your response variable, then do not include it in the file.  This package also does not check for significant interactions or significant higher order terms.  It does return plots that will hint to the user if a more complex model is required for the data set. 
}
\value{
\code{normality}:Calling the function \code{normality} will return the number of explanitory variables that this function and other functions will use. This function will also generate a pdf showing the distribution of each variable with the normal distribution curve overlaid for a visual representation of how normal your data is. The pdf will also contain QQ plots for each variable for a second method of visually representing how normal your data is. This function will also print out results from a Shapiro-Wilks test of normality with instructoins on interpretations.

\code{data_check}:Calling the function \code{data_check} will return the correlation matrix of response and predictor variabels. It will then return the values of unique correlations in a more user friendy table. It will also notify the user if large correlations were found between explanitory variabels by printing out which variables and their correlation. This function will then generate a pdf that contains box plots for each variable for assesment of outliers. Notes on how to interpret and what to do with outliers are also printed.

\code{model_quality}: calling the function \code{model_quality} will print a pdf containf plots of each explanitory variable against the response variable. It will also print the model selected and summary statistics about the model. The pdf will also show the QQ plot of the model predicted values against the actual values as an assesment of how well the model can be used for predictive purposes. The model will then return the adjusted R square, the Akike information criterion, and the Bayesian information criterion as qulality score for how well the model is explaining the data. 
}
\author{
Lucas Roberts
}
\note{
\code{normality}
If the number of explanitory varibles returned is not what you intendted, check the details section and reformat your data. The histogram of each variable should be apporximately normal and follow the green normality distribution.  If you see left or right skewed data or platykurtic data, consider transforming your data via centering or scaling. QQ plots should follow the 1/1 line closely.  If points deviate from the predicted line consider transforming your data. 

\code{data_check}

High correlations are wanted amoung predictor and explanitory varriables.  This means that the higher the correlations the better the model will be for predictive purposes. Unique correlations are only reported between explanitory variabels.  0.5 is the standard for considering a correlation modereate.  Values above 0.8 are considered highly correlated and should be considered for combining the two traits into one variable in a multivariate method. Outliers are not automatically an issue, remove with caution and only if their leverage score are grealty that of other points. 

\code{model_quality}
You would like to see roughly linear responses for each explanitory variable plotted against the response variable.  The model will take into account all variabels together, but if obvious parametric responses are seen the user should consider looking at term interactions or higher order models which this package does not asses. The QQ plot should be very linear, if not then the model is not a good fit of the data and the user should look into interactions or higher order terms in their model. The adjusted R square falls within 0 to 1 and the closer to 1 the better.  This quality score tells the user how much variablility the model is explaining.  If it is lover than .5 the model is not a good fit of the data. The AIC and BIC are used to compare one model to another.  If the plots or R square tell you this model is not good and you fit an interaction or higher order model, the way to compare and decide which model is better is to compare their AIC and BIC scores.  Whichever model has the lowest score for either criterion is the better model that explains the data beter.  If the AIC and BIC do not agree on a model, use BIC which is a more stringent method.  
}
\examples{
datafm <- system.file("extdata", "499data.csv", package = "Final_Project")
mydata <- read.csv(datafm)

normality(mydata)
data_check(mydata)
model_quality(mydata)
}

