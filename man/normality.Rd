\name{normality}
\alias{normality}
\alias{data_check}
\alias{model_quality}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Data Analysis and Simple Model Building
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
function to check normality of data, calculate and report correlations, plot outliers, build a simple model and report quality statistics about the model.
}
\usage{
normality(df)
data_check(df)
model_quality(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
This is the data fram that the model will be build off of.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
normality returns histograms with the normal distribution overlaid, qq plots for each variable, and the results from a shapiro-wilks test for normality. Data check returns the correlation matrix for each variable, and reports if any correlation amoung parameter variables are over .5.  It also reports box plots that show if any outliers are present. Model quality shows how each parameter plotted against the response variable looks, in addition to the full linear model and summary statistics such as quality scores.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Lucas Roberts
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
%## The function is currently defined as
data.frame <- system.file("extdata", "499data.csv", package = "Final_Project")

normality(data.frame)
data_check(data.frame)
model_quality(data.frame)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
