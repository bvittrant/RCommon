\name{RC_annotationCheck}
\alias{RC_annotationCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_annotationCheck
%%  ~~function to do ... ~~
}
\description{Performe a check between your Data and Annotation dataframe.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RC_annotationCheck(Data, Design)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{Your initial dataframe containing the gene expression or microarray value for examples}
  \item{Design}{Your dataframe descrining your Data. It should match the columns of your Data dataframe.}
}
\details{Check the number of row in Annotation with number of column in Data. It also check the names and give some useful messages.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Return the empty void of dark matter. But is dark matter emtpy ?
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
The experience to avoid futur problems.
%% ~put references to the literature/web site here ~
}
\author{Benjamin Vittrant
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
RC_annotationCheck = function(Data, Annotation){

  # Data checking and error handling
  #row_Data = dim(Data)[1]
  col_Data = dim(Data)[2]
  row_Annotation = dim(Annotation)[1]
  col_Annotation = dim(Annotation)[2]

  if(col_Data != row_Annotation){
    return("Your annotation file has not the same number of rows than your number of columns in your data file")
  }
  if(colnames(Annotation)[1] != "Feature"){
    return("The first column of your annotation file shoudl be called Feature and be the same as the colnames of your data file.")
  }
  if(!all(colnames(Data) == Annotation[,1]) ){
    return("Your rownames in Data don't match feature name in Annotation or are not in the same order !")
  }
  # Everything good function can continue
  print(paste("You have", (col_Annotation-1),"descriptors for your features in your annotation file plus the first column Feature:", sep = " "))
  print(colnames(Annotation))
  #return()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
