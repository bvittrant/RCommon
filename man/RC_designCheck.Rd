\name{RC_designCheck}
\alias{RC_designCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_designCheck
%%  ~~function to do ... ~~
}
\description{Perform a check between your Data and Design dataframe.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RC_designCheck(Data, Design, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{Your initial dataframe containing the gene expression or microarray value for examples}
  \item{Design}{Your dataframe descrining your sample. It should match the Data dataframe.}
}
\details{
It will check if the number of rows are corresponding and if the name are equall with the all.equal function. It will return some message that i advice you to read.
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
RC_designCheck = function(Data, Design){

  # Data checking and error handling
  row_Data = dim(Data)[1]
  col_Data = dim(Data)[2]
  row_Design = dim(Design)[1]
  col_Design = dim(Design)[2]

  if(row_Data != row_Design){
    return("Your design file has not the same number of rows than your data file")
  }

  print("I tested your rownames from Data and Design with the all.equal function:")
  print(all.equal(row.names(Data), row.names(Design)))

  # Everything good function can continue
  print(paste("You have",row_Data,"samples with",col_Data,"features in your Data file.",sep=" "))
  print(paste("And",col_Design,"descriptors for your sample in your Design file:", sep = " "))
  print(colnames(Design))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
