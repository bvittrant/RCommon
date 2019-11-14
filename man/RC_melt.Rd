\name{RC_melt}
\alias{RC_melt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RC_melt
}
\description{
A simple function that creates a kind of "melted" data frame from the melt function in reshape package.
It does the melting directly from the Data and Design object. The result can be easily use with ggplot.
}
\usage{RC_melt(Data, Design)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{An R data frame containing your data as gene expression with genes in column and sample in rows.}
  \item{Design}{An R dataframe describing your samples. Typically the rows in your design file should match the rows in your data file. The colum can represent any descriptive features like batch, group, patient's sex ...}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Return a melted data frame with the same columns you should have in your Design dataframe plus 2 columns variable and value that are related to Data (e.g Variable = genes and value = counts)
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://cran.r-project.org/web/packages/reshape/index.html
}
\author{
Benjamin Vittrant
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
RC_melt = function(Data, Design){
  Data_melt = cbind(Data, Design) %>% melt(c(colnames(Design)))
  return(Data_melt)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~melt }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~ggplot2 }% __ONLY ONE__ keyword per line
