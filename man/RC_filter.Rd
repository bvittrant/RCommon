\name{RC_filter}
\alias{RC_filter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_filter}
\description{Take a Dataframe and return a transposed and filtered dataframe.}
\usage{
RC_filter(Data, Filter, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{Your initial dataframe containing the gene expression or microarray value for examples}
  \item{Filter}{The value you want ot filter on. In our case this value is based on the graph from the RC_Kernel function. It is a background noise filter.}
}
\details{Filter a dataframe on threshold value (Filter) that is present in a specific number of samples (Nsample). The function is based on the code from Damien valour your humble servier musician:
apply(Data_t>Filter,1,FUN=function(x)sum(x=="TRUE")) > NSample
But be careful if you choose a threshold from a graph with transformed data, just do the back transfo to get the real value you to filter on raw data.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4728800/
%% ~put references to the literature/web site here ~
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
RC_filter = function(Data, Filter, Nsample){
  # Filter the row in Data which have at least Nsample with more
  # than (2^Filter-1) counts
  # Because we usually choose a filter from a log2 transfo density graph
  # We used this value as filter and then we re-transformed it to be
  # coherent with raw counts
  Data_t = as.data.frame(t(Data))
  tmp = apply(Data_t>Filter,1,FUN=function(x)sum(x=="TRUE")) > NSample
  Data_t_filtered = Data_t[tmp,]
  return(Data_t_filtered)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~filter }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~noise }% __ONLY ONE__ keyword per line
