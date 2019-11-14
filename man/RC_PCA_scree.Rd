\name{RC_PCA_scree}
\alias{RC_PCA_scree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_PCA_scree
}
\description{Just perform a cumulative PCA from the FactoExtra package.
}
\usage{
RC_PCA_scree(Data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{Your initial dataframe containing the gene expression or microarray value for examples}
}
\details{Just a simple wrapping:
RC_PCA_scree = function(Data, Design, Name_Project, ...){
  # PCA cumulative
  res.pca = PCA(Data,  graph = FALSE)
  return(fviz_screeplot(res.pca, addlabels = TRUE, ... ))
}
}
\value{Return a graph object.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{https://cran.r-project.org/web/packages/factoextra/index.html
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
RC_PCA_scree = function(Data, ...){
  # PCA cumulative
  res.pca = PCA(Data,  graph = FALSE)
  return(fviz_screeplot(res.pca, addlabels = TRUE, ... ))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
