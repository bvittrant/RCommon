\name{RC_PCA_loading}
\alias{RC_PCA_loading}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_PCA_loading
%%  ~~function to do ... ~~
}
\description{Return the loading (V matrix) plot of a PCA.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RC_PCA_loading(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Your data to use to set up the PCA.}
  \item{cos2}{A value between 0:1 to select the features to plot by contribution.}
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
}
\references{
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
RC_PCA_loading = function(data, cos2){
  res.pca = PCA(data, graph = FALSE)
  tmp = median(res.pca$var$contrib)

  p = fviz_pca_var(res.pca, col.var="contrib", select.var = list(cos2=cos2)) +
    scale_color_gradient2(low="white", mid="blue",
                          high="red", midpoint=tmp, space = "Lab") +
    theme_minimal()
  return(p)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
