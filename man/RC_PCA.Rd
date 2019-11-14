\name{RC_PCA}
\alias{RC_PCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_PCA}
\description{Just do a PCA or a colored PCA plot.}
\usage{
RC_PCA(Data, Design, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{Your initial dataframe containing the gene expression or microarray value for examples}
  \item{Design}{Your dataframe descrining your sample. It should match the Data dataframe.}
  \item{Name_project}{A title name for your project. You should define it as an env variable at the beginning of your work.}
  \item{ColorVar}{The feature you want to use to color the PCA plot.}
}
\details{Just do a PCA from the FactoExtra package. If Choice is provided then color the PCA and add ellipses.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Return a graph object.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://www.sthda.com/french/wiki/fviz-pca-visualisation-de-l-analyse-en-composante-principale-logiciel-r-et-analyse-de-donn-es
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
RC_PCA = function(Data, Design, Project_Name, ColorVar = 0){
  tmp = ColorVar
  # PCA run
  res.pca = PCA(Data,  graph = FALSE)

  # PCA plot
  if(tmp == 0) {
    p = fviz_pca_ind(res.pca, label = "none") # hide individual labels)
  }else{
    p = fviz_pca_ind(res.pca,
                     label = "none", # hide individual labels
                     habillage = Design[, tmp], # color by groups
                     palette = Design[, tmp] %>% unique %>% length %>% viridis,
                     addEllipses = TRUE # Concentration ellipses
    )
  }

  # The function will return:
  return(p)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
