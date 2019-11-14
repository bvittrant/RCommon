\name{RC_plsda}
\alias{RC_plsda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_plsda}
\description{Perform a single or corrected PLS-DA from the mixOmics package.}
\usage{
RC_plsda(Data, Design, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{Your initial dataframe containing the gene expression or microarray value for examples.}
  \item{Design}{Your dataframe descrining your sample. It should match the Data dataframe.}
  \item{Name_project}{A title name for your project. You should define it as an env variable at the beginning of your work.}
  \item{ColorVar}{The main feature you want to use in the PLS-DA. It should be an existing column from your Design dataframe.}
  \item{CorrectVar}{A feature you want to use to do a multilevel PLS-DA. This will be the feature used to correct the first.}
}
\details{ If only ColorVar is provided then do a single level PLS-DA. If CorrectVar is provided then do a multi-level PLS-DA corrected for CorrectVar.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Return a graph object
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://mixomics.org/
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
RC_plsda = function(Data, Design, Name_Project, ColorVar, CorrectVar = NULL, ...){
  # ColorVar & CorrectVar, any column from your design file
  # If only ColorVar is provided then it's a single level PLS-DA
  # If CorrectVar is provided it's a pls-da on ColorVar corrected for CorrectVar
  tmp1 = ColorVar
  tmp2 = CorrectVar

  # PLSDA
  if(is.null(tmp2)) {
    # Simple level PLSDA mixOmics
    plsda = plsda(Data, Design[,tmp1], ncomp = 10, ...)
    p = plotIndiv(plsda , comp = 1:2,
                  group = Design[,tmp1], ind.names = FALSE,
                  ellipse = TRUE, legend = TRUE, title = paste("Simple PLSDA - ",tmp1, sep=""))
    return(p)
  }

  if(!is.null(tmp2)){
    # Multilevel PLSDA mixOmics
    plsda = plsda(Data, Design[,tmp1], ncomp = 10, multilevel = Design[,tmp2], ...)
    p = plotIndiv(plsda , comp = 1:2,
                  group = Design[,tmp1], ind.names = FALSE,
                  ellipse = TRUE, legend = TRUE,
                  title = paste("Multilevel PLSDA - ",tmp1," vs ", tmp2, sep="")
    )
    return(p)
  }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
