\name{RC_plsda_loading}
\alias{RC_plsda_loading}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_plsda_loading
%%  ~~function to do ... ~~
}
\description{Perform a PLS-DA in the same way as RC_PLSDA1 but return the loadings plot of the desired componant.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RC_plsda_loading(Data, Design, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{Your initial dataframe containing the gene expression or microarray value for examples.}
  \item{Design}{Your dataframe descrining your sample. It should match the Data dataframe.}
  \item{Name_project}{A title name for your project. You should define it as an env variable at the beginning of your work.}
  \item{comp}{The componant you want to plot (1,2,3 ...).}
  \item{choice1}{The main feature you want to use in the PLS-DA. It should be an existing column from your Design dataframe.}
  \item{choice2}{A feature you want to use to do a multilevel PLS-DA. This will be the feature used to correct the first.}
}
\details{ If only choice1 is provided then do a single level PLS-DA. If choice2 is provided then do a multi-level PLS-DA corrected for choice2.
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
RC_PLSDA1 = function(Data, Design, Name_Project, choice1, choice2 = NULL, ...){
  # Choice1 & choice2, any column from your design file
  # If only choice1 is provided then it's a single level PLS-DA
  # If choice 2 is provided it's a pls-da on choice1 corrected for choice2
  tmp1 = choice1
  tmp2 = choice2

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
