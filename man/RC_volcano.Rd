\name{RC_volcano}
\alias{RC_volcano}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_volcano
%%  ~~function to do ... ~~
}
\description{Create a volcano plot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RC_volcano(res, t_pvalue, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{A dataframe containing differential analysis results. Typically from DESeq2 or limma.}
  \item{t_pvalue}{A threshold for your statistical value.}
  \item{t_FC}{A threshold for your fold value.}
  \item{contrast2}{A title for your graph.}
  \item{Fold}{The name of the column to use for the fold value from the res dataframe.}
  \item{Stat}{The name of the column to use for the statistical value from the res dataframe}
  \item{xlim}{Just some limits to play with if you have huge fold value.}
  \item{np}{Number of genes to plot in the graph.}
  \item{list_names}{A vector containing names of the point you want to color in green. Can be the results of other DE analysis to check visually the overlap.}
}
\details{Plot a volcano with the different values provided. You can use any dataframe from differential analyse results. You just need a pvalue, a fold and some threshold.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Return a ggplot graph object.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
If you know where the volcano plot come from just let me know !
%% ~put references to the literature/web site here ~
}
\author{Benjamin Vittrant.
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

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
