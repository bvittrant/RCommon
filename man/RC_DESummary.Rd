\name{RC_DESummary}
\alias{RC_DESummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_DESummary
%%  ~~function to do ... ~~
}
\description{Create a dataframe with the different number of differentially expressed objects in function of the provided threshold.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RC_DESummary(res, FC, r_FC ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{A dataframe containing the pvalue and Fold change you want to iterate on.}
  \item{FC}{The colname corresponding to the Fold Change you want to use from res.}
  \item{r_FC}{A sequence of values you want to iterate on. You can the seq() functions as seq(1.2,1.8,0.1}
  \item{padj}{The colname corresponding to the pvalue you want to use from res}
  \item{r_pval}{A sequence of value you to iterate on. You can the seq() functions as seq(0.01,0.05,0.1)}
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Return a dataframe with the number of features DE for each couple of value provided with the sequences.
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
