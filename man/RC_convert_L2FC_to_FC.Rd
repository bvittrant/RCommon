\name{RC_convert_L2FC_to_FC}
\alias{RC_convert_L2FC_to_FC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_convert_L2FC_to_FC
%%  ~~function to do ... ~~
}
\description{Convert a vector of L2FC value into FC
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RC_convert_L2FC_to_FC(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{L2FC}{The vector containing the L2FC values.}
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
\references{Laura xuereb code !
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
RC_convert_L2FC_to_FC = function(res, L2FC){
  return(sign(L2FC)*2^abs(L2FC))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
