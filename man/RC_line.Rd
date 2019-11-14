\name{RC_line}
\alias{RC_line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_line}
\description{A simple line plot with color split.}
\usage{RC_line(Data_melt, Name_project, ...)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data_melt}{A melted dataframe from your Data and Design. You should use the RC_melt function to create this or directly the reshape2::melt function.}
  \item{Name_project}{A title name for your project. You should define it as an env variable at the beginning of your work.}
  \item{choice1}{The feature you want to focus on from your column variables in your melted dataframe (e.g PDCD1).}
  \item{choice2}{The feature you want to see on x axis from your Design columns.}
  \item{choice3}{The Design feature you want to use to split the data on the boxplot. It should be a column name in your melted dataframe.}
}
\details{
A simple line plot
%%  ~~ If necessary, more details than the description above ~~
}
\value{The function return a ggplot graph object. You can save it and add any ggplot line with the + geom_ ... .
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
%% ~put references to the literature/web site here ~
}
\author{
Benjamin Vittrant
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
\keyword{ ~ggplot }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~line }% __ONLY ONE__ keyword per line
