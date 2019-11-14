\name{RC_kernel}
\alias{RC_kernel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_kernel
%%  ~~function to do ... ~~
}
\description{Plot a kernel graph do check the value distribution in your data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RC_kernel(Data_melt, color1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data_melt}{A melted dataframe from your Data and Design. You should use the RC_melt function to create this or directly the reshape2::melt function.}
  \item{color1}{A feature from your Design to split the different shade of your distribution.}
  \item{vertical1}{A value to plot a vertical line at the desired threshold.}
  \item{log2}{if TRUE then the value in the column value of the melted dataframe are log2ged. Set to TRUE by default.}
}
\details{PLot a kenel density graph after performing a log2+1 transfo to check the value distribution. In RNA-Seq this is useful to check the background noise and define a threshold value. Using the color1 option let you see if there are different distribution related to a specific feature. This not desired if yes ...
%%  ~~ If necessary, more details than the description above ~~
}
\value{return a graph object.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://en.wikipedia.org/wiki/Kernel_density_estimation
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
RC_kernel = function(Data_melt, color1, vertical1, Title, log2 = TRUE){
  # Log2 the value
  if(log2 == TRUE){Data_melt$value = log2(Data_melt$value+1) }
  # kernel plot
  p = ggplot(Data_melt, aes(value)) +
    geom_density(aes_string(fill = color1), alpha = 0.1) +
    theme(legend.position="right", legend.text=element_text(size=5)) +
    geom_vline(xintercept = vertical1, linetype="dashed", size = 0.3) +
    labs(x = Title)

  # The function will return:
  return(p)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
