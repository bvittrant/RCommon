\name{RC_transpose}
\alias{RC_transpose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_transpose
%%  ~~function to do ... ~~
}
\description{Just a function to transpose a dataframe in case of you IT is afraid of giving you some RAM because you already burnt 3 times the servers with you shitty skills in programming.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RC_transpose(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{The Dataframe you want to transpose}
  \item{verbose}{If TRUE then the function will prompt i for every i modulo 100 == 0. Set to TRUE by default.}
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
\references{Type Transpose on google.
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
  RC_transpose = function(Data, verbose = T){
    tmp = matrix(ncol = nrow(Data), nrow = ncol(Data), NA) %>% as.data.frame
    # We just loop to transfert row in col and col in row
    for(i in 1:nrow(tmp)){
      tmp[i,] = Data[,i]
      if(verbose == T & i%%100 == 0){print(i)}
    }
    colnames(tmp) = row.names(Data)
    rownames(tmp) = colnames(Data)
    return(tmp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
