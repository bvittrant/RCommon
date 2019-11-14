\name{RC_boxplot}
\alias{RC_boxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RC_boxplot}
\description{This function create a boxplot using a melted data frame as it is normaly used with Reshape and ggplot. It is focused on a specific value (e.g gene) and the fill, color options are related to other columns of your choice you have in the melted dataframe. It also plot an horizontal line on the graph, this the threshold value we use in RNA-Seq to filter our gene count.
}
\usage{
RC_boxplot(Data_melt, Name_project, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data_melt}{A melted dataframe from your Data and Design. You should use the RC_melt function to create this or directly the reshape2::melt function.}
  \item{Name_project}{A title name for your project. You should define it as an env variable at the beginning of your work.}
  \item{xVar}{The feature you want to focus on from your column variables in your melted dataframe (e.g PDCD1).}
  \item{colVar}{The Design feature you want to use to split the data on the boxplot. It should be a column name in your melted dataframe.}
  \item{Hline}{If provided, just plot an horizontal line.}
}
\details{
The function use the geom_boxplot option to draw useful boxplot. The choice2 is used in the color option in aes_string() to split the boxplot. If Hline is not NULL it will add a simple horizontal line on the boxplot. It is useful to see if the differents boxplot are in range or not from the filter count used in RNA-Seq for example.
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
RC_boxplot = function(Data_melt, Name_Project, choice1, choice2, Hline = NULL){
  # choice1, a value from the column variable from data_melt (e.g. a gene, a proteine etc)
  # choice2, any column from your design file that was used to create the data_melt
  #Hline, value to plot on horizontal line
  tmp = ggplot(Data_melt[Data_melt$variable == choice1,], aes(x = variable, y = value)) +
    geom_boxplot(aes_string(col = choice2),position=position_dodge(1) ) +
    ggtitle(Name_Project)+ xlab(choice1) + ylab("")

  if(!is.null(Hline)){
    tmp = tmp +
      geom_hline(yintercept=Hline, linetype="dashed", color = "black")
  }

  return(tmp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ggplot2 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~boxplot }% __ONLY ONE__ keyword per line
