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
RC_volcano = function(res, t_pvalue, t_FC, contrast2, Fold, Stat, xlim, np){
  # res, a dataframe containing DE results from any package
  # t_pvalue & t_FC are defined threshold by yourself
  # contrast2, just a name
  # Fold, the colname to use as FC value from your res
  # Stat, the colname to use a statistics from your res,
  # Number of name to plot in the graph
  # xlim, just xlim for the plot
  # np = number of point label to show
  res_VP = res %>% as.data.frame
  res_VP = na.omit(res_VP)
  res_VP$names = row.names(res_VP) %>% as.factor
  res_VP$names = row.names(res_VP)

  # Color for up, none and down features
  Up = "#E64B35"
  None = "Gray"
  Down = "#3182bd"

  # Add a color column
  res_VP$color = "None"
  res_VP[(res_VP[, Stat] < t_pvalue & res_VP[, Fold] > t_FC),"color"] = "Up"
  res_VP[(res_VP[, Stat] < t_pvalue & res_VP[, Fold] < -t_FC),"color"] = "Down"
  res_VP$Y = -log10(res_VP[, Stat])

  g = ggplot(res_VP, aes_string(x=Fold, y = "Y")) +
    # Add points
    geom_point(aes(text=names, colour = factor(color))) +
    # Add horizontal line
    geom_hline(yintercept=-log10(t_pvalue), linetype="dashed", size = 0.3) +
    # Add vertical lines
    geom_vline(xintercept = -t_FC, linetype="dashed", size = 0.3  ) +
    geom_vline(xintercept = t_FC, linetype="dashed", size = 0.3) +
    # Add axes labels
    xlab(Fold) + ylab(paste("-log10", Stat, sep = " ")) +
    # Define xlim
    xlim(c(-xlim, xlim)) +
    # Chose theme and color
    theme_bw() + scale_color_manual(values = c("Up" = Up,
                                               "Down" = Down,
                                               "None" = None)) +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
    # Add title
    ggtitle(paste(contrast2, collapse = " vs ")) +
    # Add point names
    #geom_text(aes(label=ifelse(abs(FC)> n, names,'')), nudge_x = 1, nudge_y = 1)
    geom_text_repel(data = top_n(res_VP, n = np, wt = abs(FC)),
                    mapping = aes(label = names),
                    fontface = 'bold',color = 'black',
                    box.padding = unit(0.5, "lines"), point.padding = unit(0.5, "lines"))

  return(g)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
