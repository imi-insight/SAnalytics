\name{stransform_bestfit}
\alias{stransform_bestfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks for best fit transformation using linear model's R-square value
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stransform_bestfit(dependent, independent, dataset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dependent}{
dependent variable ie y is added here for the equation y=c+ax
}
  \item{independent}{
independent variable i.e. x is added here for the equation y=c+ax
}
  \item{dataset}{
complete dataframe is added here
}
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
Function Written by Anmole Dewan
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
stransform_bestfit(mpg$,df2$AGE,df2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
