\name{sgraph_histogram}
\alias{sgraph_histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exports a histogram using qplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sgraph_histogram(x, x.title, title, fillcolor, colcolor)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data variable for which histogram is to be created
}
  \item{x.title}{
Title of X axis in the histogram
}
  \item{y.title}{
Title of Y axis in the histogram
}
  \item{title}{
Title of the graph
}
  \item{fillcolor}{
specify the fill color here in quotes. for instance fillcolor="blue"
}
  \item{colcolor}{
specify the border color here in quotes. for instance colcolor="yellow"
}
\item{alpha}{
specify the transparent value.Range between 0 and 1
0 represents completely transparent,1 represents completely opaque
}
\item{binwidth}{
The width of the bins. Can be specified as a numeric value or as a function that calculates width from unscaled x. Here, "unscaled x" refers to the original x values in the data, before application of any scale transformation. When specifying a function along with a grouping structure, the function will be called once per group. The default is to use the number of bins in bins, covering the range of the data. You should always override this value, exploring multiple widths to find the best to illustrate the stories in your data.

The bin width of a date variable is the number of days in each time; the bin width of a time variable is the number of seconds.
}
}
\details{

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
sgraph_histogram(iris$Sepal.Length,"Sepal Length","Histogram for sepal length")
sgraph_histogram(iris$Sepal.Length,"Sepal Length","Histogram for sepal length","red","blue")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
