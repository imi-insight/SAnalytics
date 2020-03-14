\name{sgraph_bar}
\alias{sgraph_bar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot bar chart of a variable inside a data frame
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sgraph_bar(x,"variable","color")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data frame concerned
}
  \item{"variable"}{
variable name inside the data frame inside "inverted comma"
}
  \item{"color"}{
Preferred color inside "inverted comma"
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
Function created by Souvik Samanta
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

sgraph_bar(iris,"sepal.length","lightblue")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
