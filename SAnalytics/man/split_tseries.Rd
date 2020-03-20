\name{split_tseries}
\alias{split_tseries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gives the train and test data for Time Series data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
split_tseries(data,train.data.percent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame concerned
}
  \item{train.data.percent}{
Percentage of data the user wants to keep as train data
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
Function created by Shubham Jindal
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
split_tseries(iris,90)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
