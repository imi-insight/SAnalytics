\name{split_csection}
\alias{split_csection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gives the train and test data for Cross Sectional data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
split_csection(data,data.seed,train.data.percent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame concerned
}
  \item{data.seed}{
The seed that user wants to set for the data splitting
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
split_csection(iris,10,80)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
