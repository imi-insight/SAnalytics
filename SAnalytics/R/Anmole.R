#' @title exports a histogram
#'
#' @description this function exports a histogram
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples  sgraph_histogram(iris$Sepal.Length,"Sepal Length","Histogram for sepal length")
#'
#' @export sgraph_histogram
sgraph_histogram<-function(x,x.title,title)
{
  library(ggplot2)
  qplot(x,
        geom="histogram",
        main = title,
        xlab = x.title,
        fill=I("blue"),
        col=I("red"),
        alpha=I(.2))

}

#' @title exports a histogram with color options
#'
#' @description this function exports a histogram with color options
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples  sgraph_histogram(iris$Sepal.Length,"Sepal Length","Histogram for sepal length","red","blue")
#'
#' @export sgraph_histogram

sgraph_histogram<-function(x,x.title,title,fillcolor,colcolor)
{
  library(ggplot2)
  qplot(x,
        geom="histogram",
        main = title,
        xlab = x.title,
        fill=I(fillcolor),
        col=I(colcolor),
        alpha=I(.2))

}

