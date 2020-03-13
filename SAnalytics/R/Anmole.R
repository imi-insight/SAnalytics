sgraph_histogram<-function(x,x.title,y.title="",title,
                           fillcolor="blue",colcolor="white",
                           alpha=I(1),binwidth=10)
{

  ggplot2::qplot(x,
        geom="histogram",
        main = title,
        binwidth=binwidth,
        ylab=y.title,
        xlab = x.title,
        fill=I(fillcolor),
        col=I(colcolor),
        alpha=I(alpha))
}
#devtools::document()

desc_summary <- function(col_name)
{
  nmiss <- sum(is.na(col_name))   #no. of missing values in dataset
  clean_col <- col_name[!is.na(col_name)]         # all non missing values will be assigned to a
  m <- mean(clean_col)
  n <- length(clean_col)
  s <- sd(clean_col)
  med=median(clean_col)
  min <- min(clean_col)
  p1 <- quantile(clean_col,0.01)
  p99 <- quantile(clean_col,0.99)
  p2 <- quantile(clean_col,0.02)
  p98 <- quantile(clean_col,0.98)
  max <- max(clean_col)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag <- max>p98 | min<p2
  return(c(n=n, nmiss=nmiss,
           outlier_flag=outlier_flag,
           mean=m,median=med,stdev=s, min=min,
           p1=p1, p99=p99,p2_=p2,p98_=p98, max=max,UC=UC, LC=LC))
}

