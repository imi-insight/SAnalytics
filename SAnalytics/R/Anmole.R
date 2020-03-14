
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

stransform_bestfit<- function(dependent, independent, dataset)
{
  rsquared<- vector(mode = "numeric", length = 7)
  names(rsquared)<- c("linear", "Quadratic", "Log", "Exponential", "Cosine", "Square Root", "Reciprocal")
  adj_rsquared<- vector(mode = "numeric", length = 7)
  names(adj_rsquared)<- c("linear", "Quadratic", "Log", "Exponential", "Cosine", "Square Root", "Reciprocal")

  jqbera<- vector(mode = "numeric", length = 7)
  names(jqbera)<- c("linear", "Quadratic", "Log", "Exponential", "Cosine", "Square Root", "Reciprocal")

  ## Linear
  m1=lm(dependent~independent,data=dataset)
  adj_rsquared[1]<-summary(m1)$adj.r.squared
  rsquared[1]<- summary(m1)$r.squared


  ## Quadratic
  m2=lm(dependent~independent**2,data=dataset)
  adj_rsquared[2]<-summary(m2)$adj.r.squared
  rsquared[2]<- summary(m2)$r.squared


  ## Natural Log
  m3=lm(dependent~log(independent),data=dataset)
  adj_rsquared[3]<-summary(m3)$adj.r.squared
  rsquared[3]<- summary(m3)$r.squared


  ## Exponential
  m4=lm(dependent~exp(independent),data=dataset)
  adj_rsquared[4]<-summary(m4)$adj.r.squared
  rsquared[4]<- summary(m4)$r.squared


  ## Trigonometric -- cosine
  m5=lm(dependent~cos(independent),data=dataset)
  adj_rsquared[5]<-summary(m5)$adj.r.squared
  rsquared[5]<- summary(m5)$r.squared


  ## Square root
  m6=lm(dependent~sqrt(independent),data=dataset)
  adj_rsquared[6]<-summary(m6)$adj.r.squared
  rsquared[6]<- summary(m6)$r.squared


  ## Reciprocal
  m8=lm(dependent~(1/independent),data=dataset)
  adj_rsquared[7]<-summary(m8)$adj.r.squared
  rsquared[7]<- summary(m8)$r.squared

  return(c(adj_r2=adj_rsquared,r2=rsquared))

}

