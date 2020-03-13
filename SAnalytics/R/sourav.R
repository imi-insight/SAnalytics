sgraph_scatterplot<-function(x,x.title,y.title="",title,
                           binwidth=10)
{
  
  ggplot2::plot(x,
                 geom="scatter plot",
                 main = title,
                 binwidth=binwidth,
                 ylab=y.title,
                 xlab = x.title,
                 fill=I(fillcolor),
                 col=I(colcolor),
                 alpha=I(alpha))
  
}
