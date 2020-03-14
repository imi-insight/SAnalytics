# R Library Project - Bar chart
sgraph_bar <- function(data.main,data.col,color="lightblue")
{
  y <- subset(data.main, select=data.col)
  z <- as.vector(y[,1])
  ggplot2::ggplot(data.main, aes(z))+
    geom_bar(fill = color,color = "black")+
    xlab(data.col) +
    ylab("Count")+
    theme_classic()
}
