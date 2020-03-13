# R Library Project - Bar chart

# install.packages("ggplot2")


head(iris)

library(ggplot2)
sgraph_bar <- function(data.main,data.col)
{
     #y <- as.character(k)
     tt <- colnames(data.col)
     ggplot(data.main, aes(data.col))+
       geom_bar(fill = "lightblue",color = "black")+
      # xlab(tt) +
       ylab("Count")+
       theme_classic()
}

sgraph_bar(iris,iris$Sepal.Length)

































