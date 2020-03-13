
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
