sgraph_piechart<-function(x,y)
{
  ggplot(x, aes(x = "", y = y, fill = y)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(1, label = ""), color = "white")+
    theme_void()
  
}
