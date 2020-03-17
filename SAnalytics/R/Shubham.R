#Splitting
split_tseries <- function(data,train.data.percent)
{
  number.of.rows <- nrow(data)
  train.data.value.count <- train.data.percent*nrow(data)*.01
  test.data.value.count <- nrow(data) - train.data.percent*nrow(data)*.01
  train.data <- data.frame(data[1:train.data.value.count,])
  test.data.index <- train.data.value.count+1
  test.data <- data.frame(data[test.data.index:number.of.rows,])
  return(c(train.data=train.data,test.data=test.data))
}

# split_tseries(iris,10)

split_csection <- function(data,data.seed,train.data.percent)
{
  set.seed(data.seed)
  train.index <- sample(c(1:dim(data)[1]), dim(data)[1]*0.01*train.data.percent)
  train.data <- data[train.index, ]
  test.data <- data[-train.index, ]
  return(c(train.data=train.data,test.data=test.data))
}

# split_csection(iris,10,80)
