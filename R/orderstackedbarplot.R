data1 <- rnorm(10, mean = 10, sd = 1.7)
data2 <- rnorm(10, mean = 7, sd = 0.5)
data3 <- rnorm(10, mean = 14, sd = 4)
data4 <- rnorm(10, mean = 8, sd = 2)
lets <- letters[1:10]
my.df <- data.frame(lets,data1,data2,data3,data4)
library(reshape2)
library(ggplot2)
my.df

### Melt Data together across variable:
melted.df <- melt(my.df)
melted.df
colnames(melted.df)
str(melted.df)

### Order bars function

order.Levels <- function(func){
copy.df <- melted.df
ordering <- order(as.vector(sapply(split(copy.df, copy.df$variable),function(x)func(x$value))))
ordered.Variables <- levels(copy.df$variable)[ordering]
copy.df$variable <- factor(copy.df$variable, levels = ordered.Variables)
copy.df
}

viz.df <- order.Levels(function(x)rev(mean(x)))

str(order.Levels(function(x)mean(x)))

base <- ggplot(data = viz.df, aes(x = lets, y = value, fill = variable))
base + geom_bar(stat = "identity", position = "stack", aes(order = variable))

### Show together

require(gridExtra)
meanPlot <- {
  base <- ggplot(data = order.Levels(mean), aes(x = lets, y = value, fill = variable))
  base + geom_bar(stat = "identity", position = "fill", aes(fill = variable, order = variable))
}
varPlot <- {
  base <- ggplot(data = order.Levels(var), aes(x = lets, y = value, fill = variable))
  base + geom_bar(stat = "identity", position = "fill", aes(fill = variable, order = variable))
}
grid.arrange(meanPlot, varPlot)
