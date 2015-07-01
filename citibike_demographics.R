# Read data
demo <- read.csv("demographics.csv")

# Install libraries
library(ggplot2)

demo_math <- demo[c(1:12),c(3:19)]
str(demo_math)
demo_math$Population <- as.numeric(as.character(demo_math$Population))

# get means for variables in data frame mydata
# excluding missing values 
means <- sapply(demo_math, mean, na.rm=TRUE)
min <- sapply(demo_math, min, na.rm=TRUE)
max <- sapply(demo_math, max, na.rm=TRUE)


merged_stats <- cbind(means, min, max)

ggplot(demo, aes(x=Mean.earnings)) + geom_density() +
  xlab("Mean Earnings") + ggtitle("Distribution of Mean Earnings in NYC")


new_demo <- demo[,c(4:19)]
str(new_demo)
new_demo$Population <- as.numeric(as.character(new_demo$Population))

correlation <- cor(new_demo)
View(correlation)
