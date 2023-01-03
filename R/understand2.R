# << Histogram
# 

# load the data
data(iris)
# create histograms for each attribute
par(mfrow=c(1,4))
for(i in 1:4) {
    hist(iris[,i], main=names(iris)[i])
}


# << Density plots
# 

# load packages
library(lattice)
# load dataset

data <- x2008
# create a layout of simpler density plots by attribute
par(mfrow=c(12, 17))
for(i in 12, 17) {
    plot(density(data[,12]), main=names(data)[17])
}
plot(density(data[,12]), main=names(data)[17])

# << Box plot
# 

# load dataset

# Create separate boxplots for each attribute
par(mfrow=c(1,1))
for(i in 12) {
    boxplot(data[,i], main=names(data)[i])
}

# << Bar Plots
# 

# load the package
library(mlbench)
# load the dataset
data(BreastCancer)
# create a bar plot of each categorical attribute
par(mfrow=c(2,4))
for(i in 2:9) {
    counts <- table(BreastCancer[,i])
    name <- names(BreastCancer)[i]
    barplot(counts, main=name)
}
par(mfrow=c(2,4))
for(i in 1:17) {
    counts <- table(data[,i])
    name <- names(data)[i]
    barplot(counts, main=name)
}

# << Missing Plot
# 

# load packages
library(Amelia)
library(mlbench)
# load dataset
data(Soybean)
# create a missing map
missmap(Soybean, col=c("black", "grey"), legend=FALSE)
missmap(data, col=c("black", "grey"), legend=FALSE)
