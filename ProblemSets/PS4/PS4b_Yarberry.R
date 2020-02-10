# Creating df1 
df1 <- iris 

#SparkDataFrame 
createDataFrame(df <- iris)

#class
class(df1)
View(df1)
class(df)

#first 6 rows 
install.packages('dplyr')
library('dplyr')
head(select(df, df$Sepal_Length, df$Species))
head(select(df1, df1$Sepal_Length, df1$Species))

#filter first 6 rows
library('dplyr')
head(filter(df, df$Sepal_Length > 5.5))
head(filter(df1, df1$Sepal_Length > 5.5))

#combined
head(select(df, df$Species, filter(df$Sepal_Length > 5.5)))
head(select(df1, df1$Species, filter(df1$Sepal_Length > 5.5)))

#RDD operations 
head(summarize(groupBy(df, df$Species), mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))
head(summarize(groupBy(df1, df1$Species), mean=mean(df1$Sepal_Length), count=n(df1$Sepal_Length)))