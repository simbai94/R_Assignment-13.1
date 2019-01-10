crime_data <- read.csv('COBRA-YTD2017.csv')
View(crime_data)
cor(crime_data_numeric)
str(crime_data)
summary(crime_data)
cor(crime_data_numeric)
as.matrix(cor(crime_data_numeric) )
# Find out top 5 attributes having highest correlation (select only Numeric features).
sapply(crime_data, is.numeric)

crime_data_numeric <- crime_data[, sapply(crime_data, is.numeric)]
crime_data_numeric <- subset(crime_data_numeric, select = -c(x, y ) )
View(crime_data_numeric)

crime_data_numeric$loc_type[crime_data_numeric$loc_type %in% NA] = median(crime_data_numeric$loc_type,na.rm = TRUE)
crime_data_numeric$MaxOfnum_victims[crime_data_numeric$MaxOfnum_victims %in% NA] = median(crime_data_numeric$MaxOfnum_victims,na.rm = TRUE)


cor(crime_data_numeric, use = "complete.obs", method = "pearson")

as.table(cor(crime_data_numeric, use = "complete.obs", method = "pearson"))

boxplot(crime_data_numeric$loc_type)

# install.packages("Hmisc")
library("Hmisc")


res2 <- rcorr(as.matrix(crime_data_numeric))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res2<-rcorr(as.matrix(crime_data_numeric))
res2$r
res2$P
a <- flattenCorrMatrix(res2$r, res2$P)
colnames(a)
class()


b <- a[order(a$p),]
b

b <- a[order(abs(a$cor)),]

top_five <- tail(x = b,n = 5)

top_five


#part b. Find out top 3 reasons for having more crime in a city.


library(ggplot2)

sort(table(crime_data$UC2.Literal),decreasing = TRUE)
# the first three bars in the plot represents the top 3 reasons of more crime in the city
barplot(sort(table(crime_data$UC2.Literal),decreasing = TRUE))



#part c
correlations <- cor(crime_data_numeric)#represents all the numeric correlations wit crime









