# Multiple regression to predict house prices
## Preliminary

library(readr)
data <- read.table("http://www.stat.ucla. edu/data/moore/TAB1 -2.DAT", header= FALSE , sep="")
data1 <- read_csv("http://www.jaredlander.com/data/housing.csv")
head(data)
dim(data)
summary(data)
pricedt <- c("Total.Units","Year.Built","Gross.SqFt","Market.Value.per.SqFt")

data1 <- data[pricedt]
plot(data1)
#LiveL
fit <- lm(Market.Value.per.SqFt ~ Gross.SqFt + Boro, data = data1)
summary(fit)
colnames(data1)
# 
# t and p value, whats significant and whats not.

#get coefficients
fit$coefficients
# plot coefficients
fit2 <- lm(Market.Value.per.SqFt ~ Total.Units*Gross.SqFt + Boro, data = data1)
fit2$coefficients

#fit for third time using : which is for interaction
fit2 <- lm(Market.Value.per.SqFt ~ Total.Units:Gross.SqFt + Boro, data = data1)

fit3 <- lm(Market.Value.per.SqFt ~ Gross.SqFt*Total.Units*Estimated.Gross.Income, data1)
fit3$coefficients

#Both class and boro are text data
fit4 <- lm(Market.Value.per.SqFt ~ Building.Classification*Boro, data1)
fit4$coefficients


