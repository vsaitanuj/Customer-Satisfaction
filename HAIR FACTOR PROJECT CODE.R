
### SETTING UP THE WORKING DIRECTORY ###
setwd("C:/R programs great lakes/ADV STAT")
getwd()


### INVOKING THE NECESSARY LIBRARIES ####
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("Hmisc")
library(Hmisc)
install.packages("reshape2")
library(reshape2)
install.packages("psych")
library(psych)
install.packages("car")
library(car)

### Setting up working directory
setwd("C:/R programs great lakes/ADV STAT")
getwd()
### Loading up the dataset #####
Market = read.csv("Factor-Hair-Revised.csv",header = TRUE)
View(Market)
cor.plot(Market,numbers = TRUE)
### Intial exploration of the dataset ###
summary(Market)
dim(Market)
colnames(Market)
str(Market)
head(Market)
tail(Market)
#### Dataset transformation ###
### Changing the Variable Names according to the given list ##
colnames(Market) = c("Product ID","Product Quality",
                     "E-Commerce", "Technical Support",
                     "Complaint Resolution","Advertising",
                     "Product Line","Salesforce Image",
                     "Competitive Pricing","Warranty & Claims",
                     "Order & Billing","Delivery Speed",
                     "Customer Satisfaction")
colnames(Market)
### Removing the ID variable ###
Market = Market[,-1]
###Creatin a new dataset Market1 without the Satisfaction Variable###
Market1 = Market[,-12]
colnames(Market1)


### Uni-Variate Analysis ###
hist.data.frame(Market1)
ggplot(melt(Market1), aes(variable, value)) + geom_boxplot()
hist(Market$`Customer Satisfaction`,
     main = "Histogram of Customer Satisfaction",
     xlab = "Ratings")
boxplot(Market$`Customer Satisfaction`,
        main = "Boxplot of Customer Satisfaction")

### Bi - Variate Analysis ###
### CUSTOMER SATISFACTION ###
CUSTOMER.SATISFACTION= cut(Market$`Customer Satisfaction`,3,labels = c("Sad","Not Happy","Happy"))
qplot(CUSTOMER.SATISFACTION,fill = CUSTOMER.SATISFACTION)
tab = table(CUSTOMER.SATISFACTION)
View(prop.table(tab)*100)
### PRODUCT QUALITY ####
PRODUCT.QUALITY = cut(Market$`Product Quality`,3,labels = c("Sad","Not Happy","Happy"))
tab1 = table(PRODUCT.QUALITY)
View(prop.table(tab1)*100)
qplot(PRODUCT.QUALITY,fill = PRODUCT.QUALITY)
### E-COMMERCE ###
E.COMMERCE = cut(Market$`E-Commerce`,3,labels = c("Sad","Not Happy","Happy"))
tab2 = table(E.COMMERCE)
View(prop.table(tab2)*100)
qplot(E.COMMERCE,fill = E.COMMERCE)

### Missing Value Treatment ####
sum(is.na(Market))

### Outlier identification ###
outlier2 = function(i)
{
  IQ = IQR(Market[,i])
  z = quantile(Market[,i])
  z= as.data.frame(z)
  colnames(z) = as.factor(colnames(z))
  q1 = z[2,1]
  q3 = z[4,1]
  
  Market2 = Market
  subset1 = Market2[Market2[,i] < q1 - 1.5*IQ,]
  lo = nrow(subset1)
  subset2 = Market2[Market2[,i] > q3 + 1.5*IQ,]
  hi = nrow(subset2)
  Outliers = lo + hi
  print(colnames(Market[i]))
  Outliers
}

### outlier identification ###
outlier2(2)
outlier2(7)
outlier2(10)
outlier2(10)

### Question 2 (Collinearity) ####

### Correlation Matrix and Correlation Plot ###
cor.plot(Market1,numbers = TRUE)
View(cor(Market1))
### Checking the Eigen Values ####
Eigen = eigen(cor(Market1))
Eigen$values
### Checking the Regression model ###
p = lm(`Customer Satisfaction`~., data = Market)
summary(p)
### Checking the VIF values ###
vifmatrix = vif(p)
summary(vifmatrix)
colnames(Market)

### Question 3 ###

### Product Quality ###
slm1 = lm(`Customer Satisfaction` ~ `Product Quality`,data = Market)
summary(slm1)
p1 = predict(slm1)
plot(p1,col = "Red")
plot(Market$'Product Quality',col = "Blue")
lines(p1,col = "Red")
lines(Market$`Product Quality`,col = "Blue")
confint(slm1)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Product Quality`))+geom_point(col = "Black")+stat_smooth(method = "lm",col = "White")

### E-Commerce ###
slm2 = lm(`Customer Satisfaction` ~ `E-Commerce`,data = Market)
summary(slm2)
p2 = predict(slm2)
plot(p2,col = "Red")
plot(Market$`E-Commerce`,col = "Blue")
lines(p2,col = "Red")
lines(Market$`E-Commerce`,col = "Blue")
confint(slm2)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `E-Commerce`))+geom_point(col = "Blue")+stat_smooth(method = "lm",col = "Black")

### Technical Support ###
slm3 = lm(`Customer Satisfaction` ~ `Technical Support`,data = Market)
summary(slm3)
p3 = predict(slm3)
plot(p3,col = "Red")
plot(Market$`Technical Support`,col = "Blue")
lines(p3,col = "Red")
lines(Market$`Technical Support`,col = "Blue")
confint(slm3)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Technical Support`))+geom_point(col = "Blue")+stat_smooth(method = "lm",col = "White")

### Complaint Resolution ###
slm4 = lm(`Customer Satisfaction` ~ `Complaint Resolution`,data = Market)
summary(slm4)
p4 = predict(slm4)
plot(p4,col = "Red")
plot(Market$`Complaint Resolution`,col = "Blue")
lines(p4,col = "Red")
lines(Market$`Complaint Resolution`,col = "Blue")
confint(slm4)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Complaint Resolution`))+geom_point(col = "Blue")+stat_smooth(method = "lm",col = "Red")

### Advertising ###
slm5 = lm(`Customer Satisfaction` ~ `Advertising`,data = Market)
summary(slm5)
p5 = predict(slm5)
plot(p5,col = "Red")
plot(Market$`Advertising`,col = "Blue")
lines(p5,col = "Red")
lines(Market$`Advertising`,col = "Blue")
confint(slm5)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Advertising`))+geom_point(col = "Black")+stat_smooth(method = "lm",col = "Green")

### Product Line ###
slm6 = lm(`Customer Satisfaction` ~ `Product Line`,data = Market)
summary(slm6)
p6 = predict(slm6)
plot(p6,col = "Red")
plot(Market$`Product Line`,col = "Blue")
lines(p6,col = "Red")
lines(Market$`Product Line`,col = "Blue")
confint(slm6)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Product Line`))+geom_point(col = "Black")+stat_smooth(method = "lm",col = "Dark Blue")

### Salesforce Image ###
slm7 = lm(`Customer Satisfaction` ~ `Salesforce Image`,data = Market)
summary(slm7)
p7 = predict(slm7)
plot(p7,col = "Red")
plot(Market$`Salesforce Image`,col = "Blue")
lines(p7,col = "Red")
lines(Market$`Salesforce Image`,col = "Blue")
confint(slm7)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Salesforce Image`))+geom_point(col = "Black")+stat_smooth(method = "lm",col = "Brown")

### Competitive Pricing ###
slm8 = lm(`Customer Satisfaction` ~ `Competitive Pricing`,data = Market)
summary(slm8)
p8 = predict(slm8)
plot(p8,col = "Red")
plot(Market$`Competitive Pricing`,col = "Blue")
lines(p8,col = "Red")
lines(Market$`Competitive Pricing`,col = "Blue")
confint(slm8)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Competitive Pricing`))+geom_point(col = "Red")+stat_smooth(method = "lm",col = "Green")

### Warranty & Claims ###
slm9 = lm(`Customer Satisfaction` ~ `Warranty & Claims`,data = Market)
summary(slm9)
p9 = predict(slm9)
plot(p9,col = "Red")
plot(Market$`Warranty & Claims`,col = "Blue")
lines(p9,col = "Red")
lines(Market$`Warranty & Claims`,col = "Blue")
confint(slm9)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Warranty & Claims`))+geom_point(col = "Black")+stat_smooth(method = "lm",col = "Maroon")

### Order & Billing ###
slm10 = lm(`Customer Satisfaction` ~ `Order & Billing`,data = Market)
summary(slm10)
p10 = predict(slm10)
plot(p10,col = "Red")
plot(Market$`Order & Billing`,col = "Blue")
lines(p10,col = "Red")
lines(Market$`Order & Billing`,col = "Blue")
confint(slm10)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Order & Billing`))+geom_point(col = "Red")+stat_smooth(method = "lm",col = "Yellow")

### Delivery Speed ###
slm11 = lm(`Customer Satisfaction` ~ `Delivery Speed`,data = Market)
summary(slm11)
p11 = predict(slm11)
plot(p11,col = "Red")
plot(Market$`Delivery Speed`,col = "Blue")
lines(p11,col = "Red")
lines(Market$`Delivery Speed`,col = "Blue")
confint(slm11)
ggplot(Market,aes(x = `Customer Satisfaction`,y = `Delivery Speed`))+geom_point(col = "Blue")+stat_smooth(method = "lm",col = "Yellow")

### Question 4 ###

### Barlett Test ####
bartlett.test(Market)
### Principal Component Analysis ###
EigenPCA = eigen(cor(Market1))
EigenPCA$values
PCA = principal(Market,nfactors = 11,rotate = "varimax")
print(PCA)
Var.Eigen<-EigenPCA$values/sum(EigenPCA$values)*100 
cumsum(Var.Eigen)
### Factor Analysis ###
EigenFA = eigen(cor(Market1))
EigenFA$values
plot(EigenFA$values,col = "Blue",main = "Scree Plot for Factor Analysis")
abline(a = 1,b = 0,col = "Red")
### Without Rotation ###
UnrotatedFA = principal(Market1,nfactors = 4,rotate = "none")
print(UnrotatedFA)
print(UnrotatedFA$loadings,cutoff = 0.4)
### With Rotation ###
RotatedFA = principal(Market1, nfactors = 4 , rotate = "varimax")
print(RotatedFA)
### FA Diagram ####
fa.diagram(RotatedFA)

###Question 5 ###

###Using Factor Analysiss ###
### Extracting the Factor scores and Naming them ###
Market.factors = data.frame(Market$`Customer Satisfaction`,RotatedFA$scores)
colnames(Market.factors) = c("Customer Satisfaction",
                             "Shopping Experience",
                             "Product Marketing",
                             "After Sales Assistance",
                             "Valuation of the product")
str(Market.factors)
### Checking the Factors for Correlation with Customer Satisfaction and MultiCollinearity ###
cor.plot(Market.factors,numbers = TRUE)
### Creating a Multiple Regression Model using the new factors ###
MR = lm(`Customer Satisfaction`~.,data = Market.factors)
summary(MR)
MR1 = lm(`Customer Satisfaction`~`Valuation of the product`+
           `Product Marketing`+`Shopping Experience`,data = Market.factors)
summary(MR1)
###Creation of new interaction effect ###
Product.Appeal = Market.factors$`Product Marketing`*Market.factors$`Valuation of the product`
MR2 = lm(`Customer Satisfaction`~`Shopping Experience`+`Product Marketing`+
            `Valuation of the product`+Product.Appeal,data = Market.factors)
summary(MR2)

###Plotting against the predicted values ###
MRP = predict(MR2)
plot(MRP,col = "Red")
lines(MRP,col = "Red")
lines(Market$`Customer Satisfaction`,col = "Blue")
### Finding Upper and Lower limits ###
confint(MR2)
ggplot(Market,aes(x = Market$`Customer Satisfaction`,y = MRP))+
  geom_point(col = "red1")+stat_smooth(method = "lm",col = "blue")


