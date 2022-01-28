## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)

## ----setup,echo=FALSE,warning=FALSE-------------------------------------------
library(interpretCI)

## ----eval=FALSE---------------------------------------------------------------
#  #install.packages("devtools")
#  devtools::install_github("cardiomoon/interpretCI")

## -----------------------------------------------------------------------------
# With raw data
meanCI(mtcars,mpg)

## -----------------------------------------------------------------------------
# With raw data, Perform one-sample t-test  
meanCI(mtcars,mpg,mu=23)

## ----echo=FALSE---------------------------------------------------------------
string="Suppose a simple random sample of 150 students is drawn from a population of 3000 college students. Among sampled students, the average IQ score is 115 with a standard deviation of 10. What is the 99% confidence interval for the students' IQ score?"
textBox(string)

## -----------------------------------------------------------------------------
meanCI(n=150,m=115,s=10,alpha=0.01)

## -----------------------------------------------------------------------------
meanCI(iris,Petal.Width,Petal.Length)

## ----echo=FALSE---------------------------------------------------------------
string="The local baseball team conducts a study to find the amount spent on refreshments at the ball park. Over the course of the season they gather simple random samples of 100 men and 100 women. For men, the average expenditure was $200, with a standard deviation of $40. For women, it was $190, with a standard deviation of $20.

The team owner claims that men spend at least $7 more than women. Assume that the two populations are independent and normally distributed."
textBox(string)

## -----------------------------------------------------------------------------
x=meanCI(n1=100,n2=100,m1=200,s1=40,m2=190,s2=20,mu=7,alpha=0.05,alternative="greater")
x

## -----------------------------------------------------------------------------
propCI(n=100,p=0.73,P=0.8,alpha=0.01)

## ----echo=FALSE---------------------------------------------------------------
string="Suppose the Acme Drug Company develops a new drug, designed to prevent colds. The company states that the drug is equally effective for men and women. To test this claim, they choose a a simple random sample of 150 women and 100 men from a population of 12500 volunteers.

At the end of the study, 71% of the women caught a cold; and 63% of the men caught a cold. Based on these findings, can we reject the company's claim that the drug is less effective for men compared to women? Use a 0.05 level of significance."
textBox(string)

## -----------------------------------------------------------------------------
x=propCI(n1=150,n2=100,p1=0.71,p2=0.63,P=0,alternative="greater")
x

## ----eval=FALSE---------------------------------------------------------------
#  interpret(x)

## -----------------------------------------------------------------------------
meanCI(mtcars,mpg)

## -----------------------------------------------------------------------------
meanCI(mtcars,mpg) %>% plot()

## -----------------------------------------------------------------------------
x=meanCI(iris,Sepal.Width,Sepal.Length)
x

## -----------------------------------------------------------------------------
t.test(iris$Sepal.Width, iris$Sepal.Length)

## -----------------------------------------------------------------------------
plot(x,ref="test",side=FALSE)

## -----------------------------------------------------------------------------
data(Anorexia,package="PairedData")
meanCI(Anorexia,Post,Prior,paired=TRUE) %>% plot(ref="test",side=FALSE)

## -----------------------------------------------------------------------------
t.test(Anorexia$Post,Anorexia$Prior,paired=TRUE)

## -----------------------------------------------------------------------------
t.test(Anorexia$Post,Anorexia$Prior,paired=TRUE,alternative="greater",mu=4)

## -----------------------------------------------------------------------------
x=meanCI(Anorexia$Post,Anorexia$Prior,paired=TRUE,alternative="greater",mu=4)
plot(x,ref="test",side=FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  interpret(x)

## ----eval=FALSE---------------------------------------------------------------
#  interpret(x,viewer="browser")

## -----------------------------------------------------------------------------
x=meanCI(iris,Species,Sepal.Length,mu=0) 
x

## -----------------------------------------------------------------------------
plot(x)

## -----------------------------------------------------------------------------
meanCI(iris) %>% plot()

## -----------------------------------------------------------------------------
iris %>% select(ends_with("Length")) %>% meanCI() %>% plot()

## -----------------------------------------------------------------------------
data(anscombe2,package="PairedData")
anscombe2

## -----------------------------------------------------------------------------
meanCI(anscombe2,idx=list(c("X1","Y1"),c("X4","Y4"),c("X3","Y3"),c("X2","Y2")),paired=TRUE,mu=0) %>% plot()

## -----------------------------------------------------------------------------
x=meanCI(anscombe2,idx=list(c("X1","X2","X3","X4"),c("Y1","Y2","Y3","Y4")),paired=TRUE,mu=0)
plot(x)

## -----------------------------------------------------------------------------
library(tidyr)
longdf=pivot_longer(anscombe2,cols=X1:Y4)
x=meanCI(longdf,name,value,idx=list(c("X1","X2","X3","X4"),c("Y1","Y2","Y3","Y4")),paired=TRUE,mu=0)
plot(x)

## -----------------------------------------------------------------------------
meanCI(acs,DM,age,sex) %>% plot()

## ----warning=FALSE------------------------------------------------------------
acs %>% select(sex,TC,TG,HDLC) %>% meanCI(group=sex) %>% plot()

## ----warning=FALSE------------------------------------------------------------
acs %>% select(sex,TC,TG,HDLC) %>% meanCI(sex,mu=0) %>% plot()

