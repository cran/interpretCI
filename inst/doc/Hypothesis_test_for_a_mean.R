## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)
library(interpretCI)
library(glue)

## ----echo=FALSE,message=FALSE-------------------------------------------------
x=meanCI(mtcars,mpg,mu=23)

two.sided<-greater<-less<-FALSE
if(x$result$alternative=="two.sided") two.sided=TRUE
if(x$result$alternative=="less") less=TRUE
if(x$result$alternative=="greater") greater=TRUE

twoS="The null hypothesis will be rejected if the sample mean is too big or if it is too small."
lessS="The null hypothesis will be rejected if the sample mean is too small."
greaterS="The null hypothesis will be rejected if the sample mean is too big."

## ----echo=FALSE---------------------------------------------------------------

call=paste0(deparse(x$call),collapse="")
x1=paste0("library(interpretCI)\nx=",call,"\ninterpret(x)")
textBox(x1,italic=TRUE,bg="grey95",lcolor="grey50")

## ----echo=FALSE---------------------------------------------------------------

string=glue("An inventor has developed a new, energy-efficient lawn mower engine. He claims that the engine will run continuously for {round(x$result$mu,2)} minutes on a single gallon of regular gasoline. From his stock of 2000 engines, the inventor selects a simple random sample of {x$result$n} engines for testing. The engines run for an average of {round(x$result$m,2)} minutes, with a standard deviation of {round(x$result$s,2)} minutes. Test the null hypothesis that the mean run time {ifelse(two.sided,'is',ifelse(less, 'greater than','less than'))} {x$result$mu} minutes against the alternative hypothesis that the mean run time {ifelse(two.sided,'is not',ifelse(less, 'less than','greater than'))} {round(x$result$mu,2)} minutes. Use a {x$result$alpha} level of significance. (Assume that run times for the population of engines are normally distributed.)")

textBox(string)

## -----------------------------------------------------------------------------
plot(x)

## -----------------------------------------------------------------------------
draw_t(DF=x$result$DF,t=x$result$t,alternative=x$result$alternative)

## ----echo=FALSE---------------------------------------------------------------
print(x)

