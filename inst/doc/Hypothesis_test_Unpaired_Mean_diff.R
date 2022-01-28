## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)
library(interpretCI)
library(glue)

## ----echo=FALSE,message=FALSE-------------------------------------------------
x=meanCI(n1=100,n2=100,m1=200,s1=40,m2=190,s2=20,mu=7,alpha=0.05,alternative="greater")
two.sided<-greater<-less<-FALSE
if(x$result$alternative=="two.sided") two.sided=TRUE
if(x$result$alternative=="less") less=TRUE
if(x$result$alternative=="greater") greater=TRUE

twoS="The null hypothesis will be rejected if the difference between sample means is too big or if it is too small."
lessS="The null hypothesis will be rejected if the difference between sample means is too small."
greaterS="The null hypothesis will be rejected if the difference between sample means is too big."

## ----echo=FALSE---------------------------------------------------------------
call=paste0(deparse(x$call),collapse="")
x1=paste0("library(interpretCI)\nx=",call,"\ninterpret(x)")
textBox(x1,italic=TRUE,bg="grey95",lcolor="grey50")

## ----echo=FALSE---------------------------------------------------------------
library(glue)
library(interpretCI)
if(two.sided){
if(x$result$mu==0) {
     claim= "Test the hypothesis that men and women spend equally on refreshment"
} else {
     claim= paste0("The team owner claims that the difference in spending money between men and women is equal to ", x$result$mu) 
}
} else{
    if(x$result$mu==0) {
     claim= paste0("Test the hypothesis that men spend", ifelse(greater,"more","less")," than women on refreshment")
   } else {
       claim= paste0("The team owner claims that men spend at least $", x$result$mu, 
                   ifelse(greater," more"," less")," than women") 
   }
}

## ----echo=FALSE---------------------------------------------------------------
string=glue("The local baseball team conducts a study to find the amount spent on refreshments at the ball park. Over the course of the season they gather simple random samples of {x$result$n1} men and {x$result$n2} women. For men, the average expenditure was ${x$result$m1}, with a standard deviation of ${x$result$s1}. For women, it was ${x$result$m2}, with a standard deviation of ${x$result$s2}.

{claim}. Assume that the two populations are independent and normally distributed.")

textBox(string)

## -----------------------------------------------------------------------------
plot(x)

## -----------------------------------------------------------------------------
draw_t(DF=round(x$result$DF,2),t=x$result$t,alternative=x$result$alternative)

## ----echo=FALSE---------------------------------------------------------------
print(x)

