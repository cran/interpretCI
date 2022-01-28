## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)
library(interpretCI)
library(glue)
library(flextable)
library(dplyr)

## ----echo=FALSE,message=FALSE-------------------------------------------------
#x<-params$result
data(Anorexia,package="PairedData")

x=meanCI(Anorexia,Post,Prior,paired=TRUE,alternative="greater",mu=4)

two.sided<-greater<-less<-FALSE
if(x$result$alternative=="two.sided") two.sided=TRUE
if(x$result$alternative=="less") less=TRUE
if(x$result$alternative=="greater") greater=TRUE
twoS="The null hypothesis will be rejected if the difference between sample means is too big or if it is too small."
lessS="The null hypothesis will be rejected if the difference between sample means is too small."
greaterS="The null hypothesis will be rejected if the difference between sample means is too big."

if(two.sided){
if(x$result$mu==0) {
     claim= "Test the hypothesis that there will be no difference in body weightmen after treatment"
} else {
     claim= paste0("Test the claims that the weigth gain after treatment is equal to ", x$result$mu) 
}
} else{
    if(x$result$mu==0) {
     claim= paste0("Test the hypothesis that the patients gain weights", ifelse(greater,"more","less")," after treatment.")
   } else {
       claim= paste0("Test the claims that the patients ",ifelse(x$result$mu>0,"gain","loose")," at least ", 
                   ifelse(greater," more"," less")," than ", english2(abs(x$result$mu)), " pounds in weights after treatment.") 
   }
}

## ----echo=FALSE---------------------------------------------------------------
call=paste0(deparse(x$call),collapse="")
x1=paste0("library(interpretCI)\nx=",call,"\ninterpret(x)")
textBox(x1,italic=TRUE,bg="grey95",lcolor="grey50")

## ----echo=FALSE---------------------------------------------------------------
cat("After treatment")
x$data[[1]]
cat("Before treatment")
x$data[[2]]

## -----------------------------------------------------------------------------
df=x$data[1:min(10,nrow(x$data)),]
names(df)[4]="(d-mean(d)^2"
flextable(df) %>% autofit()

## ----echo=FALSE---------------------------------------------------------------
if(two.sided){
    string=glue("$$p*=1-\\alpha/2=1-{x$result$alpha}/2={1- x$result$alpha/2}$$")
} else{
     string=glue("$$p*=1-\\alpha=1-{x$result$alpha}$$")
}

## -----------------------------------------------------------------------------
draw_t(DF=round(x$result$DF,2),t=x$result$t,alternative=x$result$alternative)

## -----------------------------------------------------------------------------
plot(x,ref="test",side=FALSE)

## -----------------------------------------------------------------------------
t.test(x$data[[1]],x$data[[2]],paired=TRUE,alternative=x$result$alternative,conf.level=1-x$result$alpha,mu=x$result$mu)

## ----echo=FALSE---------------------------------------------------------------
print(x)

