## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)
library(interpretCI)
library(glue)

## ----echo=FALSE,message=FALSE-------------------------------------------------
x=propCI(n1=150,n2=100,p1=0.71,p2=0.63,P=0,alternative="greater")
  
two.sided<-greater<-less<-FALSE
if(x$result$alternative=="two.sided") two.sided=TRUE
if(x$result$alternative=="less") less=TRUE
if(x$result$alternative=="greater") greater=TRUE

twoS="The null hypothesis will be rejected if the proportion from population 1 is too big or if it is too small."
lessS="The null hypothesis will be rejected if the proportion from population 1 is too small."
greaterS="The null hypothesis will be rejected if the proportion from population 1 is too big."

## ----echo=FALSE---------------------------------------------------------------
call=paste0(deparse(x$call),collapse="")
x1=paste0("library(interpretCI)\nx=",call,"\ninterpret(x)")
textBox(x1,italic=TRUE,bg="grey95",lcolor="grey50")

## ----echo=FALSE---------------------------------------------------------------

string=glue("Suppose the Acme Drug Company develops a new drug, designed to prevent colds. The company states that the drug is equally effective for men and women. To test this claim, they choose a a simple random sample of {x$result$n1} women and {x$result$n2} men from a population of {(x$result$n1+x$result$n2)*50} volunteers.

At the end of the study, {x$result$p1*100}% of the women caught a cold; and {x$result$p2*100}% of the men caught a cold. Based on these findings, can we reject the company's claim that the drug is {ifelse(two.sided,'equally',ifelse(less,'more','less'))} effective for men {ifelse(two.sided,'and','compared to')} women? Use a {x$result$alpha} level of significance.")

textBox(string)

## ----echo=FALSE---------------------------------------------------------------

if(two.sided){
               string=glue("pnorm(-abs({round(x$result$z,2)}))\\times2")
} else if(greater){
               string=glue("pnorm({round(x$result$z,2)},lower.tail=FALSE)")
} else{
               string=glue("pnorm({round(x$result$z,2)})")
          }

## -----------------------------------------------------------------------------
draw_n(z=x$result$z,alternative=x$result$alternative)

## ----echo=FALSE---------------------------------------------------------------
print(x)

