## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)
library(interpretCI)
library(glue)

## ----echo=FALSE,message=FALSE-------------------------------------------------
x=propCI(n=100,p=0.73,P=0.8,alpha=0.01)

two.sided<-greater<-less<-FALSE
if(x$result$alternative=="two.sided") two.sided=TRUE
if(x$result$alternative=="less") less=TRUE
if(x$result$alternative=="greater") greater=TRUE

twoS="The null hypothesis will be rejected if the sample proportion is too big or if it is too small."
lessS="The null hypothesis will be rejected if the sample proportion is too small."
greaterS="The null hypothesis will be rejected if the sample proportion is too big."

## ----echo=FALSE---------------------------------------------------------------
call=paste0(deparse(x$call),collapse="")
x1=paste0("library(interpretCI)\nx=",call,"\ninterpret(x)")
textBox(x1,italic=TRUE,bg="grey95",lcolor="grey50")

## ----echo=FALSE---------------------------------------------------------------

string=glue("The CEO of a large electric utility claims that {(1-x$result$P)*100} percent of his {x$result$n*100} customers are very satisfied with the service they receive. To test this claim, the local newspaper surveyed {x$result$n} customers, using simple random sampling. Among the sampled customers, {(1-x$result$p)*100} percent say they are very satisified. Based on these findings, can we reject the CEO's hypothesis that {(1-x$result$P)*100}% of the customers are very satisfied? Use a {x$result$alpha} level of significance.")

textBox(string)

## ----echo=FALSE---------------------------------------------------------------

if(two.sided){
               string=glue("pnorm(-abs({x$result$z}))\\times2")
} else if(greater){
               string=glue("pnorm({x$result$z},lower.tail=FALSE)")
} else{
               string=glue("pnorm({x$result$z})")
          }

## -----------------------------------------------------------------------------
draw_n(z=x$result$z,alternative=x$result$alternative)

## ----echo=FALSE---------------------------------------------------------------
print(x)

