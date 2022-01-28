## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)
library(interpretCI)
library(glue)

## ----echo=FALSE,message=FALSE-------------------------------------------------
x=meanCI(n1=500,n2=1000,m1=20,s1=3,m2=15,s2=2,alpha=0.01,alternative="greater")

two.sided<-greater<-less<-FALSE
if(x$result$alternative=="two.sided") two.sided=TRUE
if(x$result$alternative=="less") less=TRUE
if(x$result$alternative=="greater") greater=TRUE

## ----echo=FALSE---------------------------------------------------------------
call=paste0(deparse(x$call),collapse="")
x1=paste0("library(interpretCI)\nx=",call,"\ninterpret(x)")
textBox(x1,italic=TRUE,bg="grey95",lcolor="grey50")

## ----echo=FALSE---------------------------------------------------------------
string=glue("The local baseball team conducts a study to find the amount spent on refreshments at the ball park. Over the course of the season they gather simple random samples of {x$result$n1} men and {x$result$n2} women. For men, the average expenditure was $ {x$result$m1}, with a standard deviation of $ {x$result$s1}. For women, it was ${x$result$m2}, with a standard deviation of ${x$result$s2}.

What is the {(1-x$result$alpha)*100}% confidence interval for the spending difference between men and women? Assume that the two populations are independent and normally distributed.")

textBox(string)

## ----echo=FALSE---------------------------------------------------------------
if(!is.na(x$data[[1]][1])) {
     x$data
}

## ----echo=FALSE---------------------------------------------------------------
show_t_table(DF=round(x$result$DF,2),p=x$result$alpha,alternative=x$result$alternative)

## ----echo=FALSE---------------------------------------------------------------
draw_t(DF=round(x$result$DF,2),p=x$result$alpha,alternative=x$result$alternative)

## ----results='asis',echo=FALSE------------------------------------------------
if(two.sided) {
     string="The range of the confidence interval is defined by the sample statistic $\\pm$margin of error."
} else if(less){
     string="The range of the confidence interval is defined by the -$\\infty$(infinite) and the sample statistic + margin of error."
} else{
     string="The range of the confidence interval is defined by the sample statistic - margin of error and the $\\infty$(infinite)."
}

## -----------------------------------------------------------------------------
plot(x)

## ----echo=FALSE---------------------------------------------------------------
print(x)

