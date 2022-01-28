## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)
library(interpretCI)
library(glue)

## ----echo=FALSE,message=FALSE-------------------------------------------------
x<-propCI(n=1600,p=0.4,alpha=0.01)
  
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

string=glue("A major metropolitan newspaper selected a simple random sample of {x$result$n} readers from their list of {x$result$n*100} subscribers. They asked whether the paper should increase its coverage of local news. {round(x$result$p*100,1)} % of the sample wanted more local news. What is the {(1-x$result$alpha)*100}% confidence interval for the proportion of readers who would like more coverage of local news?")

textBox(string)

## ----echo=FALSE---------------------------------------------------------------
if(x$result$alternative=="two.sided"){
    string=glue("$$p*=1-\\alpha/2=1-{x$result$alpha}/2={1- x$result$alpha/2}$$")
} else{
     string=glue("$$p*=1-\\alpha=1-{x$result$alpha}$$")
}

## ----results='asis',echo=FALSE------------------------------------------------
if(x$result$alternative=="two.sided"){
  string=glue("$$qnorm(p)=qnorm({1- x$result$alpha/2})={round(x$result$critical,3)}$$")
} else {
    string=glue("$$qnorm(p)=qnorm({1- x$result$alpha})={round(x$result$critical,3)}$$") 
} 

## ----echo=FALSE---------------------------------------------------------------
show_z_table(p=x$result$alpha,alternative=x$result$alternative)

## ----echo=FALSE---------------------------------------------------------------
draw_n(p=x$result$alpha,alternative=x$result$alternative)

## ----results='asis',echo=FALSE------------------------------------------------
if(two.sided) {
     string="The range of the confidence interval is defined by the sample statistic $\\pm$margin of error."
} else if(less){
     string="The range of the confidence interval is defined by the -$\\infty$(infinite) and the sample statistic + margin of error."
} else{
     string="The range of the confidence interval is defined by the sample statistic - margin of error and the $\\infty$(infinite)."
}

## ----echo=FALSE---------------------------------------------------------------
print(x)

