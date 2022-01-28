## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=4)
library(interpretCI)
library(glue)

## ----echo=FALSE,message=FALSE-------------------------------------------------
x=meanCI(mtcars,mpg)

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
textBox(x1,italic=TRUE,bg="grey95",lcolor="grey50",width=6)

## ----echo=FALSE---------------------------------------------------------------

string=glue("An inventor has developed a new, energy-efficient lawn mower engine. From his stock of  {x$result$n*100} engines, the inventor selects a simple random sample of {x$result$n} engines for testing. The engines run for an average of {round(x$result$m,2)} minutes on a single gallon of regular gasoline, with a standard deviation of {round(x$result$s,2)} minutes. What is the {(1-x$result$alpha)*100}% confidence interval for the average minutes? (Assume that run times for the population of engines are normally distributed.")

textBox(string)

## ----echo=FALSE---------------------------------------------------------------
if(!is.na(x$data[1,1])) {
     head(x$data,10)
}

## ----echo=FALSE---------------------------------------------------------------
if(x$result$alternative=="two.sided"){
    string=glue("$$p*=1-\\alpha/2=1-{x$result$alpha}/2={1- x$result$alpha/2}$$")
} else{
     string=glue("$$p*=1-\\alpha=1-{x$result$alpha}$$")
}

## ----echo=FALSE---------------------------------------------------------------
show_t_table(DF=x$result$DF,p=x$result$alpha,alternative=x$result$alternative)

## ----results='asis',echo=FALSE------------------------------------------------
if(x$result$alternative=="two.sided"){
  string=glue("$$qt(p,df)=qt({1- x$result$alpha/2},{x$result$DF})={round(x$result$critical,3)}$$")
} else {
    string=glue("$$qt(p,df)=qt({1- x$result$alpha},{x$result$DF})={round(x$result$critical,3)}$$") 
} 

## ----echo=FALSE---------------------------------------------------------------
draw_t(DF=x$result$DF,p=x$result$alpha,alternative=x$result$alternative)

## ----results='asis',echo=FALSE------------------------------------------------
if(two.sided) {
     string="The range of the confidence interval is defined by the sample statistic $\\pm$margin of error."
} else if(less){
     string="The range of the confidence interval is defined by the -$\\infty$(infinite) and the sample statistic + margin of error."
} else{
     string="The range of the confidence interval is defined by the sample statistic - margin of error and the $\\infty$(infinite)."
}

## ----echo=FALSE---------------------------------------------------------------
plot(x)

## ----echo=FALSE---------------------------------------------------------------
print(x)

