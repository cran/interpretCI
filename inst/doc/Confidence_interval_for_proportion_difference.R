## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)
library(interpretCI)
library(glue)

## ----echo=FALSE,message=FALSE-------------------------------------------------
x<-propCI(n1=400,n2=300,p1=0.4,p2=0.3,alpha=0.1)
  
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

string=glue("Suppose the Cartoon Network conducts a nation-wide survey to assess viewer attitudes toward Superman. Using a simple random sample, they select {x$result$n1} boys and {x$result$n2} girls to participate in the study. {English(x$result$p1*100)} percent of the boys say that Superman is their favorite character, compared to {english2(x$result$p2*100)} percent of the girls. What is the {(1-x$result$alpha)*100}% confidence interval for the true difference in attitudes toward Superman?")

textBox(string)

## ----echo=FALSE---------------------------------------------------------------
if(x$result$alternative=="two.sided"){
    string=glue("$$p*=1-\\alpha/2=1-{x$result$alpha}/2={1- x$result$alpha/2}$$")
} else{
     string=glue("$$p*=1-\\alpha=1-{x$result$alpha}$$")
}

## ----results='asis',echo=FALSE------------------------------------------------
if(two.sided){
  string=glue("$$qnorm(p)=qnorm({1- x$result$alpha/2})={round(x$result$critical,3)}$$")
} else {
    string=glue("$$qnorm(p)=qt({1- x$result$alpha})={round(x$result$critical,3)}$$") 
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
if(x$result$lower>0) {
  string="Since both ends of the confidence interval are positive, we can conclude that more boys than girls choose Superman as their favorite cartoon character."
} else if(x$result$upper<0) {
  string="Since both ends of the confidence interval are negative, we can conclude that less boys than girls choose Superman as their favorite cartoon character."
} else{
  string=""
}

## ----echo=FALSE---------------------------------------------------------------
print(x)

