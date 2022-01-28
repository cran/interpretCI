## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment=NA,fig.width=7,fig.height=5)
library(interpretCI)
library(glue)
library(flextable)

## ----echo=FALSE,message=FALSE-------------------------------------------------
#x<-params$result
data(Anorexia,package="PairedData")
x=meanCI(Anorexia,Prior,Post,paired=TRUE)

two.sided<-greater<-less<-FALSE
if(x$result$alternative=="two.sided") two.sided=TRUE
if(x$result$alternative=="less") less=TRUE
if(x$result$alternative=="greater") greater=TRUE

## ----echo=FALSE---------------------------------------------------------------
call=paste0(deparse(x$call),collapse="")
x1=paste0("library(interpretCI)\nx=",call,"\ninterpret(x)")
textBox(x1,italic=TRUE,bg="grey95",lcolor="grey50")

## ----echo=FALSE---------------------------------------------------------------
cat("English")
x$data[[1]]
cat("math")
x$data[[2]]

## -----------------------------------------------------------------------------
df=x$data[1:10,]
names(df)[4]="(d-mean(d)^2"
flextable(df) %>% autofit()

## ----echo=FALSE---------------------------------------------------------------
if(x$result$alternative=="two.sided"){
    string=glue("$$p*=1-\\alpha/2=1-{x$result$alpha}/2={1- x$result$alpha/2}$$")
} else{
     string=glue("$$p*=1-\\alpha=1-{x$result$alpha}$$")
}

## ----echo=FALSE---------------------------------------------------------------
show_t_table(DF=x$result$DF,p=x$result$alpha,alternative=x$result$alternative)

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

## -----------------------------------------------------------------------------
plot(x,side=FALSE)

## ----echo=FALSE---------------------------------------------------------------
print(x)

