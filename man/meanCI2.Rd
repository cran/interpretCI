% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meanCI.R
\name{meanCI.default}
\alias{meanCI.default}
\alias{meanCI2}
\title{Calculate confidence intervals of mean or difference between means}
\usage{
\method{meanCI}{default}(x, ...)

meanCI2(
  x,
  y,
  n,
  m,
  s,
  n1,
  n2,
  m1,
  m2,
  s1,
  s2,
  mu = 0,
  paired = FALSE,
  var.equal = FALSE,
  alpha = 0.05,
  digits = 2,
  alternative = "two.sided"
)
}
\arguments{
\item{x}{A vector}

\item{...}{Further arguments to be passed to meanCI2}

\item{y}{A vector}

\item{n, n1, n2}{integer sample(s) size}

\item{m, m1, m2}{Numeric mean value of sample(s)}

\item{s, s1, s2}{Numeric standard deviation of sample(s)}

\item{mu}{numeric hypothesized true value of mean or mean difference}

\item{paired}{logical If true, difference between paired sample calculated}

\item{var.equal}{logical If true, pooled standard deviation is used}

\item{alpha}{Numeric Confidence level}

\item{digits}{integer indicating the number of decimal places}

\item{alternative}{A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".}
}
\value{
An object of class "meanCI" which is a list containing at least the following components:
\describe{
  \item{data}{A tibble containing raw data or a list of numeric vector}
  \item{result}{A data.frame containing of summary statistics}
  \item{call}{the matched call}
  \item{attr(*,"measure")}{character. One of c("mean","unpaired","paired")}
}
}
\description{
Calculate confidence intervals of mean or difference between means
}
\section{Methods (by generic)}{
\itemize{
\item \code{meanCI}: Default S3 method
}}

