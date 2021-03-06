% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_t_table.R
\name{show_z_table}
\alias{show_z_table}
\title{Show z-value table}
\usage{
show_z_table(p = 0.05, alternative = "two.sided")
}
\arguments{
\item{p}{Numeric probability}

\item{alternative}{Character One of c("two.sided","greater","less")}
}
\value{
An object of class "flextable"
}
\description{
Show z-value table
}
\examples{
show_z_table()
show_z_table(p=0.01)
}
