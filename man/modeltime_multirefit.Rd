% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeltime_multirefit.R
\name{modeltime_multirefit}
\alias{modeltime_multirefit}
\title{Function to readjust the model or models for multiple time series}
\usage{
modeltime_multirefit(models_table)
}
\arguments{
\item{models_table}{"\strong{table_time}" tibble generated from the "\strong{modeltime_multifit}" function.}
}
\value{
Retrained "\strong{table_time}" object
}
\description{
This functions applies the "\strong{modeltime_refit()}" function from the  \href{https://business-science.github.io/modeltime/}{\strong{modeltime}}
package to multiple series and models.
}
\details{
It takes the "\strong{table_time}" tibble generated with the  "\strong{modeltime_multifit}" function and returns the same object but with
the models fitted for the complete period.
}
\examples{

# Data
library(modeltime)
data_serie <- sknifedatar::table_time
table_time <- data_serie$table_time

# modeltime_multirefit
sknifedatar::modeltime_multirefit(models_table = table_time)

}
