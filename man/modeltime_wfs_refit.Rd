% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeltime_wfs_refit.R
\name{modeltime_wfs_refit}
\alias{modeltime_wfs_refit}
\title{Modeltime workflow sets refit}
\usage{
modeltime_wfs_refit(.wfs_results, .serie)
}
\arguments{
\item{.wfs_results}{tibble of combination of recipes and models fitted, generated with the modeltime_wfs_fit function}

\item{.serie}{a time series dataframe}
}
\value{
a tibble containing the re-trained models
}
\description{
This function applies the modeltime_refit() function from modeltime to the wffits object generated from the modeltime_wfs_fit function (or the filtered version after the modeltime_wfs_bestmodel is applied)
}
\details{
Each model is now re-trained using all the available data.
}
\examples{

library(modeltime)
library(dplyr)

data <- sknifedatar::data_avellaneda \%>\% 
  mutate(date=as.Date(date)) \%>\% 
  filter(date<'2012-06-01')

recipe_date <- recipes::recipe(value ~ ., data = data) \%>\% 
  recipes::step_date(date, features = c('dow','doy','week','month','year')) 

recipe_date_lag <- recipe_date \%>\% 
  recipes::step_lag(value, lag = 7) \%>\% 
  timetk::step_ts_impute(all_numeric(), period=365)

mars <- parsnip::mars(mode = 'regression') \%>\%
  parsnip::set_engine('earth')

wfsets <- workflowsets::workflow_set(
  preproc = list(
    R_date = recipe_date,
    R_date_lag = recipe_date_lag),
  models  = list(M_mars = mars),
  cross   = TRUE)

wffits <- sknifedatar::modeltime_wfs_fit(.wfsets = wfsets, 
                                         .split_prop = 0.8, 
                                         .serie = data)

sknifedatar::modeltime_wfs_refit(.wfs_results = wffits,
                                 .serie = data)
                               
}
