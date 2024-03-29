% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cox_analysis.R
\name{Mul_cox}
\alias{Mul_cox}
\title{Cox Analysis}
\usage{
Mul_cox(
  variables = NULL,
  data = NULL,
  p_cut = 0.25,
  censored = NULL,
  OS = "OS",
  OS_time = "OS_time",
  included_uni = T
)
}
\arguments{
\item{variables}{Variable for analysis}

\item{data}{Clinial Matrix}

\item{p_cut}{P value cutoff for univariate analysis}

\item{censored}{OS time cutoff; 60 is recommended}

\item{OS}{Column names representing survival status}

\item{OS_time}{Column names representing survival time}

\item{included_uni}{Results of univariate analysis were included or not}
}
\value{
Cox univariate and multivariate results
}
\description{
Cox Analysis
}
