\name{rbind}
\alias{rbind.Spectra}
\alias{rbind.SpectraDataFrame}

\title{Stacking \code{Spectra} objects together}

\usage{
  rbind.Spectra(..., create_new_ids = FALSE, new_ids = NULL)
  rbind.SpectraDataFrame(..., create_new_ids = FALSE, new_ids = NULL)
}
\arguments{
  \item{\dots}{The \code{Spectra} objects to be combined.}

  \item{create_new_ids}{allows creation of new ids if the ids of the \code{Spectra*} objects you are trying to stack are redondant}

  \item{new_ids}{vector of new ids to be given to the new object}
}
\value{
  a \code{Spectra*} object.
}

\description{
This method stacks two or more \code{Spectra*} objects together. 
}

\examples{
#bar <- rbind(foo, foo, create_new_ids = TRUE)
#summary(bar)
}
