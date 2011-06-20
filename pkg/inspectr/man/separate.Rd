\name{separate}
\alias{separate}
\alias{unseparate}
\alias{separate.SpectraDataFrame}
\alias{unseparate.SpectraDataFrame}

\title{Separates a \code{SpectraDataFrame} object into a calibration and a validation set.}

\usage{
separate(obj, calibration, ...)
unseparate(obj, ...)
}

\description{Separates a \code{SpectraDataFrame} object into a calibration and a validation set.}

\details{The \code{unseparate} function provides a way back to the oriiginal object, but it still under development.}

\value{An list with two \code{SpectraDataFrame} objects, one for the calibration, and the other for the validation.}

\arguments{
  \item{obj}{an object inheriting from class \code{SpectraDataFrame}}
  \item{calibration}{The fraction of the dataset to be put in the calibration set}
  \item{...}{Ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}