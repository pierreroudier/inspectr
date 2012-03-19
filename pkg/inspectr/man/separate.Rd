\name{separate}
\docType{methods}
\alias{separate}
\alias{separate.Spectra}
\alias{separate,Spectra-method}


\title{Separates a \code{Spectra*} object into a calibration and a validation set.}

\usage{
separate(obj, calibration, ...)
}

\description{Separates a \code{Spectra*} object into a calibration and a validation set.}

\value{An list with two \code{SpectraDataFrame} objects, one for the calibration, and the other for the validation.}

\arguments{
  \item{obj}{an object inheriting from class \code{SpectraDataFrame}}
  \item{calibration}{The fraction of the dataset to be put in the calibration set}
  \item{...}{Ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{split}}, \code{\link{melt_spectra}}, \code{\link{lapply}}, and the \code{l*pply} family of function inthe plyr package.}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

l <- separate(australia, calibration=0.7)
# The result is a list of two Spectra* objects
str(l)
lapply(l, nrow)

summary(l$calibration)
summary(l$validation)
}
