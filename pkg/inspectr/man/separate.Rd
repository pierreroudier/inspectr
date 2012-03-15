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
#res <- separate(foo, calibration=0.7)
#str(res)
#lapply(res, nrow)
}
