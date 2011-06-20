\name{data}
\alias{data}
\alias{data<-}

\title{Retrieves or sets the data slot of a SpectraDataFrame object.}

\usage{
data(object, ...)
data(object) <- value
}

\description{Either retrieves the attributes values from the data slot of a SpectraDataFrame object, or upgrades a Spectra object to a SpectraDataFrame object by initialising its data slot by a suitable \code{"data.frame"} object.}

\value{If applied on a \code{"data.frame"}, either a \code{Spectra} or a \code{SpectraDataFrame} object. If applied on a \code{Spectra*} object, a vector.}

\arguments{
  \item{object}{an object inheriting from class \code{Spectra}}
  \item{value}{a \code{"data.frame"} object}
  \item{...}{Ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{spectra}}, \code{\link{wl}}, \code{\link{SpectraDataFrame-class}}}
