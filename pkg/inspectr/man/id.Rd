\name{id}
\alias{id}
\alias{id<-}

\title{Retrieves or sets the ids of a \code{Spectra*} object.}

\usage{
id(object, ...)
id(object) <- value
}

\description{Either retrieves the wavelengths from a \code{Spectra*} object, or creates a \code{Spectra*} object from a \code{"data.frame"} object by setting some of its columns as the wavelengths.}

\details{
When applied to a \code{Spectra*} object, this functions simply returns the wavelengths of the spectra it is storing.

If applied on a \code{"data.frame"} object, it is an helper function to create a \code{Spectra*} object. It then needs to be indicated the wavelengths at which the spectra values are measured. The assumption is that each row of the \code{"data.frame"} is a spectra, and the column names of the \code{"data.frame"} contain the wavelengths values.

If all the columns are used to create the \code{Spectra*} object, a \code{Spectra} object is created. If some attributes are left, they will be used to generate a \code{SpectraDataFrame} object.
}

\value{If applied on a \code{"data.frame"}, either a \code{Spectra} or a \code{SpectraDataFrame} object. If applied on a \code{Spectra*} object, a vector.}

\arguments{
  \item{object}{a \code{"data.frame"} or an object inheriting from class \code{Spectra}}
  \item{value}{the wavelengths of the \code{Spectra*} object to create}
  \item{...}{Ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{spectra}}, \code{\link{Spectra-class}}, \code{\link{SpectraDataFrame-class}}}
