\name{ids}
\docType{methods}
\alias{ids}
\alias{ids<-}
\alias{ids,Spectra-method}
\alias{ids<-,Spectra-method}
\alias{ids<-,SpectraDataFrame-method}

\title{Retrieves or sets the ids of a \code{Spectra*} object.}

\usage{
ids(object, ..., as.vector = FALSE)
ids(object) <- value
}

\description{Either retrieves the wavelengths from a \code{Spectra*} object, or creates a \code{Spectra*} object from a \code{"data.frame"} object by setting some of its columns as the wavelengths.}

\details{
When applied to a \code{Spectra*} object, this functions simply returns the wavelengths of the spectra it is storing.

If applied on a \code{"data.frame"} object, it is an helper function to create a \code{Spectra*} object. It then needs to be indicated the wavelengths at which the spectra values are measured. The assumption is that each row of the \code{"data.frame"} is a spectra, and the column names of the \code{"data.frame"} contain the wavelengths values.

If all the columns are used to create the \code{Spectra*} object, a \code{Spectra} object is created. If some attributes are left, they will be used to generate a \code{SpectraDataFrame} object.
}

\value{If \code{as.vector} is TRUE, returns a vector, otherwise, returns a \code{data.frame}.}

\arguments{
  \item{object}{a \code{"data.frame"} or an object inheriting from class \code{Spectra}}
  \item{value}{the wavelengths of the \code{Spectra*} object to create}
  \item{...}{Ignored}
  \item{as.vector}{if set to TRUE, returns a vector, otherwise, returns a \code{data.frame}}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{spectra}}, \code{\link{Spectra-class}}, \code{\link{SpectraDataFrame-class}}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

# Retrieving ids
ids(australia)

# Setting ids using a vector of values
ids(australia) <- seq_len(nrow(australia))
ids(australia)

# Setting ids using an attribute
australia$new_id <- seq_len(nrow(australia)) + 1000
ids(australia) <- ~ new_id
ids(australia)
}