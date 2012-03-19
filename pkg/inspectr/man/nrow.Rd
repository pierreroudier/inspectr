\name{nrow}
\docType{methods}
\title{Retrieve dimensions of Spectra* objects}

\alias{nrow,Spectra-method}
\alias{ncol,Spectra-method}
\alias{dim}
\alias{length}
\alias{dim.Spectra}
\alias{length.Spectra}

\usage{
dim(x)
length(x)
\S4method{ncol}{Spectra}(x)
\S4method{nrow}{Spectra}(x)
}

\arguments{
\item{x}{For \code{nrow}, \code{length}, \code{dim}, a \code{Spectra} object. For \code{ncol}, a \code{SpectraDataFrame} object.}
}

\description{Retrieves the wavelengths units and the spectral resolution from \code{Spectra*} objects.}

\details{
* Methods for \code{Spectra} objects

\code{nrow} returns the number of individuals in the collection
\code{length} returns the number of wavelengths in the collection
\code{ncol} returns NULL
\code{dim} returns a vector containing (1) the number of individuals and (2) in the number of wavelengths in the collection

* Methods for \code{Spectra} objects

\code{nrow} returns the number of individuals in the collection
\code{length} returns the number of wavelengths in the collection
\code{ncol} returns the number of attributes in the collection
\code{dim} returns a vector containing (1) the number of individuals, (2) in the number of wavelengths, and (3) the number of attributes in the collection

}

\value{
\code{nrow}, \code{ncol}, \code{nwl}, and \code{length}, return an \code{integer}.

\code{dim} returns a vector of length 2 or 3 (see Details section).
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

nrow(australia)
ncol(australia)
length(australia)
dim(australia)
}