\name{wl_units}
\docType{methods}
\title{Wavelengths of Spectra* objects}

\alias{wl_units}
\alias{wl_units<-}
\alias{res}
\alias{res.numeric}
\alias{res.integer}
\alias{res.Spectra}
\alias{wl_units,Spectra-method}
\alias{wl_units<-,Spectra-method}
\alias{res,numeric-method}
\alias{res,integer-method}
\alias{res,Spectra-method}

\usage{
wl_units(object)
wl_units(object) <- value
res(x)
}

\description{Retrieves the wavelengths units and the spectral resolution from \code{Spectra*} objects.}

\value{A vector.}

\arguments{
  \item{object, x}{an object inheriting from class \code{Spectra}, or a \code{"numeric"} vector for \code{resolution}}
  \item{value}{a character string}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

# Print wavelength information
wl(australia)
range(wl(australia))
res(australia)

# Manipulate wavelength information
wl(australia) <- wl(australia) + 1000
wl(australia)
}