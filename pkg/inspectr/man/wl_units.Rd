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
res(x, ...)
}

\description{Retrieves the wavelengths units and the spectral resolution from \code{Spectra*} objects.}

\value{A vector.}

\arguments{
  \item{object, x}{an object inheriting from class \code{Spectra}, or a \code{"numeric"} vector for \code{resolution}}
  \item{value}{a character string}
  \item{...}{ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\examples{
#wl(foo)
#range(wl(foo))
#res(foo)

#wl(foo) <- wl(foo) + 1000
#wl(foo)
}