\name{units}

\alias{units}
\alias{units<-}
\alias{resolution}
\alias{resolution.numeric}
\alias{resolution.Spectra}

\title{Utility functions}

\usage{
units(object)
units(object) <- value
resolution(object, ...)
}

\description{Retrieves the wavelengths units and the spectral resolution from \code{Spectra*} objects.}

\value{A vector.}

\arguments{
  \item{object}{an object inheriting from class \code{Spectra}, or a \code{"numeric"} vector for \code{resolution}}
  \item{value}{a character string}
  \item{...}{ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}