\name{Spectra-class}

\docType{class}

\alias{inspectr}
\alias{Spectra-class}
\alias{SpectraDataFrame-class}
\alias{as.data.frame.Spectra}
\alias{as.data.frame.SpectraDataFrame}
\alias{names.SpectraDataFrame}
\alias{names<-.SpectraDataFrame}

\alias{print, Spectra-method}
\alias{show, Spectra-method}

\title{Spectra* classes}

\description{
The inspectr package provides the user with two S4 classes that have been developped to store and manipulate spectroscopy data. 

The \code{Spectra} class is storing the spectra matrix, along with the wavelengths at which those have been measured, the units in whioch those wavelengths are expressed, and a unique id for each sample in the collection.

The \code{SpectraDataFrame} classe is extending the \code{Spectra} class by giving the opportunity to store attribute data along with the spectra - this is mostly the case when we want to predict physical or chemical properties from the spectra set.

Common generic methods implemented for these classes include: 
 
\code{summary}, \code{show}, \code{nrow}, \code{length}, \code{plot, ...}, \code{[}, \code{[[}, \code{$}. 
}


\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Spectra", ...)}, with the constructor functions like \code{Spectra(...)}, or with the helper functions such as \code{wl} and \code{spectra}.
}

\section{Slots}{
Slots for Spectra* objects
  \describe{
    \item{\code{wl}:}{object of class \code{"numeric"}; the wavelengths at which the spectra has been measured}
    \item{\code{nir}:}{object of class \code{"matrix"}; the spectra, with as many columns as wavelengths, and as many rows as samples}
    \item{\code{id}:}{object of class \code{"data.frame"} with one attribute; the identification strings for each sample in the collection}
    \item{\code{units}:}{object of class \code{"character"}; units in which the wavelengths are expressed}
    \item{\code{data}:}{object of class \code{"data.frame"} containing the attribute data}
  }
}

\author{Pierre Roudier \email{pierre.roudier@gmail.com}}

\examples{
showClass("Spectra")
showClass("SpectraDataFrame")
}
