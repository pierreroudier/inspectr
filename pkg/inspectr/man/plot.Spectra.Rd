\name{plot.Spectra}

\alias{plot.Spectra}
\alias{plot.SpectraDataFrame}

\title{Plots an object inheriting  from the Spectra class}

\usage{\method{plot}{Spectra} (x, gg=FALSE, ...)}

\description{Plots an object inheriting  from the Spectra class}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\arguments{
  \item{x}{an object of class \code{"Spectra"} or inheriting from this class}
  \item{gg}{if TRUE, uses the \code{ggplot2} package to plot the data, if FALSE uses \code{lattice} (experimental)}
  \item{...}{options to be passed to xyplot}
}
