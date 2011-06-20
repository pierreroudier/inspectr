\name{Spectra}
\alias{Spectra}

\title{Constructor for the Spectra class.}

\usage{Spectra(wl=numeric(), nir=matrix(), id=as.character(NA), units="nm")}

\description{Constructor for the Spectra class. Creates a Spectra object from scratch.}

\value{a new \code{"Spectra"} object}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\arguments{
  \item{wl}{a numeric vector giving the wavelengths at with the spectra have been measured}
  \item{nir}{a \code{"matrix"} or a \code{"data.frame"} object giving the spectra values for each sample}
  \item{id}{a vector giving the unique id of each sample in the collection}
  \item{units}{a character giving the unit in which the wavelengths values are expressed}
}

\seealso{\code{\link{spectra}}, \code{\link{wl}}, \code{\link{Spectra-class}}, \code{\link{SpectraDataFrame}}}

\examples{
  my.wl <- 350:2500
  my.id <- c("A", "B")
  my.nir <- matrix(runif(2*length(my.wl)), nrow=2)
  my.spec <- Spectra(wl = my.wl, nir = my.nir, id = my.id)
}