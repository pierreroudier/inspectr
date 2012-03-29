\name{SpatialSpectraDataFrame}

\alias{SpatialSpectraDataFrame}

\title{Constructor for the SpatialSpectraDataFrame class.}

\usage{
SpatialSpectraDataFrame(coords, nir, data, coords.nrs = numeric(0), proj4string = CRS(as.character(NA)), match.ID = TRUE, bbox=NULL, wl=numeric(), id=data.frame(id=NA), units="nm")
}

\description{Constructor for the SpatialSpectraDataFrame class. Creates a SpatialSpectraDataFrame object from scratch.}

\value{a new \code{"SpatialSpectraDataFrame"} object}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\arguments{
  \item{wl}{a numeric vector giving the wavelengths at with the spectra have been measured}
  \item{nir}{a \code{"matrix"} or a \code{"data.frame"} object giving the spectra values for each sample}
  \item{id}{a vector giving the unique id of each sample in the collection}
  \item{units}{a character giving the unit in which the wavelengths values are expressed}
  \item{data}{object of class \code{"data.frame"} containing the attribute data}
  \item{coords}{numeric matrix or data.frame with coordinates (each row is a point)}
  \item{proj4string}{ projection string of class \link{CRS-class}}
  \item{bbox}{bounding box matrix, usually NULL and constructed from the data, but may be passed through for coercion purposes if clearly needed}
  \item{coords.nrs}{numeric; if present, records the column positions where
in \code{data} the coordinates were taken from (used by \link{coordinates<-})}
  \item{match.ID}{logical; if TRUE AND coords has rownames (i.e., coerced
to a matrix, \code{dimnames(coords)[[2]]} is not NULL), AND data has
row.names (i.e. is a data.frame), then the \code{SpatialSpectraDataFrame}
object is formed by matching the row names of both components, leaving
the order of the coordinates in tact. Checks are done to see whether
both row names are sufficiently unique, and all data are matched.
If FALSE, coordinates and data are simply "glued" together. If
character: indicates the column in \code{data} with coordinates IDs
to match}
}

\seealso{\code{\link{spectra}}, \code{\link{wl}}, \code{\link{SpatialSpectraDataFrame-class}}}

\examples{
  # Creating a SpatialSpectraDataFrame object from scratch
  my.wl <- 350:2500
  my.id <- LETTERS[1:4]
  my.nir <- matrix(runif(4*length(my.wl)), nrow=4)
  my.data <- data.frame(foo = runif(4), bar = LETTERS[1:4])
  my.coords <- matrix(runif(4), runif(4), ncol=2, nrow=4)

  my.ssdf <- SpatialSpectraDataFrame(wl = my.wl, nir = my.nir, id = my.id, data = my.data, coords = my.coords)
}