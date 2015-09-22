\name{coordinates}
\docType{methods}

\alias{coordinates<-}
\alias{coordinates<-,SpectraDataFrame-method}

\title{Sets spatial coordinates to create \code{SpatialSpectraDataFrame} objects}

\description{Sets spatial coordinates to create \code{SpatialSpectraDataFrame} objects. Adapted from the \code{"coordinates<-"} method in the \bold{sp} package.}


\usage{
coordinates(object) <- value
}

\arguments{
\item{object}{A \code{SpectraDataFrame} object}
\item{value}{Spatial coordinates: either a matrix, list, or data
frame with numeric data, or column names, column number or a
reference: a formula (in the form of e.g. \code{~x+y}), column numbers
(e.g. \code{c(1,2)}) or column names (e.g. \code{c("x","y")}) specifying
which columns in \code{object} are the spatial coordinates. If the coordinates
are part of object, giving the reference does not duplicate them, giving
their value does duplicate them in the resulting structure.}
}

\value{A \code{SpatialSpectraDataFrame} object}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}, from code from the \bold{sp} package.}

\seealso{\code{\link{spectra}}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

# The australia dataset does not have associated coordinates.
# To fake it is a spatial dataset, we will associate each spectra
# in the collection to a location in the meuse dataset provided
# by the sp package

# Loding the meuse.grid dataset from the sp package
library(sp)
data(meuse.grid)
coordinates(meuse.grid) = ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
gridded(meuse.grid) = TRUE

# We randomly generate sampling locations on this grid
coords <- spsample(meuse.grid, n=nrow(australia)*2, type="random")
class(coords)

# We associate these locations to the spectra in the collection
#

# First, we add the coordinates in the data slot data
features(australia, safe=FALSE) <- as.data.frame(coords)[1:nrow(australia), ]
names(australia)

# Then we promote the SpectraDataFrame object to a SpatialDataFrame object
coordinates(australia) <- ~x+y
class(australia)
str(australia)
}