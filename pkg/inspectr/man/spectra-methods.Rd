\name{spectra}
\docType{methods}
\alias{spectra}
\alias{spectra<-}
\alias{spectra,Spectra-method}
\alias{spectra<-,data.frame-method}
\alias{spectra<-,Spectra-method}
\alias{spectra<-,SpectraDataFrame-method}

\title{Retrieves or sets the spectra of a \code{Spectra*} objects.}

\usage{
spectra(object, ...)
spectra(object, mode="rowwise") <- value
}

\description{Either retrieves the spectra matrix from a \code{Spectra*} object, or creates a \code{Spectra*} object from a \code{"data.frame"} object different interfaces detailed below.}

\details{
When applied to a \code{Spectra*} object, this functions simply returns the spectra it is storing.

If applied on a \code{"data.frame"} object, it is an helper function to create a \code{Spectra*} object. Two kind of interfaces are then available. \code{value} can be:
\describe{
  \item{a vector:}{Similarly to \code{\link{wl}}, the wavelengths of the spectra can be passed by a \code{"numeric"} vector. Alternatively, the names of the columns that contain the spectra information can be passed as a \code{"character"} vector.}

  \item{a formula:}{This interface is specific to inspectr. It follows a scheme where differents parts can be observed, the id column, the attributes columns, and the spectra columns, described by the wavelengths at which it has been measured:}
  
  \itemize{
      \item \bold{Placeholders:} 
      \itemize{
	\item{\code{...}:}{placeholder for all the columns of your data.frame object except those that have been already used in other parts of the formula. This can lead to errors. E.g. if \code{object} has data one every wavelength between 350 and 2500 nm, \code{spectra(object) <- id_field ~ ... ~ 500:2500} will stores the columns corresponding to the wavelengths 350-499 nm in its data slot!}
	\item{\code{id}:}{For the creation of a \code{SpectraDataFrame}, it is important to always specify an id field in the formula. If no id column is present, the \code{id} placeholder will create one for you.}

      }
    
    \item \code{spectra(object) <- ~ 350:2500} will build a \code{Spectra} object from the wavelengths between 350 and 2500, based on the column names.

    \item \code{spectra(object) <- ~ 350:2:2500} will build a \code{Spectra} object from the wavelengths in \code{seq(350, 2500, by = 2)}.

    \item \code{spectra(object) <- ~ 500:2350} will build a \code{Spectra} object from the wavelengths between 500 and 2350, even though other wavelengths are present (they will be dropped).

In the three later cases, the id field has been dropped (it will be automatically created). If you want to use a column of \code{"data.frame"} as an id filed, you can still use the first part of the formula:

    \item \code{spectra(object) <- id_field ~ 350:2500}
    \item \code{spectra(object) <- id_field ~ 350:5:2500}

Some data can also be added to the object, which will then be of \code{SpectraDataFrame} class:

    \item \code{spectra(object) <- id_field ~ property1 ~ 500:2300} will create a \code{SpectraDataFrame} with ids from the id_field column, data from the property1 column, and spectral information between 500 and 2300 nm. That means that data property2, and all spectral information from bands < 500 nm or > 2300 nm will be dropped.

You can also combine the placeholders:

    \item \code{spectra(object) <- id_field ~ ... ~ 350:2500} will create a \code{SpectraDataFrame} object with ids from the id_field column, all spectral bands between 350 and 2500 nm. The data slot is given all the remaining columns.
  }
}
}

\section{About rowwise and colwise formats}{
Generally, data used in spectroscopy can be represented into two formats:
\describe{
  \item{\code{rowwise}}{Each row represents a sample, each columns being the attributes that have been measured for this sample (reflectance in each wavelength, but also physical or chemical attributes)}
  \item{\code{colwise}}{Each row represents the reflectance for a given wavelength, each column being a sample. In this configuration, no attribute data can be added}
}
}

\value{If applied on a \code{"data.frame"}, either a \code{Spectra} or a \code{SpectraDataFrame} object. If applied on a \code{Spectra*} object, a vector.}

\arguments{
  \item{object}{a \code{"data.frame"} or an object inheriting from class \code{Spectra}}
  \item{value}{the wavelengths of the \code{Spectra*} object to create}
  \item{mode}{a character describing the data representation of \code{object} ; it can be either \code{'rowwise'} (default value) or \code{'colwise'}.}
  \item{...}{Ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{wl}}, \code{\link{Spectra-class}}, \code{\link{SpectraDataFrame-class}}}

\examples{
# Loading example data
data(australia)
class(australia) # this is a simple data.frame
# structure of the data.frame: it is rowwise-formatted
big.head(australia) 

## CREATING Spectra OBJECTS
##

# Using spectra() to initiate a Spectra from 
# the data.frame
spectra(australia) <- sr_no ~ 350:2500
summary(australia)

# It is possible to select wavelengths using the formula interface
data(australia)
spectra(australia) <- sr_no ~ 350:5:2500
summary(australia)

data(australia)
spectra(australia) <- sr_no ~ 500:1800
summary(australia)

## CREATING SpectraDataFrame OBJECTS
##

# Using spectra() to initiate a SpectraDataFrame from 
# the data.frame
data(australia)
spectra(australia) <- sr_no ~ carbon + ph + clay ~ 350:2500
summary(australia)

# Selecting data to be included in the SpectradataFrame object
data(australia)
spectra(australia) <- sr_no ~ carbon ~ 350:2500
summary(australia)

# Forcing the creation of new ids using the id keyword in the 
# formula interface
data(australia)
spectra(australia) <- id ~ carbon ~ 350:2500
summary(australia)
ids(australia, as.vector = TRUE)

# Using the "..." short-hand to select all the remaining columns
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500
summary(australia)

## CREATING Spectra OBJECTS FROM
## BY-COLS-FORMATTED DATA
##

# For data formatted in the colwise format, 
# use the "colwise" mode

# Transforming data into colwise format
# for demonstration's sake
#
m <- melt_spectra(australia)
australia_by_col <- reshape2::acast(m, ... ~ sr_no)
australia_by_col <- data.frame(wl = rownames(australia_by_col), australia_by_col, check.names = FALSE)

# Here's colwise-formatted data 
big.head(australia_by_col)

# Convert it into Spectra object
spectra(australia_by_col, mode = "colwise") <- wl ~ ...
summary(australia_by_col)

# Then data can be added to promote it as a SpectraDataFrame
features(australia_by_col, key = 'sr_no') <- features(australia, include_id = TRUE)
summary(australia_by_col)
}