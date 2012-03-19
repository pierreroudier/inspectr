\name{features}
\docType{methods}

% \alias{features}
\alias{features-methods}
\alias{features,SpectraDataFrame-method}
\alias{features<-,Spectra-method}
\alias{features<-,SpectraDataFrame-method}

\title{Retrieves or sets the data slot of a SpectraDataFrame object.}

\usage{
  \S4method{features}{SpectraDataFrame}(object,...,include_id=FALSE)
  \S4method{"features<-"}{Spectra}(object,safe=TRUE,key=NULL,remove_id=TRUE) <- value
  \S4method{"features<-"}{SpectraDataFrame}(object,safe=TRUE,key=NULL,remove_id=TRUE,append=TRUE) <- value
}

\description{Either retrieves the attributes values from the data slot of a SpectraDataFrame object, or upgrades a Spectra object to a SpectraDataFrame object by initialising its data slot by a suitable \code{"data.frame"} object.}

\value{
If applied on a \code{"data.frame"}, either a \code{Spectra} or a \code{SpectraDataFrame} object. 

If applied on a \code{Spectra*} object, a data.frame.
}

\arguments{
  \item{object}{an object inheriting from class \code{Spectra}}
  \item{value}{a \code{"data.frame"} object}
  \item{safe}{if TRUE, data is being added to the object using a SQL join, otherwise it is assumed the order of the rows is consitent with the order of the rows in \code{object}}
  \item{key}{name of the column of the data.frame storing the ids for the SQL join.}
  \item{remove_id}{if TRUE, ids used for the SQL join are removed from the data slot after the join.}
  \item{include_id}{if TRUE, the ids of the spectra are added in a field of the data.frame}
  \item{append}{if TRUE, the data is appended to any existing data. if FALSE, the data provided is erasing any existing data.}
  \item{...}{ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{spectra}}, \code{\link{wl}}, \code{\link{SpectraDataFrame-class}}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

# Printing available data
features(australia)

# Promoting a Spectra to a SpectraDataFrame object
s <- as(australia, "Spectra")
# Generating dummy data
d <- data.frame(id = ids(australia)[,1], foo = runif(nrow(australia)), bar = sample(LETTERS[1:5], size = nrow(australia), replace = TRUE))
head(d)
# Affecting data to Spectra object
features(s, key = 'id') <- d
summary(s)

# Adding data to an existing SpectraDataFrame object
features(australia, key = 'id') <- d
features(australia)

# Replacing data of an existing SpectraDataFrame object
features(australia, key = 'id', append = FALSE) <- d
features(australia)
}
