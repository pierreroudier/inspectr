\name{ids}
\docType{methods}
\alias{ids}
\alias{ids<-}
\alias{ids,Spectra-method}
\alias{ids<-,Spectra-method}
\alias{ids<-,SpectraDataFrame-method}

\title{Retrieves or sets the ids of a \code{Spectra*} object.}

\description{Either retrieves the wavelengths from a \code{Spectra*} object, or creates a \code{Spectra*} object from a \code{"data.frame"} object by setting some of its columns as the wavelengths.}

\section{Methods}{
\describe{

\bold{x=Spectra}

\code{ids(obj, ..., as.vector = FALSE)}

\code{ids(obj) <- value}

\tabular{rll}{
\tab \code{obj}  \tab A \code{Spectra} object \cr
\tab \code{...} \tab Ignored \cr
\tab \code{as.vector} \tab Logical, returns the IDs as a character vector rather than a 1-column \code{data.frame}. \cr 
\tab \code{value} A vector giving new IDs for \code{obj}. These IDs must be unique. \tab \cr
}

\bold{x=SpectraDataFrame}  

\code{ids(obj, ..., as.vector = FALSE)}

\code{ids(obj) <- value}

\tabular{rll}{
\tab \code{obj}  \tab A \code{Spectra} object \cr
\tab \code{...} \tab Ignored \cr
\tab \code{as.vector} \tab Logical, returns the IDs as a character vector rather than a 1-column \code{data.frame}. \cr 
\tab \code{value}  \tab Either a formula or a vector of new IDs (see the Details section). \cr
}

}
}

\details{
The \code{"ids<-"} method for \code{SpectraDataFrame} objects allows to use a formula interface to use a column in its \code{data} slot as the object IDs (see the last example provided in the Examples section).
}

\value{The \code{ids} methods return a vector if \code{as.vector} is TRUE, a \code{data.frame} otherwise. The \code{"ids<-"} method return a \code{SpectraDataFrame} object (or a \code{Spectra} object if the column in the data slot that has been used to initiate the IDs was the only attribute).}


\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{spectra}}, \code{\link{Spectra-class}}, \code{\link{SpectraDataFrame-class}}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

# Retrieving ids
ids(australia)

# Setting ids using a vector of values
ids(australia) <- seq_len(nrow(australia))
ids(australia)

# Setting ids using an attribute
australia$new_id <- seq_len(nrow(australia)) + 1000
ids(australia) <- ~ new_id
ids(australia)
}