\name{features}
\docType{methods}

\alias{features}
\alias{features<-}
\alias{features-methods}
\alias{features,SpectraDataFrame-method}
\alias{features<-,Spectra-method}
\alias{features<-,SpectraDataFrame-method}

\title{Retrieves or sets the data slot of a SpectraDataFrame object.}

\description{Either retrieves the attributes values from the data slot of a SpectraDataFrame object, or upgrades a Spectra object to a SpectraDataFrame object by initialising its data slot by a suitable \code{"data.frame"} object.}

\section{Methods}{
\describe{

\bold{x=Spectra}

\code{features(obj, safe=TRUE, key=NULL, remove_id=TRUE) <- value}

\tabular{rll}{
\tab \code{obj}  \tab A \code{Spectra} object \cr
\tab \code{safe} \tab Logical. If TRUE, data is being added to the object using a SQL join (using a key field given by the \code{key} option), otherwise it is assumed the order of the rows is consitent with the order of the rows in \code{obj} \cr
\tab \code{key} \tab Character, name of the column of the data.frame storing the ids for the SQL join. Ignored if \code{safe} is \code{FALSE}. \cr 
 \tab \code{remove_id} \tab Logical, if \code{TRUE}, ids used for the SQL join are removed from the data slot after the join. \cr 
}

\bold{x=SpectraDataFrame}  

\code{features(obj, remove_id=TRUE)}

\code{features(obj, safe=TRUE, key=NULL, remove_id=TRUE, append=TRUE) <- value}

\tabular{rll}{
\tab \code{obj}  \tab A \code{SpectraDataFrame} object \cr
\tab \code{safe} \tab Logical. If TRUE, data is being added to the object using a SQL join (using a key field given by the \code{key} option), otherwise it is assumed the order of the rows is consitent with the order of the rows in \code{obj} \cr
\tab \code{key} \tab Character, name of the column of the data.frame storing the ids for the SQL join. Ignored if \code{safe} is \code{FALSE}. \cr 
 \tab \code{remove_id} \tab Logical. For the \code{features} method, if \code{TRUE}, the spectra ids are added to the \code{data.frame} that is returned. For the \code{"features<-"} method, If \code{TRUE}, ids used for the SQL join are removed from the data slot after the join. \cr 
\tab \code{append} \tab Logical, if \code{TRUE}, the data is appended to any existing data. if FALSE, the data provided is erasing any existing data. \cr
}

}
}

\value{
The \code{features} methods return a \code{data.frame} object, while the \code{"features<-"} methods return a \code{SpectraDataFrame} object. 
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
