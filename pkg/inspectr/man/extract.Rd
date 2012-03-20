\name{extraction-methods}
\docType{methods}

\alias{[}
\alias{[<-}
\alias{[[}
\alias{[[<-}
\alias{$}
\alias{$<-}

\alias{'[',Spectra,ANY,ANY,missing-method}
\alias{'[',SpectraDataFrame,ANY,ANY,missing-method}
\alias{'[[',SpectraDataFrame,ANY,missing-method}
\alias{'[[<-',Spectra,ANY,missing-method}
\alias{[<-,Spectra-method}
\alias{[<-,SpectraDataFrame-method}


\alias{$<-,Spectra-method}
\alias{$,SpectraDataFrame-method}

\title{Extracting and replacing parts of Spectra* objects}

\usage{
  \S4method{[}{Spectra,ANY,ANY,missing}(x, i, j, ..., drop = FALSE)
  \S4method{[}{SpectraDataFrame,ANY,ANY,missing}(x, i, j, ..., k, drop = FALSE)

  \S4method{[[}{SpectraDataFrame,ANY,missing}(x, i, j, k, ..., drop = FALSE)
  \S4method{[[}{Spectra,ANY,missing}(x, i, j, ...) <- value
  
  \S4method{$}{SpectraDataFrame}(x, name)
  \S4method{$}{Spectra}(x, name) <- value  
}

\description{These methods emulates classic base methods '[', '[[' and '$' to extract or replace parts of Spectra* objects.}

\details{
...
}

\value{An object inheriting from \code{Spectra}.}

\arguments{
  \item{x}{an object inheriting from class \code{Spectra}}
  \item{value}{a vector of values to be added to the object data slot}
  \item{name}{name of the attribute to create or to query}
  \item{i}{row index, to be used for selecting individuals in the collection}
  \item{j}{selected wavelengths}
  \item{k}{selected data columns}
  \item{drop}{ignored}
  \item{...}{ignored}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{subset}}, \code{\link{aggregate_spectra}}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

# Getting features information from SpectraDataFrame
australia$carbon
australia[['carbon']]

# Creating new features
australia$foo <- runif(nrow(australia))
australia[['bar']] <- runif(nrow(australia))
summary(australia)

# Replacing values
australia$foo <- sample(LETTERS[1:5], size = nrow(australia), replace = TRUE)
australia[['bar']] <- sample(c(TRUE, FALSE), size = nrow(australia), replace = TRUE)
summary(australia)

# Promote Spectra to SpectraDataFrame
s <- as(australia, 'Spectra')
class(s)
s$foo <- runif(nrow(s))
summary(s)
}