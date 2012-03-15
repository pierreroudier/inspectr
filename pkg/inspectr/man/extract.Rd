\name{extraction-methods}
\docType{methods}

\alias{[}
\alias{[<-}
\alias{[[}
\alias{[[<-}
\alias{$}
\alias{$<-}
\alias{[<-,Spectra-method}
\alias{[,Spectra-method}
\alias{[<-,SpectraDataFrame-method}
\alias{[,SpectraDataFrame-method}
\alias{[[<-,Spectra-method}
\alias{[[,SpectraDataFrame-method}
\alias{$<-,Spectra-method}
\alias{$,SpectraDataFrame-method}

\title{Extracting and replacing parts of Spectra* objects}

\usage{
  \S4method{[}{Spectra}(x, i, j, ..., drop = FALSE)
  \S4method{[}{SpectraDataFrame}(x, i, j, ..., k, drop = FALSE)

  \S4method{[[}{SpectraDataFrame}(x, i, j, k, ..., drop = FALSE)
  \S4method{[[}{Spectra}(x, i, j, ...) <- value
  
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
  \item{i}{row index, to be used for selecting individuals in the collection}
  \item{j}{selected wavelengths}
  \item{k}{selected data columns}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{subset}}, \code{\link{aggregate_spectra}}}

\examples{
# Getting featrues information from SpectraDataFrame
#foo$bar
#foo[['bar']]

# Replacing values
#foo$bar <- runif(nrow(foo))
#foo[['bar']] <- runif(nrow(foo))

# Promote Spectra to SpectraDataFrame
#foo <- as(foo, 'Spectra')
#foo$bar <- runif(nrow(foo))
#summary(foo)

#foo[['bar']] <- runif(nrow(foo))
#foo[['bar']] <- runif(nrow(foo))
#summary(foo)
}