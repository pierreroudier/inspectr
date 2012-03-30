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

\description{These methods emulates classic base methods '[', '[[' and '$' to extract or replace parts of Spectra* objects.}

\section{Methods}{
\describe{

\bold{x=Spectra}

\code{x[x, i, j, ..., drop = FALSE]}

\tabular{rll}{
\tab \code{x}  \tab A \code{Spectra} object \cr
\tab \code{i} \tab Row index of the selected individuals \cr 
\tab \code{j} \tab Selected wavelengths \cr 
\tab \code{...} \tab Ignored \cr 
\tab \code{drop} \tab Ignored \cr
}

\code{x[[i, j, ...]] <- value}
\code{x$name <- value}
}

\bold{x=SpectraDataFrame}  

\code{x[i, j, k, ..., drop = FALSE]}
\code{x$k}

\tabular{rll}{
\tab \code{x}  \tab A \code{Spectra} object \cr
\tab \code{i} \tab Row index of the selected individuals \cr 
\tab \code{j} \tab Selected wavelengths \cr 
\tab \code{k} \tab Selected columns in the @data slot\cr 
\tab \code{...} \tab Ignored \cr 
\tab \code{drop} \tab Ignored \cr
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