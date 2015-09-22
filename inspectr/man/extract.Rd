\name{extraction-methods}
\docType{methods}

\alias{[}
\alias{[<-}
\alias{[[}
\alias{[[<-}
\alias{$}
\alias{$<-}

\alias{[,Spectra,ANY,ANY,missing-method}
\alias{[,SpectraDataFrame,ANY,ANY,missing-method}
\alias{[[,SpectraDataFrame,ANY,missing-method}
\alias{[[<-,Spectra,ANY,missing-method}
\alias{[<-,SpectraDataFrame-method}


\alias{$<-,Spectra-method}
\alias{$,SpectraDataFrame-method}

\title{Extracting and replacing parts of Spectra* objects}

\description{These methods emulates classic base methods '[', '[[' and '$' to extract or replace parts of Spectra* objects.}

\section{Methods}{
\describe{

\bold{x=Spectra}

\code{x[i, j, ..., drop = FALSE]}

\code{x$name <- value} 

\code{x[[name]] <- value} 

\tabular{rll}{
\tab \code{x}  \tab A \code{Spectra} object \cr
\tab \code{i} \tab Row index of the selected individuals \cr 
\tab \code{j} \tab Selected wavelengths \cr 
\tab \code{name} \tab A literal character string or a name \cr 
\tab \code{...} \tab Ignored \cr 
\tab \code{drop} \tab Ignored \cr
}

\bold{x=SpectraDataFrame}  

\code{x[i, j, k, ..., drop = FALSE]}

\code{x[[name]]}

\code{x[[name]] <- value} 

\code{x$name}

\code{x$name <- value} 

\tabular{rll}{
\tab \code{x}  \tab A \code{SpectraDataFrame} object \cr
\tab \code{i} \tab Row index of the selected individuals \cr 
\tab \code{j} \tab Selected wavelengths \cr 
\tab \code{k} \tab Selected columns in the @data slot\cr 
\tab \code{name} \tab A literal character string or a name \cr 
\tab \code{...} \tab Ignored \cr 
\tab \code{drop} \tab Ignored \cr
}
}
}

\value{
These methods either return an object of the same clss as \code{x}, or can promote a \code{Spectra} object to a \code{SpectraDataFrame} object by adding data ("[[<-" and "$<-" methods).
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