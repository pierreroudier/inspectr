\name{subset}
\docType{methods}
\alias{subset}
\alias{subset.SpectraDataFrame}
\alias{subset,SpectraDataFrame-method}

\title{Subset SpectraDataFrame object}

\description{
  Returns subsets of a SpectraDataFrame object.  
}

\usage{subset(x, ...)}

\arguments{
  \item{x}{SpectraDataFrame object}
  \item{...}{Additional arguments:
    \itemize{
      \item{subset}{logical expression indicating elements or rows to keep: missing values are taken as false.} 
      \item{select}{expression, indicating columns to select from the data slot.} 
      \item{drop}{passed on to "[" indexing operator.}
    }
  }
}

\value{
SpectraDataFrame object
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{mutate}}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

# Subset on attributes
s <- subset(australia, carbon < 5)
summary(s)

# Subset and selection of attributes
s <- subset(australia, carbon < 5, select = 1)
summary(s)

}