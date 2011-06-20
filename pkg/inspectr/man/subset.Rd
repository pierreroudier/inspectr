\name{subset}

\alias{subset}
\alias{subset.SpectraDataFrame}

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