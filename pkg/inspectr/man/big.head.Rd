\name{big.head}
\alias{big.head}
\alias{big.tail}

\title{Return the First or Last Part of an Object}

\usage{big.head(x, n=5, s=5, f=5)}

\description{Return the First or Last Part of an Object}

\details{Returns the first or last rows of a data frame like head() and tail(),
but also only returns the first and last columns. This has been
implemented to check big data frames.}

\value{An object (usually) like 'x' but generally smaller.}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}
\alias{big.head}
\alias{big.tail}

\arguments{
  \item{x}{a \code{"data.frame"} or a \code{"matrix"} object}
  \item{n}{a single, positive integer, number of rows for the object to return}
  \item{s}{a single, positive integer}
  \item{f}{a single, positive integer}
}

\examples{big.head(mtcars)
  big.tail(mtcars)
  big.tail(mtcars, 10)
  big.head(mtcars, 10, 2, 4)
  big.head(mtcars, , , 1)
}

\seealso{\code{\link{head}}, \code{\link{tail}}}