\name{apply_spectra}
\alias{apply_spectra}

\title{Apply a function on the spectra of a Spectra* object}

\usage{apply_spectra(obj, fun, ...)}

\description{Aggregates spectral and data information of a \code{Spectra} object using a user-defined function.}

\details{Apply a function and update the spectra of a \code{Spectra} object. This function is particularly interesting for pre-processing, e.g to derive the spectra, or apply pre-processing functions such as \code{\link{snv}}.

The philosophy of this function is to let the user free to use any function to pre-process a spectra collection, using either functions from the stats package, functions from other packages such as \code{signal}, or personal functions.}

\value{An object of the same class as \code{obj}}

\arguments{
  \item{obj}{an object inheriting from class \code{Spectra}}
  \item{fun}{an aggregation function}
  \item{...}{expressions evaluated in the context of \code{fun}}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{aggregate_spectra}}, \code{\link{snv}}, \code{\link{rnv}}}

\examples{
#res <- apply_spectra(foo, diff, 2)
#plot(res)
}
