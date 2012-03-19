\name{aggregate_spectra}
\docType{methods}

\alias{aggregate_spectra,Spectra-method}
\alias{aggregate_spectra,SpectraDataFrame-method}
\alias{aggregate_spectra}

\title{Aggregates spectral and data information}

\usage{aggregate_spectra(obj, fun = mean, id = NULL, ...)}

\description{Aggregates spectral and data information of a \code{Spectra} object using a user-defined function.}

\details{Aggregates the spectral information of a \code{Spectra} object using 
an aggregation function chosen by the user. 

There is two distinct function for \code{Spectra} and \code{SpectraDataFrame} classes. For \code{SpectraDataFrame} objects, associated data is also aggregated using the function provided by the \code{fun} option. Additionally, the method for \code{SpectraDataFrame} has an \code{id} option that allows to specify an attribute which will be used to split the object, apply sequentially the \code{fun} function, and recombine the results in an unique object.}

\value{An object of the same class as \code{obj}}

\arguments{
  \item{obj}{an object inheriting from class \code{Spectra}}
  \item{fun}{an aggregation function}
  \item{id}{attribute(s) to split the object}
  \item{...}{expressions evaluated in the context of \code{fun}}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

\seealso{\code{\link{apply_spectra}}}

\examples{
# Loading example data
data(australia)
spectra(australia) <- sr_no ~ ... ~ 350:2500

# Aggregation on the whole collection
m <- aggregate_spectra(australia, fun = mean)
summary(m)
plot(m)

# Aggregation factor-wise
australia$fact <- sample(LETTERS[1:3], size = nrow(australia), replace = TRUE) # Generate some kind of factor
summary(australia)
m <- aggregate_spectra(australia, fun = mean, id = 'fact')
summary(m)
plot(m)
}