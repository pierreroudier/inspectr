\name{kenstone}
\alias{kenstone}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{kenstone,Spectra-method}

\title{
Kennard-Stone algorithm for optimal calibration set selection.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
An implemnentation of the Kennard-Stone algorithm for calibration set selection.
}
\usage{
kenstone(x, size, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{Spectra} object}
  \item{size}{a positive number, the number of items to choose from}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
A vector of length size giving the indices of the selected individuals in x.
%% ...
}
\references{
%% ~put references to the literature/web site here ~
R.W. Kennard, L.A. Stone, Technometrics 11 (1969) 137.
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}

