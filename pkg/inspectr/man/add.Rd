\name{add}
\alias{add}
\alias{add.Spectra}
\alias{add.SpectraDataFrame}

\title{Adds \code{Spectra*} objects together}

\usage{add(x, y,...)}

\description{Adds two or more \code{Spectra*} objects together.}

\details{
This function adds two or more \code{Spectra*} objects together. It can of add objects of similar class (e.g. two \code{Spectra} objects, or three \code{SpectraDataFrame} objects). The resulting object will have the same class as the objects it has been created from. The only condition is that the \code{id} of the objects to concatenate must NOT overlap.

At present, \code{Spectra} and \code{SpectraDataFrame} objects can NOT be added together.
}

\value{An object of the class \code{Spectra} or \code{SpectraDataFrame} (see Details).}

\arguments{
  \item{x}{an object inheriting from class \code{Spectra}}
  \item{y}{an object inheriting from class \code{Spectra}}
  \item{...}{more objects inheriting from class \code{Spectra}}
}

\author{Pierre Roudier \url{pierre.roudier@gmail.com}}