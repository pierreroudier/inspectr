[![Travis-CI Build Status](https://travis-ci.org/pierreroudier/inspectr.svg?branch=master)](https://travis-ci.org/pierreroudier/inspectr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/inspectr)](http://cran.r-project.org/web/packages/inspectr)

# Welcome to the inspectr project page

## Scope of the Package

The `inspectr` package is making it easy (or at least *easier*!) to handle spectroscopy data. It provides the user with dedicated classes (namely `Spectra` and `SpectraDataFrame`), so that most of the useful information about the spectral dataset is available in one R object:

* the spectral values 
* the wavelengths at which these have been recorded
* some kind of ID
* if available, some associated data (typically, some lab measurements)

## Graphical Capabilities

It also provides easy ways to plot a collection of spectra:

* simple line plots of the individual spectra
* offset plots of the individual spectra
* stacked plots of the individual spectra
* summary plots of a whole collection, or aggregated against a given factor
* tools to code more advanced visualisations yourself using eg `ggplot2` or `lattice`

It also gives overloads to the most common operators such as `$`, `[`, or `[[`, so that any user familiar with `data.frame` object would fell right at home.

## Processing

The philosophy of the package is really just to make it easier to work with quite complex data. There are a lot of tools already existing in R to do spectral preprocessing (`signal`, etc.). A few additional tools have been added in `inspectr`, such as the ASD splice correction. 

The idea is for the package to work quite well with the pipe (`%>%`) operator from the `magrittr` package, to create chains of pre-processing operators. The function `apply_spectra` makes it easy to work with any function whose input is either a `numeric` vector or a `matrix`:

```
# Example of splice correction, followed by
# a first derivative, followed by a SNV

my_spectra %>% 
  splice %>% 
  apply_spectra(diff, 1) %>%
  apply_spectra(snv)
```

## Regression and Classification

Again, lots of existing methods available, so `inspectr` is not re-implementing any of these. There's various ways to use `inspectr` with the different methods available, but my favoured option is to use it in conjonction with the `caret` package, which gives a unique API to 160+ models in R:

```
fit <- train(
  y = s$carbon,
  x = spectra(s),
  method = "pls"
)
spectroSummary(fit)
```
