# inspectr

In chemometrics (eg vis-NIR spectroscopy) you are analysing a bunch of spectra (a matrix of size nb of samples x nb of wavelength) vs a set of measured variables on the same samples to build up a prediction model. If the `pls` and `chemometrics` packages are satisfying to build up such relations, I was after a proper class to handle vis-NIR spectra along with their measured attributes. `inspectr` tries to make the handling of those easier. It should:

* Store spectra and attributes inside the same object
* Apply transformations/pre-processing stuff to the spectra
* Provide a transparent interface towards `chemometrics` and `plsr` packages
* Track transformations applied to the spectra
* Maybe provide `ggplot2` plots?
* What else?

And yeah, it is a crap name for a package!
