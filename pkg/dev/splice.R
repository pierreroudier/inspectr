## Code for splice-correction
##
#
# x a vector
# y the wavelengths 
# wl the wavelengths at which the splice correction should take place
# s the size of the window for interpolation
#
# ASD version is using cubic splines
#
splice.numeric <- function(x, y, s = 5) {
  require(signal)

  # wavelengths where the correction should occur
  sites <- c(1000, 1830)
  
  # identify the wavelengths to correct
  wls <-  list(
    seq(from = sites[1] - s, to = sites[1]),
    seq(from = sites[2], to = sites[2] + s)
  )

  # for each splice to operate
  # the current part is interpolated from a 
  # part of the spectra, defined by the size s
  
  idx <- which(x %in% wls[[1]])
  
  wl <- x[-1*idx]
  nir <- y[-1*idx]
  browser()
  interp1(wl, nir, x, method = 'cubic', extrapolate = TRUE)

  wl <- x[idx]
  nir <- y[idx]

}