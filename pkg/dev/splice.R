## Code for splice-correction
##
#
# x a vector
# y the wavelengths 
# wl the wavelengths at which the splice correction should take place
#
# ASD version is using cubic splines
#
# The SWIR part of the spectrum (1000-1800 nm) is taken as a reference 
# for corrections as it is stable to the instrument sensitivity drift
# (Beal & Eamon, 2010)
#
# http://support.asdi.com/Document/Documents.aspx
#
splice.numeric <- function(x, y, wl = c(725:1020, 1801:1950), method = "spline") {
  # Getting index of WLs to be interpolated
  idx <- which(x %in% wl)
  y_cutted <- y[-1*idx]
  x_cutted <- x[-1*idx]
  # Interpolation
  r <- signal::interp1(x = x_cutted, y = y_cutted, xi = x, method = method)
  r
}

splice.Spectra <- function(object, wl = c(725:1020, 1801:1950), method = "spline") {
  nir <- apply_spectra(object, function(x) 
    splice.numeric(x = wl(object), y = as.vector(x), wl = wl, method = method)
  )
}
