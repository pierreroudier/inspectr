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
splice.numeric <- function(x, y, wl = list(725:1020, 1801:1950)) {

  # for each splice to operate
  # the current part is interpolated from a 
  # part of the spectra, defined by the size s
  corec <- lapply(wl, function(w) {
    idx <- which(x %in% w)
    signal::interp1(x[-1*idx], y[-1*idx], x, method = 'spline')
  })
  
  y.cor <- y

  for (w in 1:length(corec)) {
    cur.wls <- wl[[w]]
    cur.spec <- corec[[w]]
    idx <- which(x %in% cur.wls) 
    y.cor[idx] <- cur.spec[idx]
  }
  y.cor
}