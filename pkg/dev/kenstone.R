.kenstone.Spectra <- function(x, size, ...) {
  kenstone.matrix(spectra(x), size = size, ...)
}

## Kennard-Stone algorithm for calibration set selection
##
## -- use it on PCs to compress the data !! --
##
#
# 1. Select the object which is closest to the data mean
# and add it to the subset
#
# 2. Calculate the dissimilarity between the remaining objects
# in the data set and the objects in the subset
#
# 3. Select the object, which is the most dissimilar to the ones
# already included in the subset
#
# 4. return to 2 until the desired number of objects in the subset
# is reached
#
kenstone.matrix <- function(x, size, progress = TRUE, ...){
  # Initialisation
  k <- 1
  sub <- vector(mode = 'numeric', length = size)
  sub_mat <- matrix(NA, ncol = ncol(x), nrow = size)

  # Compute data mean
  mn <- colMeans(x)
  # Get closest point to the mean
  idx <- which.min(.distToMean(x, mn))
  # Put point into subset and remove point from x
  sub[k] <- idx
  sub_mat[k,] <- x[idx,]
  k <- k + 1
  x <- x[-1*idx,]

  # Get farest point from the first selected subset
  idx <- which.max(.distToMean(x, x[idx,]))
  # Put point into subset and remove point from x
  sub[k] <- idx
  sub_mat[k,] <- x[idx,]
  k <- k + 1
  x <- x[-1*idx,]
  
  if (progress) {
    i_pb <- 1
    pb <- txtProgressBar(min = 1, max = size, style = 3)
  }      

  # Sequentially select points
  while(k <= size) {
    #
    # (1) compute the distance between each point in remaining in x
    # and each point already in the subset
    # (2) for each point in x, select the MINIMUM distance with each
    # point already in the subset
    # (3) the algorithm is selecting the point in x that have the 
    # BIGGEST minimum value (ie is most dissimilar to points already
    # in the subset)
    #
    # I know it sounds crazy but it goes better with a pen and paper
    #
    idx <- which.max(apply(.distToSubset(x, sub_mat[1:(k-1),]), 2, min))
    sub[k] <- idx
    sub_mat[k,] <- x[idx,]
    k <- k + 1
    x <- x[-1*idx,]

    if (progress) {
      setTxtProgressBar(pb, i_pb)
      i_pb <- i_pb + 1
    }
  }

  if (progress)
    close(pb)

  sub
}

# Distance of each point of a matrix x to
# a unique value
.distToMean <- function(x, mn){
  apply(x, 1, function(x) dist(rbind(mn, x)))
}

# Distance of each point in a matrix x to
# each point in another matrix sub
.distToSubset <- function(x, sub) {
  apply(x, 1, function(y) apply(sub, 1, function(x) dist(rbind(x, y))))
}

## OptiSim, an efficient implementation of the KS algorithm
##
## -- use it on PCs to compress the data !! --
##
#
# b is the number of objects in the random subset
#
optisim.matrix <- function(x, size, B = round(0.10*nrow(x)), progress = TRUE, ...) { 
  
  n <- nrow(x)
  m <- ncol(x)
  min_x <- apply(x, 2, min, na.rm = TRUE)
  max_x <- apply(x, 2, max, na.rm = TRUE)
  range <- max_x - min_x
  V <- cumprod(range)[length(range)]
  Vs <- (1/size)*V
  R <- (Vs/(sqrt(pi^m)/gamma(m/2 + 1)))^(1/m)

  mean_x <- colMeans(x)
  d <- colSums((x - matrix(rep(mean_x, n), nrow = n, byrow = TRUE))^2)
  i <- min(d)
  r <- which.min(d)
  
  model <- r
  A <- 1:n
  A <- A[-r]
  recycling <- NULL
  mS <- 1
  ma <- length(A)

  if (progress) {
    i_pb <- 1
    pb <- txtProgressBar(min = 1, max = size, style = 3)
  }      


  while(mS < size) {

    r <- sample(1:ma, size = ma)

    if (B < ma) 
      r <- r[1:B]
    
    odleglosc <- .distToSubset(x[model, , drop = FALSE], x[A[r], , drop = FALSE])
    
    if (mS > 1) 
      odleglosc <- min(odleglosc) # apply(odleglosc, 2, min)

    remove <- which(odleglosc < R)
    i1 <- max(odleglosc)
    i2 <- which.max(odleglosc)
    model <- c(model, A[r[i2]])

    if (length(remove) > 0) 
      A <- A[-r[remove]]
    
    r <- r[-i2]
    recycling <- c(recycling, r)

    if (length(A) == 0)
      A <- unique(recycling)
    
    ma <- length(A)
    mS <- length(model)

    if (progress) {
      setTxtProgressBar(pb, i_pb)
      i_pb <- i_pb + 1
    }
  }

  if (progress)
    close(pb)

  model
}
