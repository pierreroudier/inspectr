## Kennard-Stone algorithm for clibration set selection
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
kenstone.matrix <- function(x, size, ...){
  # Initialisation
  k <- 1

  # Compute data mean
  mn <- colMeans(x)
  # Get closest point to the mean
  idx <- which.min(.distToMean(x, mn))
  # Put point into subset and remove point from x
  sub[[k]] <- idx
  k <- k + 1
  x <- x[-1*idx,]

  # Get farest point from the first selected subset
  idx <- which.max(.distToMean(x, x[idx,]))
  # Put point into subset and remove point from x
  sub[[k]] <- idx
  k <- k + 1
  x <- x[-1*idx,]
  
  # Sequentially select points
  while(k < size) {
    # Select the biggest distance ????
    [ii,ww]=max(min(fastdist(x(:,2:n),X(model,:))));
    model(d)=x(ww,1);
    x(ww,:)=[];
  }
  
  do.call('c', sub)
}

.distToMean <- function(x, mn){
  apply(x, 1, function(x) dist(rbind(mn, x)))
}