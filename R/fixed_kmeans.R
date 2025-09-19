fixed_kmeans <- function(Feature, Centers) {
  
  ## distance matrix: rows = observations, cols = centres
  dists <- abs(outer(Feature, Centers, FUN = "-"))
  
  ## nearest centre index for each observation
  cluster <- max.col(-dists, ties.method = "first")
  
  #d dimensional case
  # # Data: matrix or data.frame of points (rows = samples, columns = features)
  # # Centers: matrix or data.frame of fixed cluster centers (rows = centers, columns = features)
  # 
  # if (!is.matrix(Data)) Data <- as.matrix(Data)
  # if (!is.matrix(Centers)) Centers <- as.matrix(Centers)
  # 
  # # Sanity checks
  # if (ncol(Data) != ncol(Centers)) stop("Data and centers must have the same number of columns (features).")
  # 
  # # Compute distances from each point to each center
  # dists <- as.matrix(dist(rbind(Data, Centers)))
  # dists <- dists[1:nrow(Data), (nrow(Data)+1):(nrow(Data)+nrow(Centers))]
  # 
  # # Assign each point to the closest center
  # Cls <- apply(dists, 1, which.min)
  
  return(cluster)
}