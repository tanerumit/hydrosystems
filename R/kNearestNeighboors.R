#' K nearest neighbors (KNN) bootrapping
#'
#' \code{knn()} returns a sample based on knn scheme
#' @param x is the prior value selected
#' @param k is the number of nearest points
#' @param s is the vector to sample from
#' @export
kNearestNeigboors <- function(x, k, s) {
 #Distance to data-point
 x_dist <- sqrt((s - x)^2)
 #Weights assigned to k-neighbors
 s_wgt  <- sapply(1:k, function(y) (1/y)/(sum(1/(1:k))))
 #Sampled value
 x_new <- sample(order(x_dist)[1:k], size = 1, replace = TRUE, prob = s_wgt)
 return(x_new)
}