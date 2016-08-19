#' K nearest neighbors (KNN) bootrapping
#'
#' \code{knn()} returns a sample based on knn scheme
#' @param x is the prior value selected
#' @param k is the number of nearest points
#' @param s is the vector to sample from
#' @export
knn <- function(x, k, s) {
    
    x.dis <- sqrt((s - x)^2)
    s.ind <- which(x.dis %in% sort(x.dis)[1:k])
    s.wgh <- sapply(1:k, function(y) (1/y)/(sum(1/(1:k))))
    x.new <- sample(s[s.ind], size = 1, replace = TRUE, prob = s.wgh)
    x.new.ind <- which(x.new == s)
    
    return(x.new.ind)
}

#' Inverse Box-cox transformation
#'
#' \code{boxcox_inverse()} returns a sample based on knn scheme
#' @param lamda value extracted from power transform
#' @param Y box-cox transformation value
#' @export
boxcox_inverse <- function(lambda, Y) {
    
    if (lambda == 0) {
        result <- exp(Y)
    }
    if (lambda != 0) {
        result <- (lambda * Y + 1)^(1/lambda)
    }
    return(result)
    
}

#' Function to extract the overall ANOVA p-value out of a linear model object
#'
#' \code{extract_pval()} returns a sample based on knn scheme
#' @param modelobject is the model to extract p values from
#' @export
extract_pval <- function(modelobject) {
    if (class(modelobject) != "lm") 
        stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1], f[2], f[3], lower.tail = F)
    attributes(p) <- NULL
    return(p)
}
