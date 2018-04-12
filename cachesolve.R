cacheSolve <- function(x, ...) {
  invers <- x$getInverse()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  invers <- mean(data, ...)
  x$setmean(invers)
  invers
}
