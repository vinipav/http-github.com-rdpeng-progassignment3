# http-github.com-rdpeng-progassignment3
makeCacheMatix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setInverse <- function(mean) invers <<- inverse
  getInverse <- function() invers
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
