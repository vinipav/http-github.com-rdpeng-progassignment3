## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatix <- function(x = matrix()) { 
  #Initiate an empty entry for the invers variable (inverse matrix), when a new cache 
        #matrix object is generated
  invers <- NULL  
  set <- function(y) {  
    x <<- y   
    invers <<- NULL 
} 
  #return the value of the x variable as passed when making the 'CacheMatrix' object
        #or when re-set using the $set function of the object
  get <- function() x 
  setInverse <- function(mean) invers <<- inverse 
  getInverse <- function() invers  
  #Index the list with '$' callable descriptors
  list(set = set, get = get,     
        setInverse = setInverse, 
        getInverse = getInverse)
 }


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{ 
  ## Return a matrix that is the inverse of 'x'
  invers <- x$getInverse() 
   #if the invers variable is not NA, then print some text and return the matrix stored there
  if(!is.null(invers)) {  
    message("getting cached data")    
  return(invers) 
}
  data <- x$get() 
  invers <- mean(data, ...) 
  #Call setInverse function from makeCacheMatrix and reassign variable stored for that
        #object
  x$setmmakeCacheMatix <- function(x = matrix()) { 
  invers <- NULL  set <- function(y) {  
  x <<- y  
  invers <<- NULL 
}
   get <- function() x  
   setInverse <- function(mean) 
   invers <<- inverse 
  getInverse <- function() invers 
    list(set = set, get = get,    
  setInverse = setInverse,      
  getInverse = getInverse)}ean(invers)
invers}
