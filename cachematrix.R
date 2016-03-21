## There are two functions 
## 1. makeCacheMatrix sets/gets the matrix and its inverse value in the cache
## 2. cacheSolve calculates the inverse of matrix only if the inverse is not been 
##    calculated before. After calculating the value is set in the cache.


## makeCacheMatrix function is resposible for - 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  set <- function(y){
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function checks whether the inverse of the matrix is already in cache.
## If not then it calculates the inverse using solve function and sets it to the cache.
## Otherwise it returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixInverseValue <- x$getInverse()
  if(!is.null(matrixInverseValue)) {
    message("getting cached data")
    return(matrixInverseValue)
  }
  data <- x$get()
  matrixInverseValue <- solve(data, ...)
  x$setInverse(matrixInverseValue)
  matrixInverseValue
}
