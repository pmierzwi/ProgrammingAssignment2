## The functions makeCacheMatrix and cacheSolve are designed to create a special "matrix" 
## object that can cache its inverse and to compute the inverse of the special "matrix".

## The makeCacheMatrix function creates special object to store matrix and its inverse matrix
## When the object is initialised with set() function the matrix is stored in ma, and its 
## inverse is set to NULL. After calculating inverse matrix with cacheSolve function the
## value of inverse matrix (inv) is updated.
## ------------------------------------------------------------------------------------------
## set() - sets the matrix to be cached
## get() - returns the actual cached matrix
## setinv(inverse) - sets the inverse of cached matrix to the value 'inverse'
## getinv() - returns actual value of inverse matrix

makeCacheMatrix <- function(ma = matrix()) {
  inv <- NULL
  set <- function(y) {
    ma <<- y
    inv <<- NULL
  }
  get <- function() ma
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function cacheSolve is designed to return inverse matrix of cached matrix object created
## with makeCacheMatrix. It has two ways of behaviour depending on whether inverse matrix 
## was already calculated or not. If it was calculated previously it returns previous result
## in another case it calculates and returns the result of the inverse matrix
## -----------------------------------------------------------------------------------------
## x - the object of makeCacheMatrix type.

cacheSolve <- function(x, ...) {
  ma <- x$getinv()
  if(!is.null(ma)) {
    message("getting cached data")
    return(ma)
  }
  dane <- x$get()
  inv <- solve(dane, ...)
  x$setinv(inv)
## Return a matrix that is the inverse of 'x'
  inv
}
