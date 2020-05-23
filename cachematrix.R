## Put comments here that give an overall description of what your
## functions do
## functions below namely "makeCachematrix" and "cacheSolve" are supposed to create a matrix object
## and cache the invesrse of the matrix.

## Write a short comment describing this function
## the function "makeCachematrix" will create a matrix object (this matrix is square matrix and hence is invertible)
## that can cache its inverse for the input.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## the function "cacheSolve" will compute the inverse of the "matrix object" mentioned while describing   
## the work of "makeCacheMatrix" function above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
