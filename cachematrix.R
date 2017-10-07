## Coursera's Data Science Specialization from John Hopkins Univ
## R Programming course, week 3
## Programm Assignment number 2
## These pair functions calculate a matrix inverse and takes benefit of 
## lexical scoping for retrieving previously calculated matrix stored in cache

## Creates an object that stores "get" and "set" functions for the matrix x and its inverse matrix m
## It also stores the matrix itself and its inverse matrix m, in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## Initializing the matrix inverse to ensure that it is stored in cache (in the makeCacheMatrix object's environment)
  set <- function(y) {
    x <<- y
    m <<- NULL  ## if the "set" function is called, the matrix inverse is reset to NULL to delete the "old" inverse matrix from cache
                ## therefore when cacheSolve() is called later, the matrix inverse must be recalculated
  }
  get <- function() x ##returns the matrix currently stored
  setinv <- function(inv_mat) m <<- inv_mat ##inv_mat is the inverted matrix, which will be calculated by the other function cacheSolve
  getinv <- function() m ##returns the matrix inverse currently stored
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes as input an output from the makeCacheMatrix function above.
## It retrieves the matrix inverse stored in the makeCacheMatrix object. If this matrix inverse has not
## been calculated yet, it is calculated and stored in the makeCacheMatrix object. If the matrix inverse
## has already been calculated for this matrix, it is retrieved from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  
  if(!is.null(m)) { ## testing if an inverted matrix is cached
    message("getting cached data")
    return(m) ##return the cached matrix
  }
  
  ## if the matrix inverse has not been calculated yet :
  data <- x$get()  ## matrix that has to be inverted is retrieved
  m <- solve(data,...) ## matrix inverse is calculated
  x$setinv(m) ##matrix inverse is stored in the makeCacheMatrix object
  m     ## matrix inverse is displayed
}
