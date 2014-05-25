## Functions to create a cached copy of the inverse of a matrix and then
## on subsequent calls of the function retrieve the cached copy rather than
## recomputing it.

## This function returns a function that is a list of four functions:
## 1) set - makes a cached copy of the specified matrix in a separate
## environment from the current environment ;
## 2) get - returns the cached copy of the matrix
## 3) setinversematrix - solves for the inverse of the matrix and caches
## the inverse matrix in the separate environment
## 4) getinversematrix - returns the cached copy of the inverse matrix
## common usage: cachedAmatrix <- makeCacheMatrix(Amatrix)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) im <<- solve
  getinversematrix <- function() im
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## This function computes the inverse of a matrix, or retrieves a cached copy
## of it if the inverse has been computed previously.
## It uses the list created by the makeCacheMatrix function to 
## retrieve the inverse matrix if it has been previously computed, 
## or to compute, cache, and return the inverse matrix if it has not been
## previously computed.
## common usage: cacheSolve(cachedAmatrix)
##   (after calling:  cachedAmatrix <- makeCacheMatrix(Amatrix) ) 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinversematrix()
  if(!is.null(im)){
    message("getting cached matrix data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinversematrix(im)
  im
}

