## The pair of functions below cache the inverse of a matrix:

###############################################################################
## FUNCTION: makeCacheMatrix()
## This function creates a special "matrix" object that can cache its inverse
## 
###############################################################################
## x <- makeCacheMatrix() creates a cached matrix object x
## x$set(y) sets cached matrix object with matrix y
## x$get() gets cached matrix object
## x$setInverse uses the solve function to cache the inverse
## x$getInverse returns the cached inverse
###############################################################################

makeCacheMatrix <- function(x = matrix()) {
  ## set m (cached matrix) to null on initial function call
  m <- NULL
  
  ## function to set matrix x
  set <- function(y) {
    x <<- y
    
    ## set m (cached matrix) to NULL on any x$set operation
    m <<- NULL
  }
  ## function to return cached value of x
  get <- function() x
  
  ## function to set cached value of x to its inverse (using "solve")
  setInverse <- function(solve) m <<- solve
  
  ## function to get cached inverse value of x
  getInverse <- function() m
  
  ## list of callable functions for a cached matrix
  ## set, get, setInverse, getInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

###############################################################################
## FUNCTION: cacheSolve
## Return a matrix that is the inverse of 'x'
## 
###############################################################################
## cacheSolve(x)
## Checks to see if there is a cached copy of the inverse of x
## If so, returns the cached copy, otherwise
## computes the inverse of x using solve(x)
##
###############################################################################
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ## call the getInverse method of x and assign to m
  ## if x has a cached inverse value it will now be assigned to m
  m <- x$getInverse()
  
  ## if a cached value (this was already set previously) exists, use the cached
  ## inverse (m) rather than recomputing
  if(!is.null(m)) {
    
    ## tell the user that we're using cached values
    message("Getting cached data...")
    return(m)
  }
  
  ## otherwise, call the get method of x and assign to data
  data <- x$get()
  
  ## compute the inverse of the matrix in data using "solve" and assign to m
  m <- solve(data, ...)
  
  ## call the setInverse method of x to set the cached matrix inverse value
  x$setInverse(m)
  m
}


###############################################################################
## testcachematrix.R
##
## Test script for cachematrix.R
##
###############################################################################

## include cachematrix.R
#source("cachematrix.R")

## Call the makeCacheMatrix() function and assign to a variable z
## z is now a list of four functions (set, get, setInverse, getInverse)

#z <- makeCacheMatrix()

## use z's set function to create a 2x2 matrix containing:
##      [,1] [,2]
## [1,]    1    2
## [2,]   -1    0

#z$set(rbind(1:2,-1:0))

## use z's get function to retrieve the matrix created

#z$get()

## pass the matrix z to the cacheSolve() function
## the inverse of the matrix z should be returned:
##       [,1] [,2]
## [1,]   0.0 -1.0
## [2,]   0.5  0.5

#cacheSolve(z)

## pass the matrix z to the cacheSolve function a second time
## the inverse of the matrix z should be returned, along with a message
## "Geting cached data..." informing the user that the inverse is returned 
## using cached data rather than recalculating

#cacheSolve(z)
