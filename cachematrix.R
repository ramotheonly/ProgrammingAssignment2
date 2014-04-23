## Put comments here that give an overall description of what your
## functions do

## Author: Rakesh
## Date: 22-Apr-2014
## This file holds 2 functions to implement a specialized Matrix class that holds a cached version of its own inverse  

## Write a short comment describing this function
## This function constructs the specialized Matrix class that manages the cache of its own inverse 
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL      ## the member variable to hold the inverse cache 
  
  ## set method to reset a new data matrix
  set <- function(y) {    
    minv <<- NULL     ## reset the cache, since the matrix is changed
    x <<- y           ## set the x symbol in the parent scope
  }

  ## get method to retrieve the data matrix
  get <- function() x            ## return the x from the parent scope, since none defined in current scope

  ## setsolve method to set the supplied inverse in minv symbol in parent scope
  setsolve <- function(minverse) minv <<- minverse 
  
  ## getsolve method to return the inverse minv from the parent scope
  getsolve <- function() minv
  
  ## expose the defined methods to consumer of the this class
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## cacheSolve() implements the caching mechanism for the specialized cached matrix
cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()   ## get cache of inverse matrix from the specialized matrix object
  if(!is.null(m)) {   ## Yay! we've got a Cache HIT!!
    message("getting cached data")
    return(m)         ## return the cached data
  }
  
  ## The inverse was not cached yet. Okay, let's cache it below
  data <- x$get()         ## get the matrix data
  m <- solve(data, ...)   ## compute the inverse
  x$setsolve(m)           ## cache the computed inverse in our specialized matrix object
  m                       ## return the computed matrix inverse (which has now been cached for subsequent calls until matrix is changed)
}


##===========================================================
## U S A G E:     
##===========================================================
## > mat <- matrix(rexp(1000000, rate=.1), nrow=1000, ncol=1000)    # a million elements matrix!!!
## > smat <- makeCacheMatrix(mat)
## > cacheResolve(smat)
## > library(microbenchmark)    ##install microbenchmark package, if not already installed
## > microbenchmark(solve(mat), smat$getsolve())
## Unit: nanoseconds
##            expr       min        lq    median        uq       max neval
##      solve(mat) 681462881 694886044 703541093 722494662 753621577   100
## smat$getsolve()         0       428       856      7271     31647   100
##