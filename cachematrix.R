## Coursera R Programming - Assignment 2
## This script contains a set of functions to define a
## matrix-type object whose inverse can be cached.
## When extracting the inverse, an attempt is made to
## retrieve cached data. If nothing has been stored, the
## inverse is calculated from scratch.

## This function defines a matrix-type object
## capable of caching its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  #Inverse of the matrix
  xInv <- NULL
  
  #Function to set the matrix
  set <- function(y){
    x <<- y
    xInv <<- NULL
  }
  
  #Function to get the matrix
  get <- function() x
  
  #Function to set the inverse
  setinv <- function(inv) xInv <<- inv
  
  #Function to get the inverse
  getinv <- function() xInv
  
  #In reality, all we need is a vector of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to compute the inverse of a "cacheMatrix" object.
## It first attempts to retrieve the inverse from the cache.
## If NULL, it computes the inverse with the solve() function
cacheSolve <- function(x, ...) {
  xInv <- x$getinv()
  
  #Check if inverse has been previously found and cached
  if(!is.null(xInv)){
    message("Retrieving cached inverse...")
    return(xInv)
  }
  
  #If nothing in cache, get matrix and compute inverse
  matrix <- x$get()
  xInv <- solve(matrix)
  x$setinv(xInv)
  
  #Return inverse
  xInv
}
