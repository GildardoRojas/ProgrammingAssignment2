## -----------------------------------------------------------------------------
## R Programming - Assignment 2: Lexical Scoping.
## Objective: Create functions to cache the inverse of a matrix.
## Data Science Specialization, Johns Hopkins University.
## Written by Gildardo Rojas Nandayapa
## Date: 05/24/2014
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## This function creates special "matrix" object and caches its inverse.
## It provides functions to set/get both the matrix and its inverse
## -----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setmatrix <- function(y) { ## Sets matrix y and clears inverse cache
        x <<- y
        inverse <<- NULL
    }
    getmatrix <- function() x ## Returns matrix x cached
    setinverse <- function(invert) inverse <<- invert ## Sets inverse cache
    getinverse <- function() inverse ## Returns inverse matrix cached
    list(setmatrix = setmatrix, getmatrix=getmatrix,
         setinverse=setinverse, getinverse=getinverse)
}

## -----------------------------------------------------------------------------
## In general this function returns a matrix that is the inverse of 'x'
## If the inverse exists and matrix has not changed it retrieves from the cache
## If not, computes the inverse and caches it for future use without calculation
## -----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    inv <- x$getinverse() ## Fetch cached inverted matrix
    if(!is.null(inv)) { ## If it exists returns cached matrix
        message("Retrieving from cached matrix...")
        return(inv)
    }
    data <- x$getmatrix() ## Sets data equal to matrix to invert
    message("Calculating and caching inverted matrix...")
    inv <- solve(data, ...) ## Calculates inverted matrix
    x$setinverse(inv) ## Passes result to cache for future usage
    inv ## Returns recently calculated matrix
}