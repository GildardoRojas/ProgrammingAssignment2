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
## -----------------------------------------------------------------------------
## Evaluation suggested test cases, copy and paste to the console text after ">". 
## 1. Source this code to enable makeCacheMatrix and cacheSolve functions.
## 2. Create myCachedMatrix object as a 2x2 test matrix.
## -----------------------------------------------------------------------------
#     > myCachedMatrix <- makeCacheMatrix(matrix(c(0, -5, -4, 0), nrow=2, ncol=2, byrow=TRUE))
#  3. Call getmatrix function to print the recently stored matrix.
#     > myCachedMatrix$getmatrix()
#  4. Notice inverse doesn't exist yet, you'll get a NULL if you type:
#     > myCachedMatrix$getinverse()
#  5. Call cacheSolve, this time the inverse is being calculated.
#     > cacheSolve(myCachedMatrix)
#  6. Now the inverse is stored repeating step 4 returns the inverse matrix.
#     > myCachedMatrix$getinverse()
#  7. Comprobation: any matrix times it's inverse equals the identity matrix. 
#     > myCachedMatrix$getmatrix() %*% myCachedMatrix$getinverse()
#  8. If we need the inverse matrix, cacheSolve will retrieve the cached matrix.
#     > cacheSolve(myCachedMatrix)
#  Now we will change the matrix by initializing a different one:
#  9. Input a different 2x2 matrix.
#     > myCachedMatrix$setmatrix(matrix(c(0, -3, -2, 0), nrow=2, ncol=2, byrow=TRUE))
#  10. Print the matrix stored.
#     > myCachedMatrix$getmatrix()
#  11. Inverse is NULL again as the matrix has changed, inverse has to be computed.
#     > myCachedMatrix$getinverse()
#  12. Lets call again cacheSolve, this forces calculation again, notice the message.
#     > cacheSolve(myCachedMatrix)
#  13. Let's test again the matrix and its inverse, we get an identity matrix.
#     > myCachedMatrix$getmatrix() %*% myCachedMatrix$getinverse()
#  14. We can display the inverse directly
#     > myCachedMatrix$getinverse()
#  15. If we call again the inverse, it is just retrieved from cache, not calculated
#     > cacheSolve(myCachedMatrix)
#  Bigger matrices: You can just try a 3x3 matrix or a huge one to notice delay
#  Here some cases to test, if you wish to do it.
#     > myCachedMatrix <- makeCacheMatrix(matrix(runif(9,-10,10), nrow=3, ncol=3, byrow=TRUE))
#     > myCachedMatrix <- makeCacheMatrix(matrix(runif(2250000,1,10),ncol=1500,byrow=TRUE))
#  Thanks for reading and testing!