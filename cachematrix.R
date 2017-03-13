#--------------------------------------------------------------------------------
# File Name:    cacheMatrix.R
# File Summary: This file defines two functions, makeCacheMatrix() and 
#               cacheSolve(). These functions provide a means of caching a
#               matrix's inverse, allowing it to be reused without recalculation.
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# Function Name:    makeCacheMatrix()
# Function Summary: creates a cached environment for storing a matrix and its
#   calculated inverse. It also defines several accessor functions to allow 
#   access to the cached variables. A named list of these functions is returned.
#--------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
    # checkSquare() ensures that the matrix passed in is "square" (i.e. 
    # possesses the same number of rows and columns). As it is not possible to 
    # calculate the inverse of a non-square matrix, this method will call stop() 
    # if the matrix is not square)
    checkSquare <- function(m)
    {
        if (nrow(m) != ncol(m))
        {
            stop("The matrix passed in is not square and therefore cannot have its inverse calculated")
        }
    }
    
    # ensure that the matrix passed in is square
    checkSquare(x)
    
    # set the inverse function initially to NULL
    inverse <- NULL
    
    # set() changes the cached matrix data in its parent environment. It also
    # clears the cached inverse matrix
    set <- function(y) 
    {
        checkSquare(y)
        x <<- y
        inverse <<- NULL
    }
    
    # get() returns the actual matrix data we are holding
    get <- function() x
    
    # setInverse() saves the solved inverse matrix into its parent environment
    setInverse <- function(solvedInverse) inverse <<- solvedInverse
    
    # getInverse() returns the currently solved inverse matrix. If this has not
    # been calculated, then NULL is returned.
    getInverse <- function() inverse
    
    # return a named list of functions that provide access to the variables 
    # stored in this function's environment
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#--------------------------------------------------------------------------------
# Function Name:    cacheSolve()
# Function Summary: using the list returned from makeCacheMatrix(), this function
#   returns the inverse of the matrix. If the inverse has already been calculated
#   it simply returns that value, otherwise, it performs the calculation and 
#   stores that value in the environment created by makeCacheMatrix()
#--------------------------------------------------------------------------------
cacheSolve <- function(x, ...)
{
    # try to get the matrix inverse from the cached environment
    inverse <- x$getInverse()
    
    # if the inverse has already been calculated, then return that.
    if(!is.null(inverse)) 
    {
        message("Returning cached inverse")
        return(inverse)
    }
    
    # if we get to here, we have not yet calculated the inverse.
    # First, get the real matrix
    data <- x$get()
    
    # calculate it's inverse
    inverse <- solve(data, ...)
    
    # save the calculated inverse to the cached environment
    x$setInverse(inverse)
    
    # return the inverse
    inverse
}
