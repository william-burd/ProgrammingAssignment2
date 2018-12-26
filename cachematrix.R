# A pair of functions:
#     1. makeCacheMatrix : caches a matrix and its inverse.
#     2. cacheSolve      : takes a makeCacheMatrix object and returns its inverse.


# makeCacheMatrix caches a matrix, caches its inverse (initially NULL) and creates getters and setters 
# for the matrix and its inverse.
#
# Args:
#    theMatrix : a matrix.
# 
# Returns:
#    A list of 4 functions: getters and setters for the cached matrix and its inverse.
#
makeCacheMatrix <- function(theMatrix = matrix()) {

    inverseOfTheMatrix <- NULL

    set <- function(m) {
        theMatrix <<- m
        inverseOfTheMatrix <<- NULL
    }

    get <- function() theMatrix
    getMatrixClass <- function() {
        class(theMatrix) 
    }

    setInverseMatrix <- function(solvedMatrix) inverseOfTheMatrix <<- solvedMatrix

    getInverseMatrix <- function() inverseOfTheMatrix 

    list(set = set, 
         get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)

}


# cacheSolve takes a makeCacheMatrix object and returns its inverse as a matrix object.
#
# If the inverse is not cached, call R's "solve" function to get the inverse and cache it,
# otherwise return the cached inverse value.
#
# Args:
#    theCacheMatrix : the object returned by the makeCacheMatrix function.
#    ...            : arguments passed through to R's solve function.
#
# Returns:
#    The inverse matrix of the theCacheMatrix.
#
cacheSolve <- function(theCacheMatrix, ...) {

        inverseOfTheMatrix <- theCacheMatrix$getInverseMatrix()

        if(!is.null(inverseOfTheMatrix)) {
                message("getting cached data")
                return(inverseOfTheMatrix)
        }

        data <- theCacheMatrix$get()
        inverseOfTheMatrix <- solve(data, ...)
        theCacheMatrix$setInverseMatrix(inverseOfTheMatrix)
        inverseOfTheMatrix 

}

