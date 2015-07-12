## Coursera - R Programming: Programming Assignment 2
## use if  <<- operator

# makeCacheMatrix function is used to create and store a matrix
# and its inverse
#
# It contains following functions:
#   set - sets the original matrix
#   get - returns the original matrix
#   setInverse - stores the inverse of the matrix
#                used by cacheSolve
#   getInverse - returns the inverse of the matrix
#                or NULL if inverse does not exist
#
#
#   setRandom - can be used to set the original matrix
#               parameters: N - the number of rows/columns - default 2
#                           valuesRange - vector of int, default 0:5
#   setTM - creates a "nice" 2x2 matrix. Its inversion is also made of
#           integers.

makeCacheMatrix <- function(x = matrix()) {
    
    inverseMatrix <- NULL
    
    get <- function() x
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    getInverse <- function() inverseMatrix
    setInverse <- function(im) inverseMatrix <<- im

    setRandom <- function(N = 2, valuesRange = 0:5) {
        x <<- matrix(sample(valuesRange, N*N, TRUE), N)
        inverseMatrix <<- NULL
    }
    
    setTM <- function() {
        x <<- matrix(c(4,3,3,2), nrow = 2)
        inverseMatrix <<- NULL
    }
    
    list(set = set, setInverse = setInverse,
         get = get, getInverse = getInverse,
         setRandom = setRandom, setTM = setTM)
    
}


# cacheSolve returns a matrix that is the inverse of 'x'

# If the inverse of the matrix is already in the cache, the message
# "getting cached data" is shown first, followed by the inverse matrix
# Should the matrix not be cached yet, it is computed first and than 
# displayed (no text "getting cached data" shown)

cacheSolve <- function(x, ...) {

    if(!is.null(x$getInverse())) {
        message("getting cached data")
        return(x$getInverse())
    }

    x$setInverse(solve(x$get()))
    x$getInverse()
}

######################################################################
# Usage:
#       a <- makeCacheMatrix()  # Creates a cachematrix variable
#       a$setTM()               # Sets the initial matrix. set(matrix)
#                               # or setRandom() can also be used
#       a$get()                 # Shows the initial matrix
#       a$getInverse()          # Returns NULL as no inverse exists
#       cacheSolve(a)           # Shows (and sets) the inverse of the
#                               # matrix
#       a$getInverse()          # Returns the inverse matrix
#       a$getInverse()          # Returns "getting cached data"
#                               # + the inverse matrix
#       a$get() %*% a$getInverse()
#                               # Multiplies the matrixes - returns
#                               # an identity matrix
######################################################################