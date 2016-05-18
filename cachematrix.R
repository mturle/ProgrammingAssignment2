# Programmatic Assignement 3

# This function returns a list containing a function to store (set), read (get)
# the value of a matrix and later to store the the inverted matrix or retrieve it
# from cache

makeCacheMatrix <- function(x = matrix()) {
    
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) minv <<- inverse
    getinverse <- function() minv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}


# This function returns an iverted matrix via SOLVE function, but first
# checks wheter an inverted matrix is not already stored in cache. If
# yes, the inverted matrix is read and not calculated

cacheSolve <- function(x, ...) {
    
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data.")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data)
    x$setinverse(minv)
    minv
}

# Test

# A simple matrix x (3x3)
# > x <- matrix(c(1,3,3,1,4,3,1,3,4), nrow=3, ncol=3)

# Storing the matrix
## > m <- makeCacheMatrix(x)

# Retrieve the original matrix from m variable
## > m$get()

#       [,1] [,2] [,3]
# [1,]    1    1    1
# [2,]    3    4    3
# [3,]    3    3    4

# Test the cacheSolve function for the first run (the inverse of a matrix 
# is calculated)

# > cacheSolve(m)
#       [,1]          [,2] [,3]
# [1,]    7 -1.000000e+00   -1
# [2,]   -3  1.000000e+00    0
# [3,]   -3  1.665335e-16    1

# > cacheSolve(m)

# Test the cacheSolve function for the second run (the inverse of a matrix 
# is read from cache)

# > cacheSolve(m)

# > getting cached data.
# >      [,1]          [,2] [,3]
# > [1,]    7 -1.000000e+00   -1
# > [2,]   -3  1.000000e+00    0
# > [3,]   -3  1.665335e-16    1


 
