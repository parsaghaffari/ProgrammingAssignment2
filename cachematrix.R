## In this file we define two functions for creating a special type of matrix,
## and for computing and caching its inverse 

## Creates a special type of matrix, with helper methods for getting
## and setting the value of the matrix, as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Reads the inverse of the matrix from the cache if it has been computed,
## otherwise computes the inverse and stores it in the cache
## Note: We assume the provided matrix is invertible 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# Below we've written a series of commands as tests for the above functions

my_matrix <- makeCacheMatrix(matrix(rnorm(9),3,3))
my_matrix$getinverse() # returns NULL
cacheSolve(my_matrix)
my_matrix$getinverse() # returns the true inverse
cacheSolve(my_matrix) # reads from cache
