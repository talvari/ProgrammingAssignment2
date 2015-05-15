## These two functions are used to cache the inverse of a matrix
## to the memory to aid in faster calculations.

## This function is used to make a matrix and store its inverse in a cache
## It returns a list of functions that can be used to set/get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #initialize the inverse to NULL
    set <- function(y) { #function to set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x #function that returns the value of the matrix
    setinv <- function(inverse) { #function to cache the given inverse
        inv <<- inverse
    }
    getinv <- function() inv #function to return the inverse

    #return a list of the above functions
    list(set =set, get=get, setinv=setinv, getinv=getinv)
}


## This function solves the inverse of a matrix but first checks if the
## inverse has already been stored in cache and if not, stores the calculated
## inverse.

cacheSolve <- function(x, ...) {
    #first check if the inverse has already been calculated
    inv = x$getinv()

    #if it has already been calculated, return the cached value
    if (!is.null(inv)) {
        message("Getting cached inverse matrix") #this helps to see when it's returning the cached value
        return(inv)
    }

    #otherwise calculate the inverse and cache it in the memory
    data <- x$get()
    inverse <- solve(data)
    x$setinv(inverse)
    #Return a matrix that is the inverse of 'x'
    inverse
}
