# This function creates a list object 
# from the input matrix containing functions to
#   1  set the value of the matrix
#   2  get the value of the matrix
#   3  set the value of the inverse
#   4  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


#  This function receives the list object as its argument and calculates
#  its inverse. However, it first checks to see if the inverse has already 
#  been calculated. If so, it gets the inverse from the cache and skips 
#  the computation. Otherwise, it calculates the inverse of the data and 
# sets the value of the inverse in the cache via the setinv function.


cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
