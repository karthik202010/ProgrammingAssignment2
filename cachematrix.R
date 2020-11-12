## Put comments here that give an overall description of what your
## functions do

#### The objective of this assignment is to write a pair of 
#### functions that cache the inverse of a matrix
#### If a new matrix is provided, it computes the inverse, caches it
#### and returns the inverse of the matrix
#### If the matrix inverse is already computed, 
#### then it returns the inverse matrix from the cache

## Write a short comment describing this function

#### makeCacheMatrix function takes a matrix as input
#### initialized the inverse matrix to NULL and stores both input
#### and inverse matrix to cache
#### it then gets the input matrix from cache 
#### stores the inverse of the matrix to cache
#### fetches the inverse of the matrix from cache
#### finally returns all functions in a list so that it can be 
#### leveraged by other functions like cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv_m <<- solve
    getinv <- function() inv_m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#### cacheSolve function takes a square matrix as input and 
#### fetches the inverse of the matrix from cache
#### if the inverse of the matrix is not null then it is returned
#### with message "getting cached data"
#### Otherwise, it fetches the input matrix
#### computes the inverse of the matrix
#### stores it in cache and also returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getinv()
    if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
    }
    data <- x$get()
    inv_m <- solve(data, ...)
    x$setinv(inv_m)
    inv_m
}
