## For very big R objects and time-consuming computations, it might be useful to cache the results that are computed once
## and read them from the cache next rather than recomputing them again.
## The <<- operator can be used to assign a value to an object in an environment that is different from the current environment 
## and this can be utilised to achieve the caching of vectors. 
## Following is a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix" represented by a list of functions that: 

## 1. set the values of a matrix
## 2. get the values of a matrix
## 3. set the values of the inverse matrix
## 4. get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setminv <- function(minv) inv <<- minv
        getminv <- function() inv
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        
        inv <- x$getminv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setminv(inv)
        inv
}

## Test run:

# > m <- matrix(1:4,2,2)
# > x <- makeCacheMatrix(m)
# > x$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(x)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(x)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

