## Put comments here that give an overall description of what your
## functions do

# Reading here https://share.coursera.org/wiki/index.php/DataSciSpec:Functional_Progr_in_R

## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

a <- makeCacheMatrix(matrix(1:4,2))
a$get()
a$getInverse()

a$set(matrix(5:8,2))
a$get()

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has 
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    
}
cacheSolve(a)
cacheSolve(a)
a$getInverse()
b <- a$getInverse()
a$get() %*% b
