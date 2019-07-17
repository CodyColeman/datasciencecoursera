## These functions will be created to take the inverse of a 
## matrix, cache the matrix, and call the matrix from the cache
## if the inverse of the original matrix has already been found. 

## This function will

# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse of the matrix
# 4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function gives the inverse of the matrix. First, it checks
## to see if the inverse is already been taken. If it has, then it pulls
## the inverse from the cache, but if not, it takes the inverse and sets
## the result in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
