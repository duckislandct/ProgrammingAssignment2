## makeCacheMatrix and cacheSolve cache a time-consuming
## matrix inverse computation.
## 
## Given x, an invertible matrix, makeCacheMatrix creates a list
## containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the matrix inverse
## 4) get the value of the matrix inverse.
##
## cacheinv calculates the inverse of matrix created
## by makeCacheMatrix but checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache otherwise it sets the value of 
## the inverse using the setinverse function.
##
## Example:
## m <- matrix(sample(10000,1000), nrow = 10, ncol = 10)
## v <- makeCacheMatrix(m)
## cacheSolve(v) # First time, calculated the inverse.
## cacheSolve(v) # Second time, retreives from cache.


## makeCacheMatrix creates a list containing a function
## to get & set a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve calculates or retrieves from cache
## the inverse of matrix created by makeCacheMatrix
cacheSolve <- function(x,...) {
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

