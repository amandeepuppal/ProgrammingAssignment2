## makeCacheMatrix and cacheSolve helps in avoiding the costly computation of the 
## inverse by caching the matrix and its inverse as matrix inverstion is a costly operation

## makeCacheMatrix create a special matrix object that can cache its inverse. 
## It also performs four other functions:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve function returns the inverse of the matrix
## If the inverse is already calculated, it returns the value from the cache
## If the inverse is not already available, it calculates the inverse, set the inverse in the cache 
## and returns the inverse

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
