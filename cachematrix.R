## Put comments here that give an overall description of what your
## functions do

## The 2 functions first take a square, inversable matrix, define a vector in the environment containing the original matrix and its inverse. 
## The inverse is retrieved from cache (via lexical caching) when cacheSolve is called

## Write a short comment describing this function

## The function makeCacheMatrix initializes a vector containing values of the matrix and its NULL initial inverse. The vector is indexed so that
## elements can be retrieved.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The cacheSolve function retrieves from cache the inverse if it is not NULL. It also calculates the inverse matrix and stores it in x.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
