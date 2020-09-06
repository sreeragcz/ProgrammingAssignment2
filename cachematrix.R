#makes a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(z) {
    x <<- z
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#This function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve
#the inverse from the cache.

CacheSolve <- function(x, ...) {
  
  #return a matrix that is inverse of x
  
  n <- x$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}