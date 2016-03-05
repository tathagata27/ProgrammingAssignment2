## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly
## I have written a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
      }
      get <- function() x
	setcache <- function(cacheSolve) m <<- cacheSolve
	getcache <- function() m
	list( set = set, get = get, getcache = getcache, setcache = setcache)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}

