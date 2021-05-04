## Put comments here that give an overall description of what your
## functions do

## handle a cache for matrices and their inverses
makeCacheMatrix <- function(x = matrix()) {
  ## initialize cache
  cache <- NULL
  
  ## define setter function
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ## define getter function
  get <- function() x
  
  ## define setCache() function
  setCache <- function(inverse) cache <<- inverse
    
  ## define getCache() function
  getCache <- function() cache
  
  ## define public list of functions
  list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## compute the inverse of x if x is not cached, else return cached value for inverse of x
cacheSolve <- function(x, ...) {
  ## check for cached value
  m <- x$getCache()
  
  ## return cached value if non-null value found
  if (!is.null(m))
    return(m)
  
  ## no cached element found, calculation required
  m <- solve(x$get()) %*% x$get
  
  ## setCache for future use
  x$setCache(m)
  
  return(m)
}
