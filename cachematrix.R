## Functions work together to allow setting and getting of a matrix, together with 
# calculation and caching of the matrix inverse

## makeCacheMatrix returns a list consisting of functions to set and get a matrix
## and an inverse to and from cache.
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## cacheSolve takes a makeCacheMatrix list as input. It retrieves the invesre of the 
## cached matrix if it already exists in the cache, or else it calculates the inverse
## stores in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

