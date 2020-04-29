## makeCacheMatrix and cacheSolve calculate inverse of given matrix, cache it and retrieve it when asked

## makeCacheMatrix sets up the special vector (list) of functions required to cache the matrix, initiate the
## solve and get the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve retrieves the inverted matrix if it had been inverted previously or solves for the inverse
## it also has the logic to return the cached matrix built in

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
