## This two functions allow to get the inverse of a matrix,
## provided it is square and it has an inverse, and repeat
## the operation any time without additional calculations

## The first function allows to create a matrix and define
## "set" and "get" to use them later

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve<-function(solve)s<<-solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function uses the matrix created by the first function
## and calculates its inverse. If the inverse had already been
## calculated, it can retrieve it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
}
