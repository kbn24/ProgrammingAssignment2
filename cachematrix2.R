## This two functions allow to get the inverse of a matrix,
## provided it is square and it has an inverse, and repeat
## the operation any time without additional calculations


## The first function allows to create a matrix and define
## "set" and "get" to use them later

makeCacheMatrix <- function(x = matrix()) {  
  s <- NULL
  
  ## The above lines initialize two objects within the function 
  ## environment: "x", a matrix which is the function 
  ## argument, and "s", an object that will be used later
  ## to contain the inverse of the matrix
  
  set <- function(y) {
    x <<- y
    s <<- NULL
    
  ## With SET we assign the input argument to X, in the 
  ## parent environment, and we make sure that every time
  ## the function runs any previous value of S is cleared
  ## from memory
    
  }
  get <- function() x
  setsolve<-function(solve) s <<- solve
  getsolve <- function() s
  
  ## With the three above lines we can retrieve the value of
  ## X from the parent environment, redefine S as the result
  ## of every run of the program and access the value of S that
  ## we have just established
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  ## This code assigns each of these functions as an element
  ## within a list(), and returns it to the parent environment.
  ## It returns a fully formed object of type makeVector() to be
  ## used by downstream R code. Also, it gives a name to each
  ## element in the list, allowing us to access it with the $
  ## operator
}




## The second function uses the matrix created by the first function
## and calculates its inverse. If the inverse had already been
## calculated, it can retrieve it from the cache

cacheSolve <- function(x, ...) {
  ## The function uses X as argument, just as makeCacheMatrix
  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
    
  ## We retrieve the cached value of S is available. If not, it
  ## goes to the next lines
    
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
  
  ## These last lines allow us to calculate the inverse (S) of the
  ## matrix for the first time, i.e., when there are no cached data.
  ## The program stores the value of S so that it can retrieve it
  ## from there in the future
}
