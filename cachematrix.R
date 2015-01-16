## Put comments here that give an overall description of what your
## functions do

## The function is to make a cache matrix with initial variable x is an empty matrix
## s also is assigned as an empty matrix
## setsolve is used to set the invertiable matrix into matrix "s"
## getsolve is used to return the value of invertiable matrix "s"

makeCacheMatrix <- function(x = matrix()) {
  s <- matrix()
  
  set <- function(y) {
    x <<- y
    s <<- matrix()
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Below function is used the check whether invertiable matrix is made, then and return the invertible
## if the invertible matrix is existing, matrix "s" is not empty (element s[1][1] is not an NA value
## it then prints "getting cached data" and return the value os invertible matrix "s" in its memory.
## If the invertible matrix is not existing, the function the compute its invertible matrix and return it

cacheSolve <- function(x, ...) {
          s <- x$getsolve()
  if(!is.na(s[1])) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
