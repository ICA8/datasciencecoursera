#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Dynamic lookup using X
  get <- function() x
  #change mean to inverse
  #set the value of the vector
  setinverse <- function(inverse) m <<- inverse
  #get the value of the vector
  #dynamic lookup with subsetted list
  #Dynamic lookup using m
  getinverse <- function() m
  #dynamic lookup with subsetted list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cachesolve <- function(x, ...) {
  #get the value of the inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # inverse of a square matrix can be done with the solve function in R
  m <- solve(data, ...)
  #set the value of the inverse
  x$setinverse(m)
  m
}
