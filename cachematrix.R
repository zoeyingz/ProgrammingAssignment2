## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# Make a local variable
  m <- NULL
# Pass the y from this function to the X, x from calling function, 
# do not make a new variable, and set m as null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
# Get the matrix x
  get <- function() x
# Set the inverse, set the inverse to m 
  setinverse <- function(inverse) m <<- inverse
# Get inverse, return inverse
  getinverse <- function() m
# This function return a list of functions as result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
# X is a makeCacheMatrix
# Call getinverse function, find out if the inverse of x has been calculated before
# give the result to m
  m <- x$getinverse()
# If m is not null, which means the inverse has been calculted before, 
#  and has been stored in m, we just need to return m. This is the inverse of x 
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
# When m is null, function process to next step
# Get the matrix from x
  data <- x$get()
# Calcultae the inverse of metrix
  m <- solve(data, ...)
# Set the inverse to m, the inverse will stored in x
  x$setinverse(m)
# Return n, which it the inverse
  m
}


