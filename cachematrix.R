## The first function creates a list that stores four functions, used to store a matrix and cached inverse
## The second function pulls the cached inverse if available from the list, if available, 
## and otherwise creates the inverse and stores it in the list

## This function creates a list of four functions. The first function sets the matrix, storing it in that environment
## The second function gets the matrix if called from that environment
## The third function sets a cached inverse
## The third function pulls a cached inverse matrix from that environment

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function creates an inverse matrix given a list created in makeCacheMatrix
## This function first checks if there is a cached inverse in the function environment
## It pulls that cached inverse if available
## If there is not cached inverse, it creates an inverse and caches the inverse in the environment.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
