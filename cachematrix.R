## Set of functions that saves computation time by caching the inverse of your 
## matrix in case the content of the matrix is not changing

## takes a matrix as argument initializes the inverse, caches it in setinverse
## returns a list of functions that allows to access and set the inverse or
## the data of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse to Null
  inv <- NULL
  #set function to cache the matrix
  set <- function(y){
    inv <- NULL
    x <<- y
  }
  #functions to set and access the matrix, or its inverse
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## takes a cached matrix as argument computes the inverse if not already cached
## and returns it, otherwise it 

cacheSolve <- function(x, ...) {
  #get  the inverse if not null return the inverse to save computation time
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if inverse is not already cached, computes it, returns it and caches it, veni vedi vici
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
