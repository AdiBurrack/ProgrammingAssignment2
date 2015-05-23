#The following functions calculates the inverse of a matrix. 
#It tries to decrease computation time by checking if the inverse has allredy been calculated,
#if so it skips the computation time.


makeCacheMatrix <- function(x = matrix()) {
  #return set/get functions for cached matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvM <- function(invM) m <<- invM
  getinvM <- function() m
  list(set = set, get = get,
       setinvM = setinvM,
       getinvM = getinvM)

}


# returns the inv of a matrix - using cache data to shorten the runtime.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvM()
  if(!is.null(m)) { #if this is not the first time we calculate the inv
    message("getting cached data")
    return(m)
  } #if this is the first time we calculate the inv 
  data <- x$get()
  m <- solve(data, ...)
  x$setinvM(m)
  m
}
