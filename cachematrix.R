## Cache an inversion of a matrix

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(mat, ...) {
  inv <- mat$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data)
  inv$setmean(inv)
  inv
}
