# makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix using the solve function.
## If the matrix inverse has already been calculated, it will
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
  # m is the inverse of matrix x
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}

cacheSolve <- function(x=matrix(), ...) {
## Returns matrix m that is the inverse of 'x'
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

