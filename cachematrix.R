## The code implement allows to compute the invert of a square matrix and 
## write it to a cache

## The function "makeCacheMatrix" creates a special vector 
## which allows to work with a cached solution when the second
## function is implemented.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <-- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function computes an inverse matrix of a square matrix and verifies
## if that computation has already been implemented. If so, if does not
## computes inverse matrix again but rather takes the solution from the vector
## created with the function above.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

## Example of code implementation

z <- matrix(c(1,2,3,4),2)

x <- makeCacheMatrix(z)
cacheSolve(x)
