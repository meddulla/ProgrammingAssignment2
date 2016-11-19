## Cache a matrix's inverse

## makeCacheMatrix returns an object which can cache its inverse.
## cacheSolve returns the matrix inverse from cache if available or calculates it, caches it and then returns it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() { # get original (non-inverted) matrix
    x
  }
  reset <- function(y) { # reset cache
    x <<- y
    inverse <<- NULL
  }
  saveInverted <- function(inverse) { # save inverted matrix in cache
    inv <<- inverse
  }
  getInverted <- function() { # get inverted matrix from cache
    inv
  }
  list(get = get,
       reset=reset,
       saveInverted = saveInverted,
       getInverted = getInverted)
}


## Gets inverted matrix from cache or calculates it

cacheSolve <- function(x, ...) {
  # get the cached inverse if it exists
  inv <- x$getInverted()

  # if cache hit, return
  if(!is.null(inv)) {
    message("cache hit")
    return(inv)
  }

  # cache miss, calculate matrix inverse and cache it
  message("cache miss")
  matr <- x$get()
  inverse <- solve(matr, ...)
  x$saveInverted(inverse)

  return(inverse)
}
