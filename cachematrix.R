# Constructs a smart matrix that caches its inverse.
# The returning object is a list with 4 functions attached.
# Each function makes use of the scoping rules present in R.
# The underlying, "raw" matrix is held in the defining
# environment's closure
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(val) {
    x <<- val
    inv <<- NULL
  }

  get <- function() x

  setinv <- function(val) inv <<- val

  getinv <- function() inv

  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}

# Returns an inverse of the matrix held by a "smart" matrix object
# given by x.
# Any additional arguments are passed to the underlying call to solve.
# The function makes use of the interface constructed by the makeCahceMatrix
# function in the form of the functions attached to the resulting list.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()

  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()

  inv <- solve(data, ...)

  x$setinv(inv)

  inv
}

# TESTING:
# -------
# let's create an invertible matrix (as in help for ?solve) with:
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#
# Now:
# m <- makeCacheMatrix(hilbert(8))
# cacheSolve(m)
# cacheSolve(m) # again - and see the message being printed
# m <- makeCacheMatrix(hilbert(4))
# cacheSolve(m) # no message here again because we've changed the internal matrix
# cacheSolve(m) # again - and see the message being printed

