## Put comments here that give an overall description of what your
## functions do

      ## The first function makeCacheMatrix() creates an R object that stores a
      ## matrix and its inverse. The second function cacheSolve() requires an
      ## argument that is returned by makeCacheMatrix() in order to retrieve the
      ## inverse from the cached value that it is stored in the makeCacheMatrix()
      ## object's environment.

## Write a short comment describing this function

      ## makeCacheMatrix() builds a set of functions (set(), get(). setsolve(),
      ## getsolve()) and returns the functions within a list to the parent
      ## environment.
      ##
      ## Notice that x has a default value as an empty matrix because
      ## otherwise data <- x$get() would generate an error.
      ##
      ## Within set() we use the <<- form of the assignment operator because we
      ## need to assign values to objects in the parent environment. The same
      ## holds for setInverse().
      ##
      ## Finally, notice that get() and getInverse() retrieve values from the
      ## parent environment thanks to lexical scoping.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function

      ## At first, the function attempts to retrieve an inverse from the object
      ## passed in as the argument and it checks whether the result is NULL.
      ##
      ## If not, it returns the value to the parent environment.
      ##
      ## If it is NULL, it computes the inverse and returns it to the
      ## parent environment.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}
