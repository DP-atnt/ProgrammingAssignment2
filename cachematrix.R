## The following two functions will calculate the inverse of
## a matrix and store the value in memory. For the next calculation
## if the matrix exist, it retrieves the inverse from the cache,
## otherwise it will calculate it and store it.

## makeCacheMatrix checks if the inverse exist

# first run makeCacheMatrix with the matrix of interest and store
# its value in a object of any name

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The following retunrs the inverse of a matrix
## checking if the inverse exists first,

cacheSolve <- function(x, ...) {
            m <- x$getinv()
            
            if (!is.null(m)) {
                  message("getting cached inverse matrix")
                  return(m)
            }
            message("calculating inverse matrix")
            data <- x$get()
            
            m <- solve(data, ...)
            x$setinv(m)
            
            m
}

