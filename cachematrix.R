## The following functions are used to calculate and then cache the inverse of a matrix so that the
## next time the inverse is used, it is taken out of the cache rather than computed repeatedly.

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) m <<- inverse
 getinverse <- function() m
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with the above matrix.
##However, it checks first to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse matrix and sets the value of the inverse in the cache via
##the setinverse function. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
