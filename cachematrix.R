## Put comments here that give an overall description of what your
## functions do the cache of the inverse of a matrix

## makeCacheMatrix creates a special matrix which may cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set<-function(y){
    x<<-y
    i <<- NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## inverses the special matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
