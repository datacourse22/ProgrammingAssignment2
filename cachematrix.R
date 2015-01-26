## This R script allows you to save/cache an already-computed 
## inverse of a matrix to save on computation time. Also allows
## you to retrieve a cached inverse

## The following function gives handles to set or get the
## the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  getenv<- function() environment()
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve,
       getenv = getenv)
}


## The following function checks to see if the inverse of a matrix
## has already been computed. If it has, it returns the inverse
## If it hasn't, it computes it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  cur_x <- x$get()
  if( (!is.null(s)) && (cur_x==parent.env(x$getenv())$x) ) {
    message("getting cached inverse")
    return(s)
  }
  s <- solve(cur_x, ...)
  if(cur_x != parent.env(x$getenv())$x) {
    x$set(cur_x)
  }
  x$setsolve(s)
  s
}
