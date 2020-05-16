## Put comments here that give an overall description of what your
## functions do

##create "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  temp <- NULL
  set <- function(y){
    x <<- y
    temp <<- NULL
  }
  get <- function()x
  set_inver <- function(inver) temp <<- inver
  get_inver <- function() temp
  list(set=set, get=get, set_inver=set_inver, get_inver=get_inver)
}


## computes the incerse of "matrix" returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  temp <- x$get_inver()
  if(!is.null(temp)){
    message("getting cached data")
    return(temp)
  }
  data <- x$get()
  temp <- solve(data,...)
  x$set_inver(temp)
  ## Return a matrix that is the inverse of 'x'
  temp
}
