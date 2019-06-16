
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  inv <- function(solve)i <<- solve
  getinv <- function()i
  list(set = set, get = get, inv = inv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)){
          return(i)
        }
        m <- x$get()
        i <- solve(m)
        return(i)
}

