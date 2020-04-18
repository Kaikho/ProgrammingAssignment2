makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve=getsolve)
}

cachesolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

#Checking the script
Example1 <- makeCacheMatrix()
y <- matrix(c(4,2,7,6),nrow=2,ncol=2)
Example1$set(y)
Example1$get()
Example1$setsolve()
Example1$getsolve()

cachesolve(Example1)

