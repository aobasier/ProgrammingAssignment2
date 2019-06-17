makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## test the function

> my_matrix <- makeCacheMatrix(matrix(4:7, 2, 2))
> my_matrix$get()
[,1] [,2]
[1,]    4    6
[2,]    5    7
> cacheSolve(my_matrix)
[,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2
