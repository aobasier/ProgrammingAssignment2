
####### 1. `makeCacheMatrix` function creates a special "matrix" object.

makeCacheMatrix <- function(x = matrix()) { .    ## function here creates a special "matrix" that can cache its inverse
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


#### 2.  `cacheSolve`: This function computes the inverse of the "matrix" created by `makeCacheMatrix` above. 
#### If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {      ##function here return the inverse of the creted matrix
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

## test the function using the R code

> my_matrix <- makeCacheMatrix(matrix(4:7, 2, 2))
> my_matrix$get()
[,1] [,2]
[1,]    4    6
[2,]    5    7
> cacheSolve(my_matrix)
[,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2
