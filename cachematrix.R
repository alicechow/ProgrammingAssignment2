## Matrix inversion is usually a costly computation.  
## There may be some benefit to caching the value of the inverse of a matrix rather than computing its value repeatedly.

## makeCacheMatrix creates a matrix and contains 4 sub-functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
      invert <-NULL
      set <- function (y){
            x<<- y
            invert <<- NULL
        
      }
      get <- function () X
      setinverse <- function(inverse) invert <<- inverse
      getinverse <- function () invert
      list (set=set, get = get, setnverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the inverse of the matrix from makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        invert <- x$getinverse()
        if (!is.null(invert)) {
              message ("getting cached data")
              return(invert)
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setinverse(invert)
        invert
}
