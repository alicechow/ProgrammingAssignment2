## Matrix inversion is usually a costly computation.  There may be some benefit to caching the value of the inverse of a matrix rather than computing the value repeatedly.

## makeCacheMatrix creates a special "matrix" with the function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
      inver <-NULL
      set <- function (y){
            x<<- y
            inver<<- NULL
        
      }
      get <- function () X
      setinverse <- function(inverse) inver<<- inverse
      getinverse <- function () inver
      list (set=set, get = get, setnverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the inverse of the matrix from makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if (!is.null(m)) {
              message ("getting cached data")
              return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}
