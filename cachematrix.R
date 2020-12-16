## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
# makeCacheMatrix is a special 'matrix' object that can cache its inverse 
# I set x as a matrix then I set the value m as a null - 
# to indicate the what the function is supposed to do, I set mean to inverse

makeCacheMatrix <- function(x = matrix()) {
              m<- NULL
              set<- function(y){
                x<<-y
                m<<-NULL
              }
              get<-function() x
              setinverse <- function(inverse) m <<- inverse
              getinverse <- function() m
              list(set=set, get=get,
                   setinverse = setinverse, 
                   getinverse = getinverse)
}


## Write a short comment describing this function
# with this function we can get the inverse of the "special" matrix that was returned by makeCacheMatrix,
#if it hasn't been already created
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#Example
matrix_1<- makeCacheMatrix(matrix(1:4, 2, 2))
matrix_1$get()
matrix_1$getinverse()
cacheSolve(matrix_1)
matrix_1$getinverse()
