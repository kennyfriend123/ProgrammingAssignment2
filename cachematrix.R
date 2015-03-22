## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a "matrix", 
## which is really a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL 
  set <- function(y) { 
        x <<- y
        inver <<- NULL
  } 
  get <- function() x 
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) 
}


## Write a short comment describing this function
## Function cacheSolve first check the if the inverse has been already 
## calculated
## if so, then cache the value. otherwise, it will calculate and cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse() 
  if(!is.null(inver)) { 
          message("Getting cached data") 
          return(inver) 
  } 
  data <- x$get() 
  inver <- solve(data, ...) 
  x$setinverse(inver) 
  inver 
}
