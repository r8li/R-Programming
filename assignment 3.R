## JHU R PROGRAMMING ASSIGNMENT 3 (PEER REVIEWED)
## PART I CREATE makeCacheMatrix FUNCTION 
## THIS FUNCTION WILL CREATE SPECIAL MARTIX BY READED MARTIX OBJECTS
## AND SAVE MATRIX AND ITS INVERSE 

makeCacheMatrix <- function(x = matrix()){
  i <- NULL 

  # read matrix and save it into object

  set <- function(y){
    x <<- y
    i <<- NULL
  }
  #return the matrix from object

  get <- function() x

  #read a matrix and save into cache

  setinverse <- function(inverse) i <<- inverse

  #return the inverse of matrix from saved cache
 
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
} 


## PART II CREATE cacheSolve FUNCTION
## THIS FUCNTION COMPUTES THE INVERSE OF THE SPECIAL MARTIX RETURNED BY makeCacheSolve
## IF THE INVERSE HAS ALREADY BEEN CALCULATED, THEN THE CAHSESOVE SHOULD RETRIEVE
## THE INVERSE FROM TEH CACHE.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
  