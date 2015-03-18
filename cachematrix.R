## makeCacheMatrix(x)  create a special "matrix" object to store the inverse
## of a matrix. 
##cacheSolve(x) to calculate the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. 

## makeCacheMatrix(x)  makes a special matrix to store the inverse
## of the cashe matrix. It returns a list with functions:
##set(y)--(to set/reset the matrix),
##get()--(to retrive the matrix),
##setinverse(y)--(to set the inverse matrix),
##getinverse()--(to retrive the inverse matrix)
## 

makeCacheMatrix <- function(x = matrix()) {
  
  im <- NULL ##initiate the inverse to NULL
  
  ## set/reset the matrix to y; reset inverse matrix to NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  ## retrieve the matrix
  get <- function() x
  
  ## set the inverse of the matrix 
  setinverse <- function(inverse) im <<- inverse
  
  ## retrieve the inverse matrix
  getinverse <- function() im
  
  ## return a list with all these functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve(x) to calculate the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  ##if inverse matrix is already stored, retrieve the inverse
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  ## otherwise compute the inverse of the matrix x and set it to store
  data <- x$get()
  im <- solve(data)
  x$setinverse(im)
  im
}
