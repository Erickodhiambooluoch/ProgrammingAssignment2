
#matrices for testing R functions
#makeCacheMatrix and CacheSolve
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
n1 <- m1 %% n1

#M1 and n1 are matrix multiply. m1 %% n1
# is the 2 row by 2 column identity matrix
I2 <- matrix(c(1, 0, 0,1), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2  )
#checks to see if n1 and m1 are giving identity matrix
m1 %*% n1
#checks n1 and m1
n1 %*% m1
solve(m1)
solve(n1)
#in the Make cache matrix, the function isbasically to set the inverse a a matrix e.g
#from the above M1
makeCacheMatrix <- function(x = m1){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#this function is to basically to retrive the cached inverse of M1, which ia already
#stored in a temporary cache 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#if you want to get the Matrix, you simply call them using the following
myMatrix_object <- makeCacheMatrix(m1)
myMatrix_object$get()
myMatrix_object$getinverse()
myMatrix_object$set(m1)
cacheSolve(myMatrix_object)
myMatrix_object$getinverse()
