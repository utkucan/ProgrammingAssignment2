## There are only two function. first you may initalize a matrix with makeCacheMatrix function. It stores the matrix data and inverse of the data
## when you call cacheSolve function It checks the object which have already created with makeCacheMatrix, if it includes inverse data it returns that value 
## if the inverse data is NULL it computes its value.

#initilizes CacheMatrix instance
makeCacheMatrix <- function(x = matrix()) {
 I <- NULL
 set <- function(y){
   x <<- y
   I <<- NULL
 }
 get <- function() x
 setInverse <- function(solve) I <<- solve
 getInverse <- function() I
 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## tries to compute inverse of matrices created with makeCacheMatrix function. 
## if the instance has inverse value then returns it else it computes and sets it with setInverse
cacheSolve <- function(x, ...) {
 I <- x$getInverse()
 if(!is.null(I)){
  message("GET FROM CACHE!")
  return(I)
 }
 message("CALCULATED")
 data <- x$get()
 I <- solve(data, ...)
 x$setInverse(I)
 I
}

##TEST##
##initialize object
M <- makeCacheMatrix();
## create a matrix 
test <- matrix(1:4, 2, 2);
## set the matrix
M$set(test);
## solve it
cacheSolve(M);
## get solution
M$getInverse();
## solve againt (it returns from cache!)
cacheSolve(M);
## set as NULL
M$setInverse(NULL);
## solve it again (computes again) 
cacheSolve(M);

