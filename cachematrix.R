#######################################################################
## Function makeCacheMatrix() takes a matrix as an input, and returns a list of 4 functions.
## For example, test1 <- makeCacheMatrix(matrix(rnorm(100),10,10)) is a list of 4 functions where:
##    - "test1$set(...)" will allow us to set the matrix for test1 (here matrix(rnorm(100),10,10))
##    - "test1$get()" will allow us to retrieve the matrix "matrix(rnorm(100),10,10)"
##    - "test1$setinv(...)" will allow us to set the inverse of the matrix specified for test1 (see cacheSolve())
##    - "test1$getinv()" will allow us to retrieve the inverse of the matrix specified for test1
##
## Function cacheSolve() takes the list described above as an input and 
##    1/ checks if the inverse of the matrix has already been calculated
##    2/ computes the inverse if had not been calculated 
##    3/ returns the inverse of the matrix
##
## For example, cacheSolve(test1) will return the inverse of the "matrix(rnorm(100),10,10)" matrix
## assigned to test1. You can check the inversion of the matrix in test1 with the following test :
## > inv_test1 <- cacheSolve(test1)
## > inv_test1 %*% test1$get()
#######################################################################

## makeCacheMatrix() takes a matrix as argument and transforms the matrix into a list
## with the 4 following functions:
## 1/ set()
## 2/ get()
## 3/ set_inv()
## 4/ get_inv()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(compute_inv) inv <<- compute_inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes the list described above as argument and:
## 1/ checks if the inverse has already been cached
## 2/ computes the inverse if had not been calculated yet
## 3/ returns the inverse

cacheSolve <- function(x, ...) {
  my_inv <- x$getinv()
  if(!is.null(my_inv)) {
    message("getting cached data")
    return(my_inv)
  }
  my_matrix <- x$get()
  my_inv <- solve(my_matrix, ...)
  x$setinv(my_inv)
  my_inv
  ## Return a matrix that is the inverse of 'x'
  
}
