# makeCacheMatrix <- function(x = matrix())
# Create an object with 2 variables and 4 functions
#   variables
#     x is a matrix initialized with the arguement
#     inverse is initialized to null and available to store the inverse matrix
#   functions
#     SetMatrix <- function(y)
#       Caches the value for a new matrix and reinitialize inverse
#     GetMatrix <- function()
#       Returns the current value of the matrix
#     SetInverse <- function(StoreInverse)
#       Caches the argument as the inverse
#     GetInverse <- function()
#       Returns the Cached value of inverse
makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  SetMatrix <- function(y){
    x <<- y
    inverse <<- NULL
  }
  GetMatrix <- function() x
  SetInverse <- function(StoreInverse) inverse <<- StoreInverse
  GetInverse <- function() inverse
  list( SetMatrix = SetMatrix, GetMatrix = GetMatrix,
        SetInverse = SetInverse, GetInverse = GetInverse)
}

# cacheSolve <- function(x, ...)
#   Returns the cached value of inverse from the object x using the makeCacheMatrix functions
#   If no value is currently cached, the inverse will be calculated and cached prior to return

cacheSolve <- function(x, ...){
  InverseMatrix <- x$GetInverse()
  if(!is.null(InverseMatrix)){
    message("getting cached data")
    return(InverseMatrix)
  }
  MyMatrix <- x$GetMatrix()
  InverseMatrix <- solve(MyMatrix)
  x$SetInverse(InverseMatrix)
  InverseMatrix
}