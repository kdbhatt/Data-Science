## Course : Coursera - R Programming
## Programming Assignment 2
## Author: Krishan Bhatt


## Descriptions: 

#The first function, makeCacheMatrix function creates a matrix 
#and create some of the special function to

# Set the value of the marix.
# Get the value of the matrix.
# Set the value of the inverse of the martix.
# Get the value of the inverse of the martix.


makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matInv <<- inverse
  getinverse <- function() matInv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#cacheSolve function returns the inverse of the matrix. If the inverse has been
#already computed, it just ges the result. Else it computes the inverse and place 
#the value in the cache.

cacheSolve <- function(x, ...) {
  matInv <- x$getinverse()
  if(!is.null(matInv)) {
    message("I am from cache.")
    return(matInv)
  }
  data <- x$get()
  matInv <- solve(data)
  x$setinverse(matInv)
  matInv
}


#  TEST CASE

#met is a 2X2 matrix 
#met <- makeCacheMatrix(rbind(c(2, 4), c(5, 1)))

#get matrix mat
# >met$get()
#      [,1] [,2]
#[1,]    2    4
#[2,]    5    1

#value has been calculated in first run
# >cacheSolve(met)
#        [,1]       [,2]
#[1,] -0.05555556  0.2222222
#[2,]  0.27777778 -0.1111111

#Just retrived value from cache
# >cacheSolve(met)
#I am from cache.
#            [,1]       [,2]
#[1,] -0.05555556  0.2222222
#[2,]  0.27777778 -0.1111111

#get inverse of a matrix without usinf cache
# >met$getinverse()
#       [,1]       [,2]
#[1,] -0.05555556  0.2222222
#[2,]  0.27777778 -0.1111111








#this is test