## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse matrix.
## (c) Assignment2 description (Above and below comments)

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This special "vector" is really a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## 1) set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## 2) get the value of the matrix
  get <- function() x
  
  ## 3) set the value of the inverse matrix
  setInverse <- function(solve) m <<- solve
  
  ## 4) get the value of the inverse matrix 
  getInverse <- function() m
  
  ## return a list where each element is a function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  
        m <- x$getInverse()
        
        ##  Check if the inverse has already been calculated
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        
        ##  If not calculate the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

## Easy Inverse Test (c) Al Warren

## m <- matrix(c(-1, -2, 1, 1), 2,2)
## x <- makeCacheMatrix(m)
## x$get()
##      [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1

## inv <- cacheSolve(x)
## inv
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1

## inv <- cacheSolve(x)
## getting cached data
## inv
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1

