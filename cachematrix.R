## The functions below create and enable the use of a special "matrix". This special matrix object allows
## for the inverse of the matrix to be cached. The first function, makeCacheMatrix, creates the special matrix object.
## the second function, cacheSolve, determines if the inverse has been cached and calculates it if it has not been.

##The makeCacheMatrix function creates a special "matrix" object, which is really a list containing four functions:
## (1) a function to set the matrix, (2) a function to get the matrix, (3) a function to set the inverse, and 
## (4) a function to get the inverse.
makeCacheMatrix <- function (x=numeric()){
  inv <- NULL             ## initializes the inverse to NULL
  set <- function(y){     ## the set function sets the value of the matrix, and sets the inverse back to NULL
    x <<- y
    inv <<- NULL
  }
  get <- function() x     ## the get function returns the value of the
  setinverse <- function(inverse) inv <<- inverse   ## the setinverse function takes sets the inverse of the matrix
  getinverse <- function () inv  ## the getinverse function returns the cached inverse or NULL if nothing has been cached          
  list (set = set, get = get,    ## creates the special "matrix" object, which is a list of four functions
        setinverse = setinverse, 
        getinverse = getinverse)
}

## The cacheSolve function takes a special "matrix" object, which was created using the makeCacheMatrix function, and 
## returns its inverse. It first checks to see if the object has the inverse cached, if so, it returns the cached inverse matrix.
## If the inverse has not been cached before calling cacheSolve, then in addition to calculating and returning the inverse matrix,
## it also caches it in the special "matrix".
cacheSolve <- function(x){
  inv <- x$getinverse()     ## gets the cached inverse, if one exists, from the matrix object
  if(!is.null(inv)){        ## if the inverse exists (is not NULL) then a message is printed and the matrix is returned
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()         ## if the inverse was not cached, it is calculated using the solve function
  inv <- solve(data)
  x$setinverse(inv)       ## the calculated inverse is stored in the matrix object and returned
  inv
}
