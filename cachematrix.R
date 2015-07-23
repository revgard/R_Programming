## Caching the Inverse of a Matrix
## This R Proggram is a set of functions that cache (stores numeric vector) and computes the 
## inverse of that matrix.

## The makeCacheMatrix function creates a special "vector" object 
## which allows the rest
## of the function to check and cache the inverse.

makeCacheMatrix <- function(mat = matrix()) {
                 inverse <- NULL
                     set <- function(x) {
                     mat <<- x;
                 inverse <<- NULL;
      }
                     
      get <- function() return(mat);
       setinv <- function(inv) inverse <<- inv;
       getinv <- function() return(inverse);
       
        return(list(set = set, get = get, 
              setinv = setinv, 
              getinv = getinv))
      
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
    
     inverse <- mat$getinv()
    
      if(!is.null(inverse)) {
    
      message("Checking for cached data")
   
      return(inverse)
    
     }
  
      data <- mat$get()
      invserse <- solve(data, ...)
       mat$setinv(inverse)
       return(inverse)
    
}
