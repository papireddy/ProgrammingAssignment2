## These functions are to cache the inverse of a matrix. The first function holds the ## matrix to be cached, next one calculates the inverse and calls the first one to   ## hold the matrix. 

## This function holds the matrix for which the inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
      inverse<-NULL
      set<- function(y){
            x<<-y
            inverse<<-NULL
      }
      get<-function() x
      setInverse<- function(val) inverse<<-val
      getInverse<- function() inverse
      list(set = set, get = get,
           setinverse = setInverse,
           getinverse = getInverse)
}


## This function first searches if this matrix has the inverse cached, if not it ## ## ## calculates and then stores it in the cache.

cacheSolve <- function(x, ...) {
      inverse<- x$getinverse()
      
       if(!is.null(inverse)){
             print("Cached already! Inverse is:")
             return(inverse)
       }
      matrix<-x$get()
      inverse<-solve(matrix)
      x$setinverse(inverse)
      inverse
}
