## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions, with elements of this list we can set new matrix, get existing matrix, 
##set inverse matrix and get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
  set<-function(y=matrix()){ ##describing function, that creates matrix and sets inverse to NULL
    x<<-y
    i<<-NULL
  }
  get<-function() x  ##function, with which we can see the input matrix
  setinverse<-function(inverse) i<<-inverse  ##sets inverse to desired 
  getinverse<-function() i ##returns inverse matrix
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse) ##creates a list of functions, with elements of this list we can
  ##set new matrix, get existing matrix, set inverse matrix and get inverse matrix
}


## This function checks, whether the inverse matrix was already cached and than depending on the result 
##caches the inverse and prints it or just prints it (with message "getting cached data!")

cacheSolve <- function(x, ...) {
 i<-x$getinverse() 
  if(!is.null(i)){ ## checks it inverse was already calculated
    message("Getting cached data!") ## it yes->message
    return(i) ## and inverse (function stops)
  }
  data<-x$get() ## if no, loading matrix to "data" variable 
  i<-solve(data,...)## calculating inverse
  x$setinverse(i) ## sets inverse matrix to calculated
  i ## Return a matrix that is the inverse of 'x'
}
