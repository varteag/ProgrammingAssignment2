makeCacheMatrix <- function(x = matrix()) {   #define function and its arguments
  i <- NULL                                   #clears the old inverse 
  set <- function(y) {                        # Set the value of the matrix
    x <<- y                                   #assign Y to X in the parent environment named by the object X.
    i <<- NULL                                # clears the old inverse if Set function is called. (set:is a setter)
  }
  get <- function() x                         # get the value of the matrix(get function is a getter)
  setinverse <- function(inverse) i <<- inverse  # defines the value (setter) of inverse of the matrix. the input is assign to i in the parent environment
  getinverse <- function() i                     # getter for the inverse of the matrix
  list(set = set, get = get,                     # creates a list where every element is named
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {                 # define cacheSolve fucntion
      i <- x$getinverse()                        #gets the cached value of getinverse() and assign it to i
    if(!is.null(i)) {                            # if the inverse was calculated before, it can be returned
      message("getting cached data")            
      return(i)
    }
    data <- x$get()                              # if not, then is calculated: getting the value of the matrix          
    i <- solve(data,...)                         #then computing the inverse 
    x$setinverse(i)                              # and storing the result in the cache
    i                                           #prints the inverse  
}
