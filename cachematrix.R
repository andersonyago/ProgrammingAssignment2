#Creating matrix object #-----
makeCacheMatrix <- function(x = matrix()) {       #calling the function to input the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {x}                           #matrix output
  
  setInverse <- function(inverse) {m <<- inverse}  
    
  getInverse <- function() {m}                    #NULL inverse matrix output
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Cache Solve matrix #---

cacheSolve <- function(x, ...) {                  #calling the function using the created matrix (x)
  m <- x$getInverse()
  if(!is.null(m)) {                               #checking if the inverse has already been calculated
    message("getting cached data") 
    return(m)                                     #if not, returning the cached matrix
  }
  data <- x$get()                                 #calling the matrix
  m <- solve(data, ...)                           #computing the inverse of the matrix using solve function
  x$setInverse(m)
  m                                               #output the invertible matrix 
}
