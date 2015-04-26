## First type a<-makeCacheMatrix(x), this stores the functions to memory.
## Next run cacheSolve(x). The first time this is run, the inverse will be calculated from scratch.
## Running cacheSolve(x) again will return message "getting cached data", since the result can be obtained from cached data.
## If the matrix is changed, makeCacheMatrix will be rerun to clear m.

## Creates a list of 4 functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## First input the matrix 'x' into function makeCacheMatrix and assign the output to a variable 'a',
## then retrieve the inverse if already calculated. If it does not exist (m=NULL),
## function proceeds to calculate the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (identical(x,a$get())==F) a<<-makeCacheMatrix(x) #Reruns makeCacheMatrix(x) if x changes.
  m <- a$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- a$get()
  m <- solve(data, ...)
  a$setsolve(m)
  m
}
