## Create a special Matrix and cache it.

## getting and setting cache.

makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Checking matrix inverse has been cached, if not create one and cache it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  c <- x$getInv()
  if(!is.null(c)) { 
    message('Getting from cache')
    return(c) 
  }
  data <- x$get()
  message('Calcuating inverse matrix')
  c <- solve(data)
  x$setInv(c)
  c
}
