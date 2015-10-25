## I write a function and make the funtion inverse with solve()
## In my function, I use the knowledge of lexical scoping
## My English is poor ,so I can only doing these works without getting more 
## information of the programming assignment

##A short comment describing this function
##amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##cacheSolve(amatrix)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
## cacheSolve(amatrix)
##[,1] [,2]
##[1,] -0.13333333  0.2
##[2,]  0.01010101  0.0
## amatrix$get()
##[,1] [,2]
##[1,]    0   99
##[2,]    5   66
##amatrix$getmean()
##[,1] [,2]
##[1,] -0.13333333  0.2
##[2,]  0.01010101  0.0


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean<- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
}


## All comment describing this function in the front of the first function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
