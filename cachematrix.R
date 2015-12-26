## This program will find the inverse of a square, invertible matrix.
## The solution is stored in cached data so if the user attempts to
## solve the inverse again on the same dataset, it will retrieve the
## cached data rather than recalculating the inverse 
## to save system resources

#Sample Input of function
#> a <- makeCacheMatrix(c(1,0,5,2,1,6,3,4,0))
#> cacheSolve(a)

## This function stores a list of functions. Passing data to makeCacheMatrix
## creates a square matrix of the data passed similar to the set function
## defined within. The get function retrieves a stored matrix. The setinverse
## function stores an inverse matrix (does not solve for the inverse). Finally,
## the getinverse function retrieves the stored inverse matrix (again, it
## does not solve for the inverse).
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #convert input into square matrix; note: matrix built columnwise
    x <- matrix(x,sqrt(length(x)),sqrt(length(x)))
    set <- function(y = matrix()) {
        #convert input into square matrix; note: matrix built columnwise
        x <<- matrix(y,sqrt(length(y)),sqrt(length(y)))
        m <<- NULL
    }
    get <- function() x #return matrix x
    setinverse <- function(inverse) m <<- inverse #doesn't calculate inverse; stores inverse
    getinverse <- function() m #retrieve stored inverse matrix
    #listing of functions to establish object with access to all functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    #if the user is requesting the previously calcualted inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #otherwise we must get the new data and solve for the inverse
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
