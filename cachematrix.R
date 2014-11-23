## The function 'makeCacheMatrix' contains a list of 4 functions:
## 'set' to set the value of a matrix
## 'get' to get the value of a matrix
## 'setinverse' to set the inverse of a matrix
## 'getinverse' to get the inverse of a matrix

## the variable 'i' is used to carry the inverse of the matrix
## the '<<-' operator ensures that a search is made through the
## parent environment for 'i' when 'i' is referenced

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function 'cacheSolve' uses the R 'solve' function
## and the functions defined in 'makeCacheMatrix'
## to compute the inverse of a matrix (assuming there is one)
## and to set it.
## Before it calculates the inverse, it checks to see if 
## the inverse has already been created (and is stored in 'i')

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
