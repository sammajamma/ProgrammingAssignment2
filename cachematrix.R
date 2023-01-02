## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#using library(mass) to calculate inverse for non-square matrices 
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL #setting inverse as NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL #setting values in diff environment
        }
        get <- function() x  #function to get matrix x
        setinv <- function(inverse) minv <<- inverse
        getinv <- function(){
                inver <- ginv(x)
                inver%*%x   #function to calculate inverse of matrix
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
#function used to recall the cached data
cacheSolve <- function(x, ...) {
        minv <- x$getinv()
        if(!is.null(minv)) {          #checking if matrix inverse pulled in NULL
                message("getting cached data")
                return(minv)     #will retun inverse value if above is true
                ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        minv <- solve(data,...) #solves the inverse
        x$setinv(minv)
        minv #returns inverse of original matrix x
}


