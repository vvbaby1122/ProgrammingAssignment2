##This first function makes a empty matrix x and set m to NULL, the set and get functions withinin this makeCacheMatrix function
##set a matrix, stores it in another environment and calls the function on x. 
##Then, it set the inverse of the matrix and stores it in another environment.
##Finally, it lists out the results accordingly.
makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

##This second function index matrix x by another function getinverse, then filters out the null matrices. Then solve the inverse of
##a given matrix.
cacheSolve <- function(x,...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
