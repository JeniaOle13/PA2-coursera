## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m.in <- NULL
    set <- function(y) {
        x <<- y
        m.in <<- NULL
    }
    get <- function() x
    setinverse <- function(Inverse) m.in <<- Inverse
    getinverse <- function() m.in
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m.in <- x$getinverse()
    if(!is.null(m.in)) {
        message("getting cached data")
        return(m.in)
    }
    data <- x$get()
    m.in <- pseudoinverse(data, ...)
    x$setinverse(m.in)
    m.in
}

############ ############## ##############
a <- matrix(1:10, nrow = 10, ncol = 20)
i.a <- makeCacheMatrix(a)
i.a$get()
cacheSolve(i.a)
in.a <- cacheSolve(i.a)
in.a %*% a
