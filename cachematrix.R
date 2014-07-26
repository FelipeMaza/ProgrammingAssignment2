## This is the .R file that creates a matrix and puts its inverse in cache.
## It was developed from the original code indicated in the instructuons regading the
## generation of a vector and the determination of its mean.
## Hope you have no problem reading it.

## This first function creates a matrix and a variable named "inv" which stores the inverse
## of that matrix. It has the set() function which sets the variables x to what is being called
## and inv to null. Then the function get() returns the matrix x. Then, setinverse() stores the 
## inverse of the function on cache, and getinverse() returnes that cached inverse value.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve() function fisrt evaluates if the inverse of the matrix has already been
## determined and stored. If it has, it prints the message "getting cached data" and it 
## returns the stored value. If it has not been caclulated, it uses the solve() function to
## determine it, store that value in the cache and print it in the command prompt.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
