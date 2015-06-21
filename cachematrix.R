#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
           
        #get is a function that returns the matrix M stored in the main function 
        # example:  makeCacheMatrix(matrix(1:4,2,2))$get() returns:
        #       [,1] [,2]
        # [1,]    1    3
        # [2,]    2    4
        
        get <- function() x
        
        m <- NULL
        
        #set is a function that changes the Matrix stored in the main function
        
        set <- function(y) {
                x <<- y    # "x <<- y" substitutes the vector x with y (the input) in the main function (makeCacheMatrix)
                m <<- NULL
        }
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m 
        
        list( get = get, set = set, setinverse = setinverse, getinverse = getinverse)    
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #store the value of the inverse of Matrix introduced in the previous function
        m <- x$getinverse() 
        
        # verify the value m exists and is not NULL and return m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #data gets the matrix stored with makeCacheMatrix
        data <- x$get() 
        
        # m calculates the inverse  of the Matrix
        m <- solve(data, ...)
        #stores m in the object generated assigned with makeCacheMatrix
        x$setinverse(m)
        m

}
