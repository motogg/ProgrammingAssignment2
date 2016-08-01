## Together, fucntions makeCacheMatrix and cacheSolve reduce computational 
## effort by storing the results of the inverse of a matrix and returning the 
## stored value if it already exists.

## makeCacheMatrix is and auxilliary function to cacheSolve that supplies the 
## matrix to be computed and stores the value of the caluclated matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
        
        
}


## cacheSolve recieves the matrix from makeCacheMatrix and returns the inverse
## if it exists; otherwise, it computes and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        
        else{
                matrix <- x$get()
                inv <- solve(matrix)
                x$setinverse(inv)
                inv
        }
        
}
