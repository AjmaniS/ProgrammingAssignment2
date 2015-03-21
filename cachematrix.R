## Following two functions makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix.

##This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    	## set the value of the matrix 
    	inv <- NULL
    	set <- function(y) {
        	x <<- y
        	inv <<- NULL
    	}
    
	##get the value of the matrix
    	get <- function() x
    
    	##set the value of inverse of the matrix
    	setinverse <- function(inverse) inv <<- inverse
 
	## get the value of inverse of the matrix
   	getinverse <- function() inv
    	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
##inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the 
##inverse from the cache. (note: it assumes that the matrix is always invertible)

cacheSolve <- function(x, ...) {
        
 	##get the value of the inverse of matrix
	inv <- x$getinverse()

	##check if inverse of matrix x is available in the cache and return a matrix that is the inverse of 'x' if present
    	if(!is.null(inv)) {
        	message("getting cached data.")
        	return(inv)
    	}
    	
	##get the value of the matrix
	data <- x$get()

	## compute inverse of the matrix
    	inv <- solve(data)

	## set the value of inverse of the matrix
    	x$setinverse(inv)

	## Return a matrix that is the inverse of 'x'
    	inv
}

## Example 
##> x = rbind(c(1, -1/3), c(1/3, 1))
##> m = makeCacheMatrix(x)
##> m$get()
##          [,1]       [,2]
##[1,] 1.0000000 -0.3333333
##[2,] 0.3333333  1.0000000

##First time computes inverse of matrix m
##> cacheSolve(m)
##     [,1] [,2]
##[1,]  0.9  0.3
##[2,] -0.3  0.9

##Later retrieves inverse of matrix m from the cache
##> cacheSolve(m)
#getting cached data.
#     [,1] [,2]
#[1,]  0.9  0.3
#[2,] -0.3  0.9
#> 
