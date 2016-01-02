## This function creates a special "matrix" object that can cache its inverse.
##The special matrix object is a list that contains functions to
## allow a user to access the matrix and its inverse

makeCacheMatrix <- function(x = matrix())
{
	inv_mat <- NULL
    
	set <- function(y)
	{
		mat <<- y
		inv_mat <<- NULL
	}
	get <- function() mat
    
	setinverse <- function(x) inv_mat <<- x
    
	getinverse <- function() inv_mat
    
	list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed)
##, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) 
{
	inv_matrix <- x$getinverse()
	if(!is.null(inv_matrix) && identical(x$get(),solve(inv_matrix)))
	{
		message("Retrieving cached data.")
		return(inv_matrix)
	}
    
	matrix <- x$get()
	inv_matrix <- solve(matrix)
	x$setinverse(inv_matrix)
	inv_matrix       
}
