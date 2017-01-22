## Matrix inversion is often a costly computation. Back in matrix algebra class I remember a saying, "the only thing more annoying than inverting a 3x3 matrix is inverting a 4x4 matix". Even for computers these computations can be exhausting. It may be beneficial to cache the inverse of a matrix rather than to keep computing it. The following two functions are used to cache the inverse of a matrix.

## The function first begins by initialzing to objects, x and inv. The <<- form of the assignment operator is used so that if a valid inverse matrix cahced in inv, whenever x is rest, the value of m cached in the memory of the object is reset (this will then cause subsequent calls to cacheinverse() to recalculate). Then finally this function ccreates a list containing a function to: set the value of the matrix, get the value of the matrix, set the value of the inverse of the matrix and get the value of the inverse of the matrox. 

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
		get <-function() x
		setinverse<-function(inverse) inv<<-inverse
		getinverse<-function() inv
		list(set=set, get=get, 
		setinverse=setinverse, getinverse=getinverse)
}


## This function will return the inverse of a matrix. It first checks the environment of the original function to see if the inverse of the matrix has been computed. If not then the it computes the inverse of the specified matrix, sets the value in the cache through the setinverse function.

cacheSolve <- function(x, ...) {
	inv <-x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data<-x$get()
	inv<-solve(data)
	x$setinverse(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}
