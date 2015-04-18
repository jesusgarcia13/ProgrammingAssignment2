## makeCacheMatrix: function to Cache Inverse matrix operation
## Includes internal functions to handle cached inverse matrix
## Author: Jesus Garcia - R Programming (Coursera), course: rprog-013
makeCacheMatrix <- function(x = matrix()) {
	# Create empty Inverse matrix holder (ix)
	ix <- matrix(, nrow=nrow(x),ncol=ncol(x))
	
	# setter, assign empty matrix to initialize ix
	set <- function(y) {
                x <<- y
               ix <<- matrix(, nrow=nrow(x),ncol=ncol(x))
        }
    # getter, retrieve matrix
	get <- function() x
	
	# Assign inverse matrix
	setInverseMatrix <- function(imx) ix <<- imx
    
	# Retrieve inverse matrix
    getInverseMatrix <- function() ix
    
	#list of internal functions
    list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## cacheSolve: function to solve and 'cache' inverse matrix for
## function makeCacheMatrix
## Returns: a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ix <- x$getInverseMatrix()
		# Check for 'empty' inverse matrix, if not, use cached value
		# Empty matrix has all values = NA, vectorized to use function is.na
        if(!all(is.na(as.vector(ix)))) {
                message("getting cached Inverse matrix")
                return(ix)
        }
		# If not cached (first call), calculate or 'solve' the
		# Inverse matrix and set the value using the internal function x$setInverseMatrix()
		# setInverseMatrix
        mdata <- x$get()
        ix <- solve(mdata, ...)
        x$setInverseMatrix(ix)  
		# Return inverse matrix
        ix
}
