


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        cached_matrix <- NULL
        
        # set: 
        # a new matrix (given in "set_matrix" parameter) is assigned in the parent environment to "x" variable. 
        # As we are setting a new matrix, we do not have a cached_matrix yet, so we initialice it to NULL to avoid errors.
        set <- function(set_matrix) {
                x <<- set_matrix
                cached_matrix <<- NULL
        }
        
        # get:
        # returns the current/original matrix ("x" variable).
        get <- function() x
        
        # setinverse:
        # An inverse matrix is given in inverse_matrix parameter to be set as the inverse cached matrix, 
        # who will be stored in cached_matrix variable
        setinverse <- function(inverse_matrix){
            cached_matrix <<- inverse_matrix
        }
        
        # getinverse:
        # returns the stored cached inversed matrix ("cached_matrix" variable).
        getinverse <- function() cached_matrix
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(matrix, ...) {
        ## Return a matrix that is the inverse of 'matrix'

        # In this function environment, we will use "local_inverse_matrix" variable to 
        # store the matrix we will work with. 

        # Initially, we will get its inverse value with "getinverse" function.
        in_matrix <- matrix$getinverse()
     
     	# If it is NOT NULL, then it has the inverse matrix (hopefully :)) ), so we return it 
        if(!is.null(in_matrix)) {
                return(in_matrix)
        }

        # If it is NULL, then it's empty, so we need to calculate the inverse matrix, and will do it as follows:
        # 
        # We use the built-in "solve" function to calculate the inverse matrix.
        # Firtly we get the matrix, we pass it to solve function to calculate its inverse, and we pass the result
        # of this calculation to setinverse function to set this inverse as the new cached inversed matrix.
        else {
        	in_matrix <- matrix$setinverse(solve(matrix$get()))
            return (in_matrix)
        }
}

