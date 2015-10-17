
# Descr : Function to store a matrix and get its inverse
# Input : An input n*n matrix
# Output: Returns an object containing four functions to get and set a matrix and to get inverse of matrix
#         Do not call setInvMatrix() to set the inverse of a matrix. Use cache solve to set inverse of matrix.


makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        getMatrix    <- function()        x
        setMatrix    <- function(new_mat){ x <<- new_mat; inv_mat <<- NULL}
        getInvMatrix <- function()       inv_mat
        setInvMatrix <- function(inv)    inv_mat <<- inv
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}

# Descr  : Calculates Inverse matrix for a matrix stored in MakeCacheMatrix Object. The calculated inverse matrix is stored
#          in cache and for subsequet calls result is returned from the cache
# Input  : Object from makeCacheMatrix() call
# Output : Returns cached matrix. If matrix is not invertable terminates printing an exception.  
cacheSolve <- function(x, ...) {
        # Get the inverse matrix from cache
        invmat <- x$getInvMatrix()
        
        # If inverse is available then return it
        if(!is.null(invmat)) {
          print("getting cached data")
          return(invmat)
        }
        
        # If cache hit fails calculate inverse matrix and cache it
        data <- x$getMatrix()
        result = tryCatch({ invMat = solve(data)
        x$setInvMatrix(invMat)
        }, 
        # If inverse cannot be calculated return appropriate error message
        error = function(e) print( paste("Exception:  Matrix is non invertable. Terminating", print(e)))
        ) #end-try
        invMat
}



## TestCases
# m = matrix( c(1122,12,1443,4,45,6,7,88,93), nrow=3, ncol=3)  - Invertable
# m1 = matrix( c(11,12,14,4,45,6,7,88,93), nrow=3, ncol=3) - Invertable
# m2 = matrix( c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3)  - non Invertable
# m4 = matrix( c(1,1,1,3,4,3,3,3,4), nrow=3, ncol=3) - Invertable
# m5 = matrix( c(4,3,3,2), nrow=2, ncol=2) - Invertable


obj=makeCacheMatrix(m)
cacheSolve(obj);
cacheSolve(obj)
