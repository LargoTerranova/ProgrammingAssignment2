# Put comments here that give an overall description of what your functions do

# Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        # 1. set the matrix, 2. get the matrix, 3. set the inverse, 4. get the inverse
        
        inv = NULL
        set = function(y) {
                # use <<- to assign a value to an object in an environment different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # output of makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("fetching cache")
                return(inv)
        }
        
        # if not there the inverse is calculated
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # st the value of the inverse
        x$setinv(inv)
        
        return(inv)
}


#END
