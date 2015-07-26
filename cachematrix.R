# R Programming - Assignment 2 - Marco Fanti


# Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly. Your assignment is to write a pair of functions that
# cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL                                         
        set <- function(y) {                                    
                x <<- y                                  
                inv_matrix <<- NULL                                         
        }
        get <- function() x                               
        set_cache_matrix <- function(inverse) inv_matrix <<- inverse    
        get_cache_matrix <- function() inv_matrix                       
        list(set = set, get = get,
             set_cache_matrix = set_cache_matrix,
             get_cache_matrix = get_cache_matrix)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        local_matrix <- x$get_cache_matrix()               
        if (!is.null(local_matrix)) {                   
                message("getting cached data")  ## If local_matrix is not NULL, return the value of m with a message.
                return(local_matrix)
        }                                       
        starting_matrix <- x$get()                                        
        local_matrix <- solve(starting_matrix)   
        x$set_cache_matrix(local_matrix)             
        local_matrix          
}


test = function(mat){
        ## @mat: an invertible matrix
        
        temp = makeCacheMatrix(mat)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}
