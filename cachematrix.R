
## makeCacheMatrix function creates a list of functions which act as a matrix-like 'object'
## of which you can compute the inverse at first, getting it from cache later

makeCacheMatrix <- function(x = matrix()) {
	#print ("this is the matrix: ") 
        #print (x)

        # this variable will be accessed by the two following set'ters functions
        # since it's present in the scope where they're defined (lexical scoping)
        mat_inverse <- NULL

        # set the content of matrix
        # => re-set its inverse to null (needs recomputing)
        set <- function(y) {
                x <<- y
                mat_inverse <<- NULL
        }

        # accessor to the value of x
        get <- function() x

        # setter function
	setinverse <- function(inverse) mat_inverse <<- inverse
        # getter function
        getinverse <- function() mat_inverse

        # return an 'object', made of a list of functions
 	list(set = set, get = get,
 	     getinverse = getinverse,
 	     setinverse = setinverse)

}




## Returns a matrix that is the inverse of 'x'
## And gets the inverse from a cached value, if already computed
## Note: it operates on 'objects' as retunred by makeCacheMatrix()

cacheSolve <- function(x, ...) {

        # get inverse from the input object
        inv <- x$getinverse()
        
        # if there's a cached value for the inverse, use it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # if there's no cached value, get original matrix to be inverted
        data <- x$get()
        # do the actual inversion
        inv <- solve(data, ...)
        # set the inverse matrix as 
        x$setinverse(inv)
        
        # return the inverse
        inv
}

# basic test:
# > source("cachematrix.R")
# > mystuff <- makeCacheMatrix ( matrix( c(2,0,0,0,3,0,0,0,1/5) , 3, 3 )  )

# > cacheSolve(mystuff)
# [,1]      [,2] [,3]
# [1,]  0.5 0.0000000    0
# [2,]  0.0 0.3333333    0
# [3,]  0.0 0.0000000    5

#> cacheSolve(mystuff)
# getting cached data
# [,1]      [,2] [,3]
# [1,]  0.5 0.0000000    0
# [2,]  0.0 0.3333333    0
# [3,]  0.0 0.0000000    5