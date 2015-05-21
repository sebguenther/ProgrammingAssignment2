
#Create sample Matrix
exampleInput <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE)

#Init the caching 
sample <- makeCacheMatrix(exampleInput)

#Two sample runs to check the caching function
inversedSample <- cacheSolve(sample)
inversedSample <- cacheSolve(sample)

#Create a matrix, which is able to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        #Inverse value is set NULL (does not yet exist)
        m <- NULL
        
        #Define the set function; m is set null again, if another matrix is set
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #Get-function for the origin matrix
        get <- function() x
        #Sets the inverse matrix on the private parameter 'm'
        setInverse <- function(solve) m <<- solve
        
        #Get Function for the inverse matrix
        getInverse <- function() m
        
        #Creates the List
        invisible(list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse))
}

#Get the inverse of a matrix; calculation is cached
cacheSolve <- function(x) {
        
        #Check if the inverse matrix has already been calculated
        m <- x$getInverse()
        
        #If it is calculated, return the respective inverse matrix...
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #...else calculate the inverse and cache its values
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        
        #Return the inverse
        m
}