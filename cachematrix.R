# The two functions together create a cache of the inverse of a matrix and
# invoke that cache directly whenever there is a subsequent need to calculate
# the inverse of the same matrix, thereby eliminating the need to recompute the
# inverse on each occasion 

# This function returns a list with four elements, each of which is a function
# The four returned functions set and get the value of a matrix and of the 
# inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        
        get<-function() {
                return(x)
        }
        
        setInverse<-function(inverse) {
                inv<<-inverse
        }
        
        getInverse<-function() {
                return(inv)
        }
        
        return(list(set=set, get=get, setInverse=setInverse, 
                    getInverse=getInverse))
}


# This function checks to see whether the matrix inverse is already present in
# the cache
# If it is, then the function simply gets the inverse from the cache
# If it isn't, then the function performs the matrix inversion and sets the
# result of the computation to be the inverse in the cache

cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        
        if(!is.null(inv)) {
                message("Retrieving cached data")
                return(inv)
        }
        
        data<-x$get()
        inv<-solve(data, ...)
        x$setInverse(inv)
        
        return(inv)
}
