#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.
#**********Testing code ************************

rm(list = ls())

matrix_for_test <-read.csv(file.choose(),header = T)
returned_matrix <- makeCacheMatrix(matrix_for_test)

cacheSolve(returned_matrix)
#solve.default() can only invert numeric or logical, and not int


#********* question 1 **************************
makeCacheMatrix <-function(x = matrix){
    matrix_return<- NULL
#*******set function **********   
    set<-function(y){
        x <<- y  #setting y to a global X
        matrix_return <<- NULL
    }
#*******get function *********
    
    get<-function() x
        setmatrix<-function(solve) matrix_return <<- solve
        
        getmatrix<-function() matrix_return
        
        list(
        set=set, 
        get=get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)
}

#cacheSolve: This function computes 
#the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
    matrix_return <- x$getmatrix()
    
    if(!is.null(matrix_return)) {
        message("getting cached data")
        return(matrix_return)
    }
    data.matrix <- x$get()
    matrix_return <- data.matrix^-1  #slove() is a built in funciton to get inverse
    #print(matrix_return)
    x$setmatrix(matrix_return)
    matrix_return
}



