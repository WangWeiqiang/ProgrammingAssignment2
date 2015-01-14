## The two functions were created according the template introduced in assignment brieffing.
## Example for using them:
## > x<-makeCacheMatrix(matrix(rnorm(16),4,4))
## > cacheSolve(x) ##first time: the cache is NULL, the inverse is from calculation
## > cacheSolve(x) ##second time: the inverse value is from cache

## This function creates a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	s<-NULL
	set<-function(y){
		x<<-y
		s<<-NULL
	}
	
	get<-function() x

	setsolve<-function(solve) s<<-solve

	getsolve<-function() s

	list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above. If the inverse has 
## already been calculated, then the cacheSovle should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s<-x$getsolve()

	if(!is.null(s)){
		message("getting cached data")
		return(s)
	}

	data<-x$get()

	s<-solve(data)

	x$setsolve(s)
	
	s
}
