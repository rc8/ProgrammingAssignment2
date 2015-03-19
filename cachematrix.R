## We will cache the inverse of a matrix so that we save time and computation
## cost. This means we wont repeatedly compute the inverse of a matrix
## we will check if the inverse was already computed. If it was then we will
## skip the computation and return the value and if it wasn't then we 
## compute it as necessary. 

## The function makeCacheMatrix will create a list containing a function that:
## 1. sets the value of a matrix
## 2. gets the value of a matrix
## 3. set the value of the inverse of that matrix
## 4. gets the value of the inverse of that matrix

# Initializes a function that takes a matrix as an argument
makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	set <- function(y) {
		x <<- y
		inver <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inver <<- inverse
	getinverse <- function() inver
	#returns a list of all functions inside makeCacheMatrix
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function cacheSolve returns the inverse of a matrix. It will first
## check if that inverse has already been computed. If it has, then it will
## return the value and skip computations. If it hasn't then it will compute
## the inverse and return the result. 

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inver <- x$getinverse() 	# Sets inver equal to inverse value
	if(!is.null(inver)) { #If value has been computed it returns it
		message("\n...Retrieving cached data!!")
		return(inver)
	}	
	#if there was no number calculated we get the matrix into data variable
	data <- x$get()
	#calculates inverese of data stores it in inver variable
	inver <- solve(data,...)
	#assigns inverse value to cacheMatrixObject
	x$setinverse(inver)
	#returns the inverse
	inver
}


######## TESTING THE FUNCTIONS: ########
###Now we test it to make sure it works 
###We create a matrix that is invertible
###> x = rbind(c(4,3), c(3,2))
###> invertme = makeCacheMatrix(x)
###> invertme$get() #returns the matrix 
###     [,1] [,2]
###[1,]    4    3
###[2,]    3    2
###To show that there was NO cached data the first time so the computation
###will run in oder to get an inverse
###> cacheSolve(invertme)
###     [,1] [,2]
###[1,]   -2    3
###[2,]    3   -4
###This is attempting to calculate the matrix a SECOND time so that we 
###can see it will RETRIEVE the data from CACHE and skip the computations
###> cacheSolve(invertme)
###
###...Retrieving cached data!!
###     [,1] [,2]
###[1,]   -2    3
###[2,]    3   -4
