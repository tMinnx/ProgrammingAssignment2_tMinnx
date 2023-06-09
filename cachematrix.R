## Put comments here that give an overall description of what your
## functions do

# Both `makeCacheMatrix` and `cacheSolve` functions are incomplete when used 
# by themselves. 

# `makeCacheMatrix` produces a list of 4 special functions, a matrix of our 
# interest, and a reservoir/cache for our output of interest, the inverse of 
# the matrix.`cacheSolve` first detects whether our output of interest is 
# already in the reservoir/cache of `makeCacheMatrix` and returns that value 
# if it exists. Otherwise, it continues to calculate the inverse of the matrix 
# and stores the new output in the reservoir/cache of `makeCacheMatrix`

# That way, we can save time in calculating the inverse of the same matrix 


## Write a short comment describing this function

# The first function, `makeCacheMatrix` creates a special "matrix", which is 
# a list containing 4 nested functions: 2 setters (`set` and `setinverse`) and 2 
# getters (`get` and `getinverse`). It also includes 2 variables 'x' and 
# 'inv.make' that can be retrieved through special means for further calculation

# The formal argument of the `makeCacheMatrix` is a simple matrix called `x`. 
# Then, a local variable `inv.make` is set to the value NULL. 

# The first nested `set` function has 2 roles: 1) making a new matrix without 
# calling the parent function 'makeCacheMatrix' and 2) resetting the variable 
# 'inv.make' to NULL. Please notice that "<<-" operator was used in both roles so that 
# the variables in the parent environment ('x' and 'inv.make') are changed when 
# the child function `set` is run.

# The second nested function `get` calls the matrix of our interest 

# The third nested function `setinverse` assigns the inverse of the matrix 
# calculated by the `cacheSolve` function and stores it as a cache. Again "<<-"
# operator was used so that the variable `inv.make` in the parent environment is
# changed. The word `inverse` in the `setinverse` function is synonymous with
# the main output of the `cacheSolve` function i.e. `inv.solve`.

# The last nested function `getinverse` retrieves the value of `inv.make` which 
# may be NULL or contain a cache.


makeCacheMatrix <- function(x = matrix()) {
  inv.make <- NULL
  set <- function(new_matrix) {
    x <<- new_matrix
    inv.make <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv.make <<- inverse
  getinverse <- function() inv.make
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

# `cacheSolve` uses `getinverse`, one of the nested functions of 
# `makeCacheMatrix`, to retrieve the matrix inverse from the reservoir/cache or 
# variable `inv.make` from `makeCacheMatrix`. 

# If the cache is not NULL, we get the matrix inverse and assign it to the 
# variable `inv.solve`. The local variable 'inv.solve' of the `cacheSolve` 
# environment is distinct from the variable `inv.make` inside the 
# `makeCacheMatrix`environment. The matrix inverse is printed out together with
# the message "getting cached data".

# If the cache is NULL, `cacheSolve` retrieves the matrix of interest using 
# `get`, another nested function of `makeCacheMatrix` and assigns it to a local
# variable `matrix`. Then the matrix inverse is calculated using `solve` 
# function and assigns the result to the variable `inv.solve`. The matrix 
# inverse is printed out WITHOUT the message "getting cached data".

# The final step of the `cacheSolve` function is putting the newly calculated
# matrix inverse `inv.solve` into the reservoir/cache or `inv.make` inside the 
# `makeCacheMatrix` environment.

cacheSolve <- function(x, ...) {
  inv.solve <- x$getinverse()
  if(!is.null(inv.solve)) {
    message("getting cached data")
    return(inv.solve)
  }
  matrix <- x$get()
  inv.solve <- solve(matrix, ...)
  x$setinverse(inv.solve)
  inv.solve
  
}

# Testing commits
# It should have 2 commits

####Testing the functions
####[OPTIONAL to assist reviewers test my functions, Thank you!] 

# random 2x2 matrix of interest
set.seed(9)
n1 <- matrix(sample(1:100, 4), nrow = 2, ncol = 2)
n1                    

# inverse of n1
m1 <- solve(n1)       
m1

# Test for the Identity Matrix where the diagonal is 1
n1%*%m1               

# Test of 'makeCacheMatrix' and 'cacheSolve'
test <- makeCacheMatrix(n1) 
cacheSolve(test) #will give the same output as m1
cacheSolve(test) #the second run will give the cache result
