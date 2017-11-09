rmse <-
function(x,y){
# root mean square error function
# x, y can be vector, matrix or array but their sizes should match
# returns a scalar 
	if (any(dim(x)!=dim(y))){stop()} # check function to prevent disastrous cases
	sqrt(mean((x-y)^2))

}
