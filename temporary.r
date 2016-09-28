n.unique <- function(x){
	length(unique(x))
}

pick.day <- function(x,y,z){
	# this is to pick the closest data points to the target value of y
	# and then it gives the value of z corresponding to the picked data points
	# this is (sort of) useful to align multiple time series 
	# grid is made over y
	out <- z[min(which.min(abs(x-y)))]
	if (min(abs(x-y))>=5){out <- NA} # if too far from the target date, we mark it as NA
	return(out)
}

catn <- function(x) { 
# convenient function for cleaner screen print
	cat(x, "\n")
	}