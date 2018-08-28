rmse <- function(x,y){
	# root mean square error function
	# x, y can be vector, matrix or array but their sizes should match
	# returns a scalar 
	if (any(dim(x)!=dim(y))|| length(x)!=length(y)){
	# check function to prevent disastrous cases
	stop("two objects' sizes differ \n")
	}else{	 
	sqrt(mean((x-y)^2))
	}
}

pick_day <- function(x,y,z, threshold=5){
	# this is to pick the closest data points to the target value of y
	# and then it gives the value of z corresponding to the picked data points
	# this is (sort of) useful to align multiple time series 
	# grid is made over y
	out <- z[min(which.min(abs(x-y)))]
	if (min(abs(x-y))>=threshold){out <- NA} # if too far from the target date, we mark it as NA
	return(out)
}

n_unique <- function(x){ 
	# equivalent of dplyr::n_distinct, but n_distinct says it's faster than this
	# probably this function should be deprecated
	length(unique(x))
}

catn <- function(x) { 
	# convenient function for screen printing
	# useful when running simulation
	cat(x, "\n")
	}

check_objects  <- function(pos = 1, pattern, order.by="Size", decreasing=TRUE, head=FALSE, n=10){
    if (!missing(order.by)) 
    	out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")

    out
}