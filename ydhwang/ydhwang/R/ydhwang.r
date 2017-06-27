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


	

.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
## Credit: Taken from:  http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved list of objects
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}


rmse <- function(x,y){
# root mean square error function
# x, y can be vector, matrix or array but their sizes should match
# returns a scalar 
	if (any(dim(x)!=dim(y))){stop()} # check function to prevent disastrous cases
	sqrt(mean((x-y)^2))

}


