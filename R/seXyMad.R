
seXyMad <- function(x) {
	m <- median(na.omit(x))
	d <- mad(na.omit(x))
	u <- m + d
	l <- m - d
	c(y = m, ymin = l, ymax = u)
	}
