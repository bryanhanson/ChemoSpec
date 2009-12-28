
seXy <- function(x) {
	m <- mean(na.omit(x))
	se <- seX(x)
	u <- m + se
	l <- m - se
	c(y = m, ymin = l, ymax = u)
	}
