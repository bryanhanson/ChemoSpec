
seXy95 <- function(x) {
	m <- mean(na.omit(x))
	se <- seX(x)
	u <- m + 1.96 * se
	l <- m - 1.96 * se
	c(y = m, ymin = l, ymax = u)
	}
