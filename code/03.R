input <- readLines("input/03-input.txt")[-1] # don't need row 1
tmp <- unlist(strsplit(input, ""))
dat <- matrix(tmp, byrow = TRUE, nrow = length(input))

count_trees <- function(n, dat) {
  inds <- (1 + n * (1:nrow(dat))) %% ncol(dat)
  inds[inds == 0] <- ncol(dat)
  possible_trees <- dat[cbind(1:nrow(dat), inds)]
  sum(possible_trees == "#")
}

# question 1
count_trees(3, dat)

# question 2
all_slopes <- mapply(count_trees, c(1, 2, 5, 7, 5), 
                     list(dat, dat, dat, dat, dat[-seq(1, nrow(dat), by = 2), ]))
Reduce(`*`, all_slopes)
