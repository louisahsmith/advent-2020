input <- readLines("input/02-input.txt")

dat <- data.frame(matrix(unlist(strsplit(input, "\\-|\\s")), 
                         byrow = TRUE,
                         nrow = length(input)))
dat <- setNames(dat, c("min", "max", "letters", "password"))

# question 1
dat$min <- as.numeric(dat$min)
dat$max <- as.numeric(dat$max)
dat$letters <- substr(dat$letters, 1, 1)
dat$rev <- mapply(gsub, dat$letters, dat$password, MoreArgs = list(replacement = ""))
dat$n <- nchar(dat$password) - nchar(dat$rev)
dat$valid <- dat$n >= dat$min & dat$n <= dat$max
sum(dat$valid)

# question 2
dat$first <- substr(dat$password, dat$min, dat$min)
dat$second <- substr(dat$password, dat$max, dat$max)
dat$valid2 <- (dat$letters == dat$first & dat$letters != dat$second) |
    (dat$letters != dat$first & dat$letters == dat$second)
sum(dat$valid2)
