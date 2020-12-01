input <- read.delim("input/01-input.txt", header = FALSE)$V1

find_prod <- function(input, n = 2) {
    if (!n %in% 2:3) stop()
    out <- outer(input, input, `+`)
    if (n == 3) out <- outer(input, out, `+`)
    ind <- which(out == 2020, arr.ind = TRUE)[1,]
    Reduce(`*`, input[ind])
}

find_prod(input, n = 2)
find_prod(input, n = 3)
