help(density)

# The Old Faithful geyser data
d <- density(faithful$eruptions, bw = "sj")
d
plot(d)

example(density)


median(x = 1:10)
x
median(x <- 1:10)
x
rm(x)


x <- 1:24
dim(x) <- length(x)
x
x[22]
matrix(1:24,nrow=4,ncol=6)
matrix(1:24,nrow=4,ncol=6,byrow=T)

