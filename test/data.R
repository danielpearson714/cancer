library("MSnbase")
data(msnset)
mean(exprs(filterNA(msnset)))
