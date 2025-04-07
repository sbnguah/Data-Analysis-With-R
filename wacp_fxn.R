wacp <- 
    function(x, rows){
    mat1 <- matrix(x, nrow = rows, byrow = T)
    tbl1 <- as.table(mat1)
    tbl2 <- addmargins(tbl1)
    chsq <- c(chisq.test(tbl1)$statistic, chisq.test(tbl1)$p.value)
    fs <- c(fisher.test(tbl1)$statistic, fisher.test(tbl1)$p.value)
    list(Table = tbl2, Chisq_Test = chsq, Fisher_test = fs) 
    }