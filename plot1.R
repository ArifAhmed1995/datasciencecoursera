plot1 <- function()
{
    library(data.table)
    x <- fread("hpc.txt",na.strings = "?",stringsAsFactors = FALSE)
    x <- x[66637:69516,]
    y <- x$Global_active_power
    
    png('plo1.png')
    hist(y,freq = TRUE,col = "RED",
         main = "Global Active Power",xlab = "Global Active Power(kilowatts)")
    dev.off()
    
}