complete <- function(directory,id = 1:332)
{
    directory <- paste("/home/arif/",directory,"/",sep = "")
    total <- 0
    df <- data.frame(id = 0,nobs = 0)
    for(i in id)
    {
        if(as.integer(i / 10) == 0)#If it is one-digit number
            csv_file <- paste("00",as.character(i),sep = "")
        else if(as.integer(i / 100) == 0)    
            csv_file <- paste("0",as.character(i),sep = "")
        else
            csv_file <- as.character(i)
        dir <- paste(directory,csv_file,".csv",sep = "")
        x <- data.frame(read.csv(dir,header = TRUE,sep = ","))
        
        for(j in 2:nrow(x))
        {
            if(!is.na(x[j,2]) && !is.na(x[j,3]))
                total <- total + 1
        }
        df <- rbind(df,c(i,total))
        total <- 0
    }
    df = df[-1,]
    rownames(df) = 1:nrow(df)
    df
}