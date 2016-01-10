pollutantmean <- function(directory, pollutant, id =1:332)
{
    directory <- paste("/home/arif/",directory,"/",sep = "")
    total <- 0
    num <- 0
    if(pollutant == "sulphate")
        pollutant_column <- 2
    else
        pollutant_column <- 3
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
            if(!is.na(x[j,pollutant_column]))
             {
                total <- total + x[j,pollutant_column]
                num <- num + 1
             }    
          }
    }
       signif(total/num,4)
}