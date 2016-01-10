corr <- function(directory,threshold = 0)
{
    complete_cases <- complete(directory)
    directory <- paste("/home/arif/",directory,"/",sep = "")
    main_vector <- vector(mode = "numeric",length = 0)
    for(i in 2:nrow(complete_cases))
    {
        nobs <- complete_cases[i,2]
        if(nobs > threshold)
         {
            name <- complete_cases[i,1]
            if(as.integer(name / 10) == 0)
                csv_file <- paste("00",as.character(i),sep = "")
            else if(as.integer(name / 100) == 0)    
                csv_file <- paste("0",as.character(i),sep = "")
            else
                csv_file <- as.character(i)
            
            dir <- paste(directory,csv_file,".csv",sep = "")
            x <- data.frame(read.csv(dir,header = TRUE,sep = ","))
            sulphate <- vector(mode = "numeric",length = nobs)
            nitrate <- vector(mode = "numeric",length = nobs)
            count <- 1
            
            for(j in 2:nrow(x))
            {
                if(!is.na(x[j,2]) && !is.na(x[j,3]))
                {
                    sulphate[count] <- x[j,2]
                    nitrate[count] <- x[j,3]
                    count <- count + 1
                }
            }
            main_vector <- c(main_vector,signif(cor(x = sulphate,y = nitrate,method ="pearson") , 4))
        }
    }
    main_vector
}