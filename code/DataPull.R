library(XML)

# Section Number indicates where a course is being taught
#001-019 = main campus
#020 = bountiful campus
#060 = Murry campus
#070 = sandy campus
#090 = online

# Get a list of courses offered in a given semester
courseEnroll <- function(listurl){
  cDoc <- htmlParse(listurl)
  courseLsTbl <- readHTMLTable(cDoc, as.data.frame=T, which=1)
  
  if(is.null(courseLsTbl) == FALSE){
    CS01 <- na.omit(courseLsTbl[-1,])
    CS01$cnum <- as.numeric(levels(CS01[,3]))[CS01[,3]]
    CS01$snum <- as.numeric(levels(CS01[,4]))[CS01[,4]]
    CS02 <- CS01[which(CS01$cnum<6000),]
    CS03 <- CS02[which(CS02$cnum>=1000),]
    CS03$term <- term
  
  return(CS03)
  }
}

#Function to extract all of the OBIA tables for a course and return a dataframe with all of the information
courseStats <- function(corurl){ #parse the document for R representation: 
  
  Hdoc <- htmlParse(corurl)
  
  #get all the tables in Hdoc as data frames
  
  Htabs1 <- readHTMLTable(Hdoc, as.data.frame=T, which=c(1))
  #row.names(Htabs1) <- Htabs1$V1
  names(Htabs1) <- c("V1", "V2")
  
  Htabs2 <- readHTMLTable(Hdoc, as.data.frame=T, which=c(2))
  #row.names(Htabs2) <- Htabs2$V1
  names(Htabs2) <- c("V1", "V2")
  
  Htabs3 <- readHTMLTable(Hdoc, as.data.frame=T, which=c(3)) # Problem: need both columns which breaks when I transpose the data
  #row.names(Htabs3) <- c("Maj1", "Maj2", "Maj3", "Maj4", "Maj5", "Maj6", "Maj7", "Maj8", "Maj9", "Maj10")
  names(Htabs3) <- c("V2", "V1")
  Htabs3 <- Htabs3[, c(2,1)]
  if(nrow(Htabs3 < 10)){ # If there are less than 5 rows
    m <- as.data.frame(matrix(0, 10-nrow(Htabs3),2)) # Create a new dataframe with the missing rows
    names(m) <- c("V2", "V1") # Give it similar names
    Htabs3 <- rbind(Htabs3,m) # Bind them together
    #row.names(Htabs5) <- c("Class1", "Class2", "Class3", "Class4", "Class5")    
  } 
  
  
  Htabs4 <- readHTMLTable(Hdoc, as.data.frame=T, which=c(4))
  #row.names(Htabs4) <- Htabs4$V1
  names(Htabs4) <- c("V1", "V2")
  

  Htabs5 <- readHTMLTable(Hdoc, as.data.frame=T, which=c(5))
  if(is.null(Htabs5) == FALSE){ # If there is data in the table
    #Switch the columns and vie the columns names
    Htabs5 <- Htabs5[, c(2,1)]
    names(Htabs5) <- c("V2", "V1")
    if(nrow(Htabs5 < 5)){ # If there are less than 5 rows
      m <- as.data.frame(matrix(0, 5-nrow(Htabs5),2)) # Create a new dataframe with the missing rows
      names(m) <- c("V2", "V1") # Give it similar names
      Htabs5 <- rbind(Htabs5,m) # Bind them together
      #row.names(Htabs5) <- c("Class1", "Class2", "Class3", "Class4", "Class5")    
    } 
  } else { # If there is no table make one filled with 0's
    Htabs5 <- matrix(0, 5,2)
    names(Htabs5) <- c("V2", "V1")
  }
  
  
  Htabs6 <- readHTMLTable(Hdoc, as.data.frame=T, which=c(6))
  if(is.null(Htabs6) == FALSE){ # If there is data in the table
    #Switch the columns and vie the columns names
    Htabs6 <- Htabs6[, c(2,1)]
    names(Htabs6) <- c("V2", "V1")
    if(nrow(Htabs6 < 5)){ # If there are less than 5 rows
      m <- as.data.frame(matrix(0, 5-nrow(Htabs6),2)) # Create a new dataframe with the missing rows
      names(m) <- c("V2", "V1") # Give it similar names
      Htabs6 <- rbind(Htabs6,m) # Bind them together
      #row.names(Htabs6) <- c("HS1", "HS2", "HS3", "HS4", "HS5")     
    } 
  } else { # If there is no table make one filled with 0's
    Htabs6 <- matrix(0, 5,2)
    names(Htabs6) <- c("V2", "V1")
  }

  
  Htabs7 <- readHTMLTable(Hdoc, as.data.frame=T, which=c(7))
  if(is.null(Htabs7) == FALSE){ # If there is data in the table
    #Switch the columns and vie the columns names
    Htabs7 <- Htabs7[, c(2,1)]
    names(Htabs7) <- c("V2", "V1")
    if(nrow(Htabs7 < 5)){ # If there are less than 5 rows
      m <- as.data.frame(matrix(0, 5-nrow(Htabs7),2)) # Create a new dataframe with the missing rows
      names(m) <- c("V2", "V1") # Give it similar names
      Htabs7 <- rbind(Htabs7,m) # Bind them together
      #row.names(Htabs7) <- c("HS1", "HS2", "HS3", "HS4", "HS5")     
    } 
  } else { # If there is no table make one filled with 0's
    Htabs7 <- matrix(0, 5,2)
    names(Htabs7) <- c("V2", "V1")
  }
  
  
  full <- rbind(Htabs1, Htabs2, Htabs3, Htabs4, Htabs5, Htabs6, Htabs7)
  f02 <- t(as.data.frame(full[,2]))
  names(f02) <- row.names(full)
  return(f02)
}

courseOffered <- function(offurl){
  ofurl <- htmlParse(offurl)
  if(term==1004){
    offLsTbl <- readHTMLTable(ofurl, as.data.frame=T, which=1)
  }else{
      offLsTbl <- readHTMLTable(ofurl, as.data.frame=T, which=5)
      }
  
    C01 <- na.omit(offLsTbl)
    names(C01) <- c("Flg","Class Number", "Subject", "Catalog Number", "Section", "Component", "Units", "Title", "Days Taught", "Time", "Units.1", "Class Attributes", "Instructor", "Feedback", "PreReq", "Fees")
    C02 <- C01[which(C01$Flg != "Flg"),c(3:13)]
    C02$term <- term
    
    return(C02)
  
}

# Note acs only goes back to 2000, term 1004

# Create loop for each term (term equals year and semester)
for(i in seq(0,1,1)){ 
  for(j in seq(0,9,1)){
    for(k in seq(4,8,4)){ # k is for the semester 4==Spring, 8==Fall, 6==Summer (but I'm not doing summer)
      term <- paste(c(1,i,j,k), collapse="") # concatenate loop variables into a term
      
      #Generate the url for the specific term
      urlgen1 <- paste(c("http://www.acs.utah.edu/uofu/stu/scheduling/crse-info?term=",term, 
                         "&subj=SOC"),collapse="")
      
      # Run function to pull information on enrollment numbers
      temp1 <- courseEnroll(urlgen1)
      
      ########################################### Get Demographic Profile for Classes
      # For each course found in the Course Enrollment pull course statistics
      for(l in nrow(temp1)){
        if(temp1[l,7] != 0 & term >= 1038){
          cnum <- paste(temp1[l,3], collapse="") # get the course number
          secnum <- paste(temp1[l,4], collapse="") # get the section Number
          
          # Generate the url
          urlgen3 <- paste(c("http://obia.utah.edu/dm/cp4/apps/cpReport.php?subj=SOC&course=", 
                             cnum, "&section=", secnum,"&term=",term),
                           collapse="")
          
          # if the term is included in the OBIA database extract the relevant tables
          if(i==0 & j<=4){          
          }else{
            temp3 <- courseStats(urlgen3)
            temp3$course <- cnum # add the course number for joining
            temp3$sec <- secnum # add the Section number for joining
            temp3$term <- term # add the term number for joining
            
            # Write out the table resulting from the above function and append each new term to the file, export as .csv
            write.table(temp3, file="stats.csv", append=TRUE, col.names=FALSE, sep=",")
            print(paste(c("Writing Stats for term ", term), collapse=""))
            
              } # end else
            }# end if
          } # end for l
      
      ########################################### Get Number Registered
      #Genereate url to get the schedule for a term
      urlgen2 <- paste(c("http://www.acs.utah.edu/uofu/stu/scheduling?term=",term,
                         "&dept=SOC&classtype=g"),collapse="")
      
      temp2 <- courseOffered(urlgen2)       
      # Write out the table resulting from the above function and append each new term to the file, export as .csv
      write.table(temp1, file="enrollment2.csv", append=TRUE, col.names=FALSE, sep=",")
      write.table(temp2, file="catelog2.csv", append=TRUE, col.names=FALSE, sep=",")
      print(paste(c("Writing tables for term ", term), collapse=""))
            
      
    } # end for k
  }# end for j
  
}# end for i
# Note OBIA only goes back to 2004 term 1044