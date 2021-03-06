library(XML)

# Section Number indicates where a course is being taught
#001-019 = main campus
#020 = bountiful campus
#060 = Murry campus
#070 = sandy campus
#090 = online

# Get a list of courses offered in a given semester
courseEnroll <- function(listurl, term){
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
  if(length(readHTMLTable(Hdoc)) == 0){
    
  }else{
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
      
    } 
    
    
    Htabs4 <- readHTMLTable(Hdoc, as.data.frame=T, which=c(4))
    #row.names(Htabs4) <- Htabs4$V1
    names(Htabs4) <- c("V1", "V2")
    
    
    Htabs5 <- readHTMLTable(Hdoc, as.data.frame=T, which=c(5))
    if(is.null(Htabs5) == FALSE){ # If there is data in the table
      #Switch the columns and vie the columns names
      Htabs5 <- Htabs5[, c(1,2)]
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
      Htabs6 <- Htabs6[, c(1,2)]
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
      Htabs7 <- Htabs7[, c(1,2)]
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
}

# Function to get all of the courses offered in a given semester
courseOffered <- function(offurl, term, dept){
  ofurl <- htmlParse(offurl)
  if(term==1004 | dept %in% c("PSYCH", "GEOGR", "POL S", "ANTHR")){
    offLsTbl <- readHTMLTable(ofurl, as.data.frame=T, which=1)
  }else{
    offLsTbl <- readHTMLTable(ofurl, as.data.frame=T, which=5, stringsAsFactors = TRUE, encoding="bytes")
  } 
  
  C01 <- na.omit(offLsTbl)
  names(C01) <- c("Flg","Class Number", "Subject", "Catalog Number", "Section", "Component", "Units", "Title", "Days Taught", "Time", "Units.1", "Class Attributes", "Instructor", "Feedback", "PreReq", "Fees")
  C02 <- C01[which(C01$Flg != "Flg"),c(3:13)]
  C02$term <- term
  
  C03 <- cbind(
    gsub("[^0-9A-Za-z///' ]", "", C02[,1]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,2]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,3]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,4]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,5]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,6]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,7]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,8]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,9]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,10]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,11]),
    gsub("[^0-9A-Za-z///' ]", "", C02[,12]))
  names(C03) <- names(C02)
  return(C03)
  
}

# Note acs only goes back to 2000, term 1004

# Function to generate urls to to feed to other functions and write csv files
genData <- function(subject, cterm = "1154"){
  s <- toupper(subject)
  cterm <- as.character(cterm)
  
  #Output File Names
  ename <- paste(c("data/enrollment", s, ".csv"), collapse="")
  catname <- paste(c("data/catelog", s, ".csv"), collapse="")
  sname <- paste(c("data/stats", s, ".csv"), collapse="")
  
  # Create loop for each term (term equals year and semester)
  for(i in seq(0,1,1)){ 
    for(j in seq(0,9,1)){
      for(k in seq(4,8,4)){ # k is for the semester 4==Spring, 8==Fall, 6==Summer (but I'm not doing summer)
        term <- paste(c(1,i,j,k), collapse="") # concatenate loop variables into a term
        #print(term)
        #Generate the url for the specific term
        
        # Generate exception for 2005 name changes
        if(term<=1056 && s=="PSY"){
          s <- "PSYCH"
        } else {
          if(term>1056 && s=="PSYCH"){
            s <- "PSY"
        }}
        if(term<=1056 && s=="ANTH"){
          s <- "ANTHR"
        }else {
          if(term>1056 && s=="ANTHR"){
            s <- "ANTH"
          }}
        if(term<=1056 && s=="GEOG"){
          s <- "GEOGR"
        }else {
          if(term>1056 && s=="GEOGR"){
            s <- "GEOG"
          }}
        if(term<=1056 && s=="POLS"){
          s <- "POL S"
        }else {
          if(term>1056 && s=="POL S"){
            s <- "POLS"
          }}
        
        if(term < cterm){
          urlgen1 <- paste(c("http://www.acs.utah.edu/uofu/stu/scheduling/crse-info?term=",term, 
                             "&subj=", s),collapse="")
          
          # Run function to pull information on enrollment numbers
          temp1 <- courseEnroll(urlgen1, term)
          
          
          
          ########################################### Get Number Registered
          #Genereate url to get the schedule for a term
          
          urlgen2 <- paste(c("http://www.acs.utah.edu/uofu/stu/scheduling?term=",term,
                             "&dept=", s, "&classtype=g"),collapse="")
          
          temp2 <- courseOffered(urlgen2, term, s)       
          
          # Write out the table resulting from the above function and append each new term to the file, export as .csv
          
          
          write.table(temp1, file=ename, append=TRUE, col.names=FALSE, sep=",")
          write.table(temp2, file=catname, append=TRUE, col.names=FALSE, sep=",")
          print(paste(c("Writing tables for term: ", term," in the ",s, " department" ), collapse=""))
          
        }# end if
      } # end for k
    }# end for j
    
  }# end for i
  # Note OBIA only goes back to 2004 term 1044
  
  
  print(paste(c("Begin Writing demographics for classes in the ", s, " department"), collapse=""))
  t <- read.csv(ename, header=FALSE)
  t01 <- t[which(t$V8>0 & t$V12>=1044),]
  
  for(n in seq(1,nrow(t01),1)){
    if(n==1){
      nam <- as.data.frame(c("PreMajor", "IMajor", "FMajor", "Masters", "PhD", "Prof", "NM", "total",
                             "MeanAge", "Males", "White", "FT", "Transfer", "Resident", "Foreign",
                             "MeanCredits", "MeanTransCredit", "MeanUCredit", "MeanUGPA",
                             "MeanHSGPA", "MeanTransGPA",
                             "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10",
                             "Fresh", "Soph", "Jr", "Sen", "Grad",
                             "C1", "C2", "C3", "C4", "C5",
                             "HS1", "HS2", "HS3", "HS4", "HS5",
                             "T1", "T2", "T3", "T4", "T5",
                             "cnum", "secnum", "term", "Dept"))
      write.table(t(nam), file=sname, append=FALSE, col.names=FALSE, row.names=FALSE,sep=",")
    }
    
    urlgen3 <- paste(c("http://obia.utah.edu/dm/cp4/apps/cpReport.php?subj=SOC&course=", 
                       t01[n,"V4"], "&section=", t01[n,"V5"],"&term=",t01[n,"V12"]),
                     collapse="")
    temp3 <- courseStats(urlgen3)
    if(is.null(temp3)==FALSE){
      temp3$course <-  t01[n,"V4"] # add the course number for joining
      temp3$sec <- t01[n,"V5"] # add the Section number for joining
      temp3$term <- t01[n,"V12"] # add the term number for joining
      temp3$Dept <- t01[n,"V3"]
      
      # Write out the table resulting from the above function and append each new term to the file, export as .csv
      
      write.table(temp3, file=sname, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
      #print(paste(c("Writing Stats for term ", t01[n,"V12"], " course ", t01[n,"V4"]), collapse=""))
    }
  }
  print(paste(c("Finished data collection for the ", s, " department."), collapse=""))
}

# List of department codes in CSBS
CSBS <- list("FCS", "SOC", "PSY", "GEOG", "ANTH", "POLS", "ECON")

for(x in seq(3,length(CSBS),1)){
  genData(CSBS[[x]])
}