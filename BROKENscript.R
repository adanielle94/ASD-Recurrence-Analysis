# rm(list=ls())

#Adapted from https://github.com/mekline/CLANtoR/blob/master/CLANtoR.R
library(plyr)
library(tidyr)

read.CLAN.filebroken <- function(file_broken) {
  tmp_broken <- readLines(file_broken)
  print(file_broken)
  #Cycle through utterances and make a line for each.
  alltext_broken <- paste(tmp_broken, collapse="\n")
  utts_broken <- unlist(strsplit(alltext_broken, "(?<=\n)\\*",perl=TRUE))
  utts_broken <- utts_broken[-1]
   
  data<-data.frame()
  for (checkutts in utts_broken) {
    storebadies<-get_utt_info_broken(checkutts)
  data<- rbind(data, storebadies)
    # print(checkutts)
    
  }
  
    
    
  #Collect the data that will be appended to every line
  data$Filename <- file_broken
  
  p <- grep("@Participants", tmp_broken, fixed=TRUE)
  data$Participants <- unlist(strsplit(tmp_broken[p], "\t"))[2]
  
  p <- grep("@Date", tmp_broken, fixed=TRUE)
  data$Date <- unlist(strsplit(tmp_broken[p], "\t"))[2]
  
  p <- grep("@Situation", tmp_broken, fixed=TRUE)
  data$File.Situation <- unlist(strsplit(tmp_broken[p], "\t"))[2]
  
  p<- grep("@Age", tmp_broken, fixed=TRUE)
  data$Age <- unlist(strsplit(tmp_broken[p], "\t"))[2]
  
  p<- grep("@Sex", tmp_broken, fixed=TRUE)
  data$Sex <- unlist(strsplit(tmp_broken[p], "\t"))[2]
  
  p <- grep("Target_Child", tmp_broken, fixed=TRUE)
  chiline <- tmp_broken[p[2]]
  chidata <- unlist(strsplit(chiline, "[|]"))
  data$Language <- substr(chidata[1], 6,9)
  data$Corpus <- chidata[2]
  
  #Add utt line numbers
  data$Line.No 
  
  
  #Get rid of some processing columns we don't want
  data$t.as.matrix.fields.. <- NULL
  xnums <- as.numeric(gsub("[^0-9]*[0-9]*[^0-9]*[0-9]*[^0-9]*[0-9]*[^0-9]+", "", names(data), perl=T)) 		# what a hack
  for(x in min(xnums, na.rm=T):max(xnums, na.rm=T)) {
    xname <- paste("X", x, sep="")
    data <- data[,!(names(data) %in% xname)]
    
  }
  
  #Make sure row names are preserved!
  data$Utt.Number <- row.names(data)
  
  #Return
  data
} #End read.CLAN.file


get_utt_info_broken <- function(u_broken){
  
  #Divide the line into individual utterances & tiers
  fields_broken <- unlist(strsplit(u_broken, "[%]"))
  #Make a dataframe
  myrow_broken <- data.frame(t(as.matrix(fields_broken)))
  
  #Add utterance info
  myrow_broken$Speaker <- substr(fields_broken[1], 1,3)
  myrow_broken$Verbatim <- substr(fields_broken[1], 6,nchar(fields_broken[1])-1)
  
  #Add info from any tiers, as they appear in the file
  if (length(fields_broken) > 1){
    for (j_broken in 2:length(fields_broken)){
      tier_broken <- data.frame(substr(fields_broken[j_broken], 6,nchar(fields_broken[j_broken])-1))
      names(tier_broken) <- c(substr(fields_broken[j_broken], 1,3))
      myrow_broken <- cbind(myrow_broken, tier_broken)
    }
  }
  
  #Some extra work: get the line as spoken, with glosses replaced
  #...This is an adult-language, human-readable version of the utterance
  
  myrow_broken$Gloss <- NA
  #First, find & replace sequences like this: "dunno [: don't know]" -> "don't know"
  words_broken <- unlist(strsplit(myrow_broken$Verbatim, " "))
  if (length(words_broken) == 0){
    words_broken <- c("")
  }
  
  words_broken <- unlist(strsplit(words_broken, "\t"))
  if (length(words_broken) == 0){
    words_broken <- c("")
  }
  
  words_broken <- unlist(strsplit(words_broken, "\n"))
  if (length(words_broken) == 0){
    words_broken <- c("")
  }
  
  w_broken <- 1
  wmax_broken <- length(words_broken) + 1
  while (w_broken < wmax_broken){
    #Did we hit a gloss sequence?
    if ((words_broken[w_broken]=="[:")|(words_broken[w_broken]=="[=?")){
      #Find where the gloss ends, then clean up
      closebracket_broken <- grep("]",  words_broken[w_broken:length(words_broken)], fixed=TRUE)[1] + (w_broken-1)
      words_broken[w_broken-1] <- ""
      words_broken[w_broken] <- ""
      words_broken[closebracket_broken] <- substr(words_broken[closebracket_broken], 1, nchar(words_broken[closebracket_broken])-1)
    }
    w_broken <- w_broken + 1	
  }
  
  #Next, find & replace clarification/elaboration sequences like this: "a bobby [= a thin bobbypin]" -> "a bobby"
  w_broken <- 1
  wmax_broken <- length(words_broken) + 1
  while (w_broken < wmax_broken){
    #Did we hit a gloss sequence?
    if ((substr(words_broken[w_broken],1,1) == "[")){
      #Find where the gloss ends, then clean up
      closebracket_broken <- grep("]",  words_broken[w_broken:length(words_broken)], fixed=TRUE)[1] + (w_broken-1)
      goo_broken <- closebracket_broken
      for (v_broken in w_broken:closebracket_broken){
        words_broken[v_broken] <- ""
      }
    }
    w_broken <- w_broken + 1	
  }
  
  #Next, delete internal notation we don't need here
  words_broken <- as.vector(mapply(gsub, "[()<>&@:]","",words_broken))
  
  #Remove sentence-internal periods!
  words_broken[1:(length(words_broken)-1)] <- as.vector(mapply(gsub, "[.]","",words_broken[1:(length(words_broken)-1)]))
  
  myrow_broken$Gloss <- paste(words_broken, collapse=" ")
  myrow_broken$Gloss <- gsub(" +", " ", myrow_broken$Gloss)
  
  myrow_broken
  
}

