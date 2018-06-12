#!/usr/bin/env Rscript

library(stringr)
library(readr)
library(purrr)
library(dplyr)
library(pbapply)

#Sys.setlocale('LC_ALL','C')
regex <- list()

regex[[1]]  <- c( "(?<!([\"a-zA-Z0-9_#]))(eval\\()" , "4" , "eval")
regex[[2]]  <- c( "(?<!([\"a-zA-Z0-9_#]))(evalq\\()" , "5" , "evalq")
regex[[3]]  <-  c("(?<!([\"a-zA-Z0-9_#]))(eval\\.parent\\()" , "11" , "eval.parent")
regex[[4]]  <- c("(?<!([\"a-zA-Z0-9_#]))(local\\()" , "5" , "local")

fileCrawlOne <- function(file, regex, pkgName) {
  lines <- readLines(file)
  lineCount <- length(lines)
  lines <- gsub("(^(([\\s\\t]*)?#)).*", "\n", lines , perl = TRUE);
  lineNs <- lineNumbers(lines , regex)
  
  str <- paste0(lines, collapse="\n")
  
  df <- returnEvalsFromString(lineNs , str , pkgName , file , lineCount , regex)
  return(df)
}

evalCrawlOne <- function(dir , libAddr , regex) {
  #print("1")
  print(dir)
  
  pkgName <- dir
  pkgAddr <- paste(libAddr , pkgName , sep="/")
  fileLists <- list.files(path = pkgAddr , recursive = TRUE , pattern = "\\.((R)|(Rd)|(Rmd)|(rmd)|(r)|(rd))$", full.names = TRUE)
  #fileLists <- list.files(path = pkgAddr , recursive = TRUE , full.names = TRUE)
  df<- map_dfr(fileLists, ~ fileCrawlOne(., regex , pkgName))
  return(df)
}

evalCrawl <- function( regex , libAddr) {
  dirLists <- list.dirs(path = libAddr , full.names = FALSE , recursive = FALSE)
  print(length(dirLists))
  dfs <- pblapply(dirLists, evalCrawlOne, libAddr=libAddr, regex = regex , cl=parallel::detectCores())
  
  df <- bind_rows(dfs)
  return(df)
}

main <- function(libAddr){
  dfs <- lapply(regex , evalCrawl , libAddr = libAddr)
  df <- bind_rows(dfs)
  write_csv(df, "evals.csv")
}

lineNumbers <- function(lst , regex)
{
  lineNs <- vector()
  arr <- gregexpr(pattern = regex[1] , lst , perl = TRUE )
  if(length(arr)<1)
    return(NA)
  for(i in 1:length(arr))
  {
    if( attr(arr[[i]] , "match.length") > 0  )
    {
      repLines <- rep( i , length( attr(arr[[i]] , "match.length") ))
      lineNs <- c(lineNs , repLines)
    }
  }
  return(lineNs)
}

#globVar <- 1
returnArgs <- function(ind , str , regex)
{
  str <- strsplit(str, "")[[1]]
  arg = vector()
  
  i <- ind + 1 + as.numeric(regex[2])
  bCount <- 1
  arg[1] <- '('
  bi <- 2
  switch1 <- TRUE
  switch2 <- TRUE
  switch3 <- TRUE
  while(bCount != 0)
  {
    if(identical(str[i] , '(') & switch1 & switch2 &switch3)
      bCount <- bCount + 1
    else if(identical(str[i] ,')') & switch1 & switch2 &switch3)
      bCount <- bCount - 1
    else if( identical(str[i] , '#') & switch1 & switch2){
      switch3 <- FALSE
    }
    else if(identical(str[i] , '\n') & !switch3){
      switch3 <- TRUE
    }
    else if( identical(str[i] , '"') & switch3 &switch2)
    { 
      if(!identical(str[i-1],'\\'))
        switch1 <- ! switch1	
      else if(identical(str[i-2],'\\'))
        switch1 <- ! switch1
    }
    else if( identical(str[i] , '\'') & switch1 & switch3)
    { 
      if(!identical(str[i-1],'\\'))
        switch2 <- ! switch2	
      else if(identical(str[i-2],'\\'))
        switch2 <- ! switch2
    }
    arg[bi] <- str[i]
    i <- i+1
    bi <- bi+1		
  }
  arg <- paste(arg, collapse = "")
  #globVar <<- globVar + 1
  return(arg)
}

returnEvalsFromString <- function(lineNumber, str, pkg, path , lineCount , regex)
{
  newlineIndcs <- which(strsplit(str, "")[[1]]=='\n')
  evalIndcs <- gregexpr(pattern = regex[1] , str , perl = TRUE )[[1]]
  
  if(length(evalIndcs) < 2){
    evalTable <- data_frame(pkg = character(),
                            lineNumber = integer(),
                            use = character(),
                            arguments = character(),
                            lineCount = integer(),
                            charCount = integer(),
                            path = character())
    return(evalTable)
  }
  else{
    args <- lapply( evalIndcs, returnArgs , str = str , regex = regex)
    
    evalTable <- data_frame(
      pkg,
      lineNumber,
      use = regex[3],
      arguments=unlist(args),
      lineCount,
      charCount=nchar(str),
      path
    )
    return(evalTable)
  }
}

main(commandArgs(trailingOnly = T)[1])



# regEx1 <- "eval\\((.)*\\)"
# regEx2 <- "evalq\\((.)*\\)"
# regEx3 <- "eval.parent\\((.)*\\)"
# regEx4 <- "local\\((.)*\\)"
