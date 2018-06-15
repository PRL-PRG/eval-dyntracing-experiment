#!/usr/bin/env Rscript

library(stringr)
library(readr)
library(purrr)
library(dplyr)
library(pbapply)

#Sys.setlocale('LC_ALL','C')

#regex to search for `evals` being used in the code string
notPreceededBy <- "(?<!([\"a-zA-Z0-9_#]))"
regex <- list(
	c(paste(notPreceededBy, "(eval\\()", sep = ""), "4", "eval"),
	c(paste(notPreceededBy, "(evalq\\()", sep = ""), "5", "evalq"),
	c(paste(notPreceededBy, "(eval\\.parent\\()", sep = ""), "11", "eval.parent"),
	c(paste(notPreceededBy, "(local\\()", sep = ""), "5", "local")
)

#read a file as a string and returnEvalsFromString()
fileCrawlOne <- function(regex, file, pkgName){
	lines <- readLines(file)
	lineCount <- length(lines)
	lines <- gsub("(^(([\\s\\t]*)?#)).*", "\n", lines, perl = TRUE)
	lineNs <- lineNumbers(lines, regex)

	str <- paste0(lines, collapse = "\n")

	df <- returnEvalsFromString(lineNs, str, pkgName, file, lineCount, regex)
	df
}

#for each regex, fileCrawlOne()
evalCrawlOne <- function(file, pkgName){
	df<- map_dfr(regex, ~ fileCrawlOne(., file, pkgName))
	df
}

#for each file in the package, evalCrawlOne()
evalCrawl <- function(dir, libAddr){
	print(dir)
	pkgName <- dir
	pkgAddr <- paste(libAddr, pkgName, sep = "/")
	fileLists <- list.files(path = pkgAddr, recursive = TRUE, pattern = "\\.((R)|(Rd)|(Rmd)|(rmd)|(r)|(rd))$", full.names = TRUE)
	dfs <- lapply(fileLists, evalCrawlOne, pkgName = pkgName)
	df <- bind_rows(dfs)
	df
}

#main function - for each package in library, evalcrawl()
main <- function(libAddr){
	dirLists <- list.dirs(path = libAddr, full.names = FALSE, recursive = FALSE)
	print(length(dirLists))
	dfs <- pblapply(dirLists, evalCrawl, libAddr = libAddr, cl = parallel::detectCores())
	df <- bind_rows(dfs)
	write_csv(df, "evals.csv")
}

#Lists the indices (or line numbers) of regex matching strings in the vector lst.
#If there are multiple occurrences in a single vector element, the index (or the line number) is repeated that many times.
lineNumbers <- function(lst, regex)
{
	lineNs <- vector()
	arr <- gregexpr(pattern = regex[1], lst, perl = TRUE)
	if(length(arr) < 1)
		return(NA)
	for(i in 1:length(arr))
	{
		if(attr(arr[[i]], "match.length") > 0)
		{
			repLines <- rep(i, length(attr(arr[[i]], "match.length") ))
			lineNs <- c(lineNs, repLines)
		}
	}
	lineNs
}

#return the arguments of `regex` used at index `ind` in the `str`
returnArgs <- function(ind, str, regex)
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
		if(identical(str[i], '(') & switch1 & switch2 & switch3)
			bCount <- bCount + 1
		else if(identical(str[i], ')') & switch1 & switch2 & switch3)
			bCount <- bCount - 1
		else if(identical(str[i], '#') & switch1 & switch2)
			switch3 <- FALSE
		else if(identical(str[i], '\n') & !switch3)
			switch3 <- TRUE
		else if(identical(str[i], '"') & switch3 & switch2)
		{ 
			if(!identical(str[i-1], '\\'))
				switch1 <- ! switch1	
			else if(identical(str[i-2], '\\'))
				switch1 <- ! switch1
		}
		else if(identical(str[i], '\'') & switch1 & switch3)
		{ 
			if(!identical(str[i-1], '\\'))
				switch2 <- ! switch2	
			else if(identical(str[i-2], '\\'))
				switch2 <- ! switch2
		}
		arg[bi] <- str[i]
		i <- i+1
		bi <- bi+1
	}
	arg <- paste(arg, collapse = "")
	arg
}

#from the code of a file as a string, return the required dataframe
returnEvalsFromString <- function(lineNumber, str, pkg, path, lineCount, regex)
{
	newlineIndcs <- which(strsplit(str, "")[[1]] == '\n')
	evalIndcs <- gregexpr(pattern = regex[1], str, perl = TRUE)[[1]]
	
	if(length(evalIndcs) < 2){
		evalTable <- data_frame(pkg = character(),
					path = character(),
					lineNumber = integer(),
					use = character(),
					arguments = character(),
					lineCount = integer(),
					charCount = integer())
		return(evalTable)
	}
	else{
		args <- lapply(evalIndcs, returnArgs, str = str, regex = regex)
		evalTable <- data_frame(
			pkg,
			path,
			lineNumber,
			use = regex[3],
			arguments=unlist(args),
			lineCount,
			charCount=nchar(str))
		return(evalTable)
	}
}

#execute main
main(commandArgs(trailingOnly = T)[1])
#To run -> sudo Rscript evalCrawlerFormatted.R /mnt/nvme0/R/CRAN-extracted
