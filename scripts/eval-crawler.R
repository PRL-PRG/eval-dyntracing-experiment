#!/usr/bin/env Rscript

library(stringr)
library(readr)
library(purrr)
library(dplyr)
library(pbapply)

#Sys.setlocale('LC_ALL','C')

#regex to search for `evals` being used in the code string
NOT_PRECEEDED_BY <- "(?<!([\"a-zA-Z0-9_#]))"
REGEX <- list(
	c(paste(NOT_PRECEEDED_BY, "(eval\\()", sep = ""), "4", "eval"),
	c(paste(NOT_PRECEEDED_BY, "(evalq\\()", sep = ""), "5", "evalq"),
	c(paste(NOT_PRECEEDED_BY, "(eval\\.parent\\()", sep = ""), "11", "eval.parent"),
	c(paste(NOT_PRECEEDED_BY, "(local\\()", sep = ""), "5", "local")
)

#read a file as a string and returnEvalsFromString()
fileCrawlOne <- function(regex, file, pkgName){
	lines <- readLines(file)
	lineCount <- length(lines)
	lines <- gsub("(^(([\\s\\t]*)?#)).*", "\n", lines, perl = TRUE)
	
	lineNs <- lineNumbers(lines, regex)
	
	str <- str_c(lines, collapse = "\n")

	df <- returnEvalsFromString(lineNs, str, pkgName, file, lineCount, regex)
	df
}

#for each file in the package, for each regex, fileCrawlOne()
evalCrawl <- function(dir, libAddr){
	print(dir)
	pkgName <- dir
	pkgAddr <- paste(libAddr, pkgName, sep = "/")
	fileLists <- list.files(path = pkgAddr, recursive = TRUE, pattern = "\\.((R)|(Rd)|(Rmd)|(rmd)|(r)|(rd))$", full.names = TRUE)
	
	dfs <- map_dfr(fileLists, function(file) map_dfr(REGEX, ~ fileCrawlOne(., file, pkgName)))

	df <- bind_rows(dfs)
	df
}

#main function - for each package in library, evalcrawl()
main <- function(libAddr){
	dirLists <- list.dirs(path = libAddr, full.names = FALSE, recursive = FALSE)
	
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
		return(lineNs)
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
	switch1 <- TRUE		#for -> " permitted (true means you're outside the double quoted section)
	switch2 <- TRUE		#for -> ' permitted (true means you're outside the single quoted section)
	switch3 <- TRUE		#for -> # permitted (true means you're outside the comment section)
	while(bCount != 0)
	{
		if(identical(str[i], '(') & switch1 & switch2 & switch3)
			bCount <- bCount + 1
		else if(identical(str[i], ')') & switch1 & switch2 & switch3)
			bCount <- bCount - 1
		else if( identical(str[i], '#') & switch1 & switch2){
			switch3 <- FALSE
		}
		else if(identical(str[i], '\n') & !switch3){
			switch3 <- TRUE
		}
		else if( identical(str[i], '"') & switch3 & switch2)
		{ 
			if(!identical(str[i-1], '\\'))
				switch1 <- ! switch1	
			else if(identical(str[i-2], '\\'))
				switch1 <- ! switch1
		}
		else if( identical(str[i], '\'') & switch1 & switch3)
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
	evalTable <-
		data_frame(
			pkg = character(),
			path = character(),
			lineNumber = integer(),
			use = character(),
			arguments = character(),
			lineCount = integer(),
			charCount = integer())

	if(identical(str, character(0)))
		return(evalTable)
	
	evalIndcs <- gregexpr(pattern = regex[1], str, perl = TRUE)[[1]]

	if(length(evalIndcs) < 2){
		return(evalTable)
	}
	else{
		args <- lapply(evalIndcs, returnArgs, str = str, regex = regex)

		evalTable <- 
			data_frame(
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
#sudo Rscript eval-crawler.R /mnt/nvme0/R/CRAN-extracted
