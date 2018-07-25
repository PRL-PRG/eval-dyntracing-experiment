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
file_crawl_one <- function(regex, file, pkg_name){
	lines <- read_lines(file)
	line_count <- length(lines)
	lines <- gsub("(^(([\\s\\t]*)?#)).*", "\n", lines, perl = TRUE)
	
	line_ns <- line_numbers(lines, regex)
	
	str <- str_c(lines, collapse = "\n")

	df <- return_evals_from_string(line_ns, str, pkg_name, file, line_count, regex)
	df
}

#for each file in the package, for each regex, fileCrawlOne()
eval_crawl <- function(dir, lib_addr){
	print(dir)
	pkg_name <- dir
	pkg_addr <- paste(lib_addr, pkg_name, sep = "/")
	file_lists <- list.files(path = pkg_addr, recursive = TRUE, pattern = "\\.((R)|(Rd)|(Rmd)|(rmd)|(r)|(rd))$", full.names = TRUE)
	
	dfs <- map_dfr(file_lists, function(file) map_dfr(REGEX, ~ file_crawl_one(., file, pkgName)))

	df <- bind_rows(dfs)
	df
}

#main function - for each package in library, evalcrawl()
main <- function(lib_addr){
	dir_lists <- list.dirs(path = lib_addr, full.names = FALSE, recursive = FALSE)
	
	dfs <- pblapply(dir_lists, eval_crawl, lib_addr = lib_addr, cl = parallel::detectCores())
	
	df <- bind_rows(dfs)
	write_csv(df, "evals.csv")
}

#Lists the indices (or line numbers) of regex matching strings in the vector lst.
#If there are multiple occurrences in a single vector element, the index (or the line number) is repeated that many times.
line_numbers <- function(lst, regex)
{
	line_ns <- vector()
	arr <- gregexpr(pattern = regex[1], lst, perl = TRUE)
	if(length(arr) < 1)
		return(line_ns)
	for(i in 1:length(arr))
	{
		if(attr(arr[[i]], "match.length") > 0)
		{
			rep_lines <- rep(i, length(attr(arr[[i]], "match.length") ))
			line_ns <- c(line_ns, rep_lines)
		}
	}
	line_ns
}

#return the arguments of `regex` used at index `ind` in the `str`
return_args <- function(ind, str, regex)
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
	while(b_count != 0)
	{
		if(identical(str[i], '(') & switch1 & switch2 & switch3)
			b_count <- b_count + 1
		else if(identical(str[i], ')') & switch1 & switch2 & switch3)
			b_count <- b_count - 1
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
return_evals_from_string <- function(line_number, str, pkg, path, line_count, regex)
{
	eval_table <-
		data_frame(
			pkg = character(),
			path = character(),
			line_number = integer(),
			use = character(),
			arguments = character(),
			line_count = integer(),
			char_count = integer())

	if(identical(str, character(0)))
		return(eval_table)
	
	eval_indcs <- gregexpr(pattern = regex[1], str, perl = TRUE)[[1]]

	if(length(eval_indcs) < 2){
		return(eval_table)
	}
	else{
		args <- lapply(eval_indcs, return_args, str = str, regex = regex)

		eval_table <- 
			data_frame(
				pkg,
				path,
				line_number,
				use = regex[3],
				arguments=unlist(args),
				line_count,
				char_count=nchar(str))
		return(eval_table)
	}
}

#execute main
main(commandArgs(trailingOnly = T)[1])
#sudo Rscript eval-crawler.R /mnt/nvme0/R/CRAN-extracted
