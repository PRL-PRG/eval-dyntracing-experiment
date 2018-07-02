#!/usr/bin/env Rscript

library(stringr)
library(readr)
library(purrr)
library(dplyr)
library(pryr)
library(pbapply)
library(rlist)

#read a file as a string and return_evals_from_string()
file_crawl_one <- function(file , pkg_name) {
	lines <- readLines(file)
	line_count <- length(lines)
	df <- return_evals_from_string(str, pkg_name, file, line_count)
	df
}

#for each file in the package, for all regexes, file_crawl_one()
eval_crawl <- function(dir, lib_addr) {
	print(dir)
	pkg_name <- dir
	pkg_addr <- paste(lib_addr, pkg_name, sep = "/")
	file_lists <- list.files(path = pkg_addr, recursive = TRUE, pattern = "\\.((R)|(r))$", full.names = TRUE)
	dfs <- map_dfr(file_lists, ~ file_crawl_one(., pkg_name))
	df <- bind_rows(dfs)
	df
}

#main function - for each package in library, evalcrawl()
main <- function(lib_addr) {
	dir_lists <- list.dirs(path = lib_addr, full.names = FALSE, recursive = FALSE)
	dfs <- pblapply(dir_lists, eval_crawl, lib_addr = lib_addr, cl = parallel::detectCores())
	df <- bind_rows(dfs)
	write_csv(df, "evals_ast.csv")
}

#from the code of a file as a string, return the required dataframe
return_evals_from_string <- function( str, pkg, path, line_count) {
	parse_err_flag = FALSE
	eval_table <-
		data_frame(
			pkg = character(),
			path = character(),
			use = character(),
			arg1 = character(),
			arg2 = character(),
			arg3 = character(),
			arg4 = character(),
			line_count = integer()
			)
	if(identical(str, character(0))) {
		return(eval_table)
	}
	
	#parse the code
	parsed_code <- tryCatch(parse(path, keep.source = T), error = function(cond) parse_err_flag <- TRUE)
	
	if(parse_err_flag)
		return(eval_table)

	#find evals in each expression using find_evals and get it in the right format
	extracted_evals <- lapply(parsed_code , find_evals)
	extracted_evals <- purrr::compact(extracted_evals)
	extracted_evals <- unlist(extracted_evals, rec = T)
	
	if(length(extracted_evals)==0)
		return(eval_table)

	use <- c()
	arg <- list(c(),c(),c(),c())

	#convert each expression to string and split arguments
	lapply(extracted_evals,
		function(x) {
			if(length(x)>5)
				return()

			strc <- (as.character(x))
			
			use <<- c(use, strc[1])
			if(length(x)>1) {
				for(i in 1:(length(x)-1))
					arg[[i]] <<- c(arg[[i]], strc[i+1] )
			}
			if(length(x)<5) {
				for(i in length(x):4)
					arg[[i]] <<- c(arg[[i]], "na")
			}
		}
	)

	if(!identical(extracted_evals, list()))
	{
		eval_table <-
			data_frame(
				pkg,
				path,
				use=unlist(use),
				arg1=unlist(arg[[1]]),
				arg2=unlist(arg[[2]]),
				arg3=unlist(arg[[3]]),
				arg4=unlist(arg[[4]]),
				line_count
				)
		return(eval_table)
	}
	return(eval_table)
}

#from a single parsed expression return all the evals present with arguments, recursively
find_evals <- function(x) 
{
	if(is.atomic(x)) {
		list()
	}
	else if(is.name(x)) {
		list()
	}
	else if(is.call(x))	{
		children <- unlist(lapply(x, find_evals))

		if(any( c(x[[1]]) %in% c(quote(eval), quote(evalq), quote(local), quote(eval.parent) ) ))
			children <- append(list(x) , children)
		children
	}
	else if (is.pairlist(x)) {
		children <- list()
		for (i in seq_along(x))
			children[[i]] <- find_evals(x[[i]])
		children <- lapply(children, function(x) x[!is.na(x)])
		unlist(children)
	}
	else
		stop("Errrrr! ", typeof(x), call. = FALSE)
}

#execute main
main(commandArgs(trailingOnly = T)[1])
#sudo Rscript eval-ast-crawler.R /mnt/nvme0/R/CRAN-extracted
