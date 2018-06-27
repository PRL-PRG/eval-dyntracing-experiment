library(fs)
library(optparse)
library(purrr)
library(stringr)

is_chr_scalar <- function(s) {
    is.character(s) && length(s) == 1 && nchar(s) > 0
}

log_debug <- function(...) {
    msg <- paste0(...)
    cat(msg, "\n")
}

#' @param filter a regular expression matching desired filenames without the '.R' extension
#' @export
#'
# TODO: support commentDontrun, commentDonttest
extract_package_code <- function(pkg, pkg_dir = find.package(pkg),
                                 types = c("examples", "tests",
                                           "vignettes", "all"),
                                 output_dir = tempfile(pattern = "genthat-extract_package"),
                                 filter = NULL) {

    stopifnot(is_chr_scalar(pkg))
    stopifnot(dir.exists(pkg_dir))
    stopifnot(is.null(filter) || is_chr_scalar(filter))

    if ("all" %in% types) {
        types <- c("examples", "tests", "vignettes")
    }

    types <- match.arg(types,
                       c("examples", "tests", "vignettes"),
                       several.ok = TRUE)

    # so the output list is named
    names(types) <- types

    lapply(types, function(type) {
        fun <- switch(
            type,
            examples = extract_package_examples,
            tests = extract_package_tests,
            vignettes = extract_package_vignettes
        )

        # each type has its own folder not to clash with one another
        output <- file.path(output_dir, type)
        stopifnot(dir.exists(output) || dir.create(output, recursive = TRUE))

        files <- fun(pkg, pkg_dir, output_dir = output)

        if (!is.null(filter)) {
            files <- files[grepl(filter, tools::file_path_sans_ext(files))]
        }

        names(files) <- NULL
        files
    })
}

#' @importFrom tools Rd_db Rd2ex
extract_package_examples <- function(pkg, pkg_dir, output_dir) {
    db <- tryCatch({
        tools::Rd_db(basename(pkg_dir), lib.loc = dirname(pkg_dir))
    }, error = function(e) {
        c()
    })

    if (!length(db)) {
        return(character())
    }

    files <- names(db)

    examples <- sapply(files, function(x) {
        f <- file.path(output_dir, paste0(basename(x), ".R"))
        tools::Rd2ex(db[[x]], f, defines = NULL)

        if (!file.exists(f)) {
            log_debug("Rd file `", x, "' does not contain any code to be run")
            NA
        } else {
            # prepend the file with library call
            txt <- c(
                paste0("library(", pkg, ")"),
                "",
                "",
                readLines(f)
            )
            writeLines(txt, f)
            f
        }
    })

    na.omit(examples)
}

extract_package_tests <- function(pkg, pkg_dir, output_dir) {
    test_dir <- file.path(pkg_dir, "tests")

    if (!dir.exists(test_dir)) {
        return(character())
    }

    files <- Sys.glob(file.path(test_dir, "*"))
    file.copy(files, output_dir, recursive = TRUE)

    tests <- file.path(output_dir, basename(files))
    tests <- tests[!dir.exists(tests)]
    tests <- tests[grepl("\\.R$", tests)]

    tests
}

#' @importFrom tools pkgVignettes checkVignettes
extract_package_vignettes <- function(pkg, pkg_dir, output_dir) {
    vinfo <- tools::pkgVignettes(pkg, source = T)
    if (length(vinfo$docs) == 0) {
        return(character())
    }

    if (length(vinfo$sources) == 0) {
        # so far no sources. The following should generate them if there are any
        # sources in the R code. It might actually run the vignettes as well.
        # That is a pity, but there is no way to tell it not to (the tangle is
        # needed to it extracts the R code)
        tools::checkVignettes(pkg, pkg_dir, tangle = TRUE,
                              weave = FALSE, workdir = "src")
    }

    # check if there are some sources
    vinfo <- tools::pkgVignettes(pkg, source = T)
    files <- as.character(unlist(vinfo$sources))
    if (length(files) == 0) {
        return(character())
    }

    file.copy(files, to = output_dir)
    vignettes <- file.path(output_dir, basename(files))

    vignettes
}


path_sanitized <- function(path) {
    path_abs(path_tidy(path), getwd())
}

parse_command_line_arguments <- function() {

    option_list <- list(
        make_option(c("-r", "--r-dyntrace"),
                    action = "store",
                    type = "character",
                    default = "../../R-dyntrace/bin/R",
                    help = "R-dyntrace interpreter.",
                    metavar = "r-dyntrace"),
        make_option(c("-v", "--verbose"),
                    action = "store_true",
                    default = FALSE,
                    help = "Flag to enable verbose mode.",
                    metavar = "verbose"),
        make_option(c("--corpus-dirpath"),
                    action = "store",
                    type = "character",
                    help = "Directory for storing vignettes.",
                    metavar = "corpus-dirpath"),
        make_option(c("--raw-analysis-dirpath"),
                    action = "store",
                    type = "character",
                    help = "Directory for tracer generated data.",
                    metavar = "raw-analysis-dirpath"),
        make_option(c("--disable-eval-expression-analysis"),
                    action = "store_true",
                    default = FALSE,
                    help = "Flag to disable eval expression analysis.",
                    metavar = "eval-expression-analysis"))

    args <- parse_args(OptionParser(option_list  =  option_list),
                       positional_arguments = TRUE)

    list(package = args$args[1],
         verbose = args$options$verbose,
         r_dyntrace = path_sanitized(args$options$`r-dyntrace`),
         corpus_dirpath = path_sanitized(args$options$`corpus-dirpath`),
         raw_analysis_dirpath =
             path_sanitized(args$options$`raw-analysis-dirpath`),
         analysis_flags =
             list(eval_expression_analysis =
                      !args$options$`disable-eval-expression-analysis`))
}


copy <- function(settings) {
    extract_package_code(settings$package, types = "all",
                         output_dir = path(settings$corpus_dirpath,
                                           settings$package));
}

wrap <- function(package_code, settings) {

    indent <- function(lines, spaces = 4) {
        indentation <- paste0(rep(" ", spaces), collapse = "")
        ifelse(lines == "",
               "",
               paste0(indentation, lines))
    }

    wrapper <- Vectorize(function(filepath, type) {
        dirpath <- path_dir(filepath)
        contents <- str_c(indent(readLines(filepath), 4), collapse = "\n")
        raw_analysis_dirpath <- path(settings$raw_analysis_dirpath,
                                     settings$package, type,
                                     path_ext_remove(path_file(filepath)))
        print(raw_analysis_dirpath)
        eval_expression_analysis <-
            settings$analysis_flags$eval_expression_analysis
        wrapped_contents <- str_glue(
            "setwd('{dirpath}')",
            "library(evaldyntracer)",
            "",
            "dyntrace_evals({{",
            "{contents}",
            "}},",
            "verbose = {settings$verbose},",
            "raw_analysis_dirpath = '{raw_analysis_dirpath}',",
            "analysis_flags = list(eval_expression_analysis = {eval_expression_analysis}))",
            .sep = "\n")
        writeLines(wrapped_contents, filepath)
        filepath
    }, vectorize.args = c("filepath"))

    iwalk(package_code, wrapper)
    package_code
}

run <- function(package_code, settings) {
    execute <- Vectorize(function(filepath) {
        system2(command = settings$r_dyntrace,
                args = str_glue("--file={filepath}"))
    }, vectorize.args = c("filepath"))

    iwalk(package_code,
          function(programs, type) {
              execute(programs)
          })
    package_code
}

main <- function() {
    settings <- parse_command_line_arguments()
    code <- copy(settings)
    wrap(code, settings)
    run(code, settings)
}

main()

warnings()
