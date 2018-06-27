# Eval Tracing Experiment

This is a repository of the experimental setup for tracing eval in R.

It runs [evaldyntracer](https://github.com/PRL-PRG/evaldyntracer) with our 
instrumented R interpreter, [R-dyntrace](https://github.com/PRL-PRG/R-dyntrace), 
on vignettes, examples and test cases extracted from installed R packages.


## Administration

All the experimental steps are controlled from a top level `Makefile`. 
Every experimental step is a make rule.

### Update Corpus

To update the list of packages (corpus) on which the tracer will be run, execute -

```
$ make update-corpus R_DYNTRACE_HOME=<R-DYNTRACE-HOME>
```

### Trace

To run the tracer on package vignettes, examples and test cases, execute -

```
$ make trace R_DYNTRACE_HOME=<R-DYNTRACE-HOME>
```

### Analyze

To analyze the tracing output, run -

```
$ make analyze R_DYNTRACE_HOME=<R-DYNTRACE-HOME>
```

## Development

If you are a developer, fork the project and make changes to your copy. 
After you are done making changes, push to your fork and send a pull request. 
Make sure all the make rules execute successfully with expected output.
