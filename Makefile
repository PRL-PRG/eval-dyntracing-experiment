export R_ENABLE_JIT=0
export R_DISABLE_BYTECODE=1
export R_COMPILE_PKGS=0

R_DYNTRACE_HOME := R-dyntrace
R_DYNTRACE := $(R_DYNTRACE_HOME)/bin/R
OUTPUT_DIRPATH := $(shell date +'%Y-%m-%d-%H-%M-%S')
ANALYSIS_DIRPATH := $(OUTPUT_DIRPATH)/analysis
RAW_ANALYSIS_DIRPATH := $(ANALYSIS_DIRPATH)/raw
CORPUS_DIRPATH := $(OUTPUT_DIRPATH)/corpus
LOG_DIRPATH := $(OUTPUT_DIRPATH)/log
JOBLOG_FILEPATH := $(LOG_DIRPATH)/summary
CORPUS_FILEPATH := data/corpus.txt
PARALLEL_JOB_COUNT_FILEPATH := data/parallel-job-count.txt
VERBOSE :=
ANALYSIS_FLAGS :=

define tracer =
$(R_DYNTRACE) \
    --slave \
    --no-restore \
    --file=scripts/extract-package-code.R \
    --args \
    --r-dyntrace=$(R_DYNTRACE) \
    --corpus-dirpath=$(CORPUS_DIRPATH) \
    --raw-analysis-dirpath=$(RAW_ANALYSIS_DIRPATH) \
    $(ANALYSIS_FLAGS) \
    $(VERBOSE)
endef

define parallel =
parallel \
    --jobs $(PARALLEL_JOB_COUNT_FILEPATH) \
    --files \
    --bar \
    --load 80% \
    --results $(LOG_DIRPATH)/packages/{1}/ \
    --joblog $(JOBLOG_FILEPATH) \
      $(tracer) \
      {1} \
      ::::
endef

trace:
	mkdir -p $(ANALYSIS_DIRPATH) $(CORPUS_DIRPATH) $(LOG_DIRPATH)
	-$(parallel) $(CORPUS_FILEPATH) > /dev/null

analyze:
	@echo "Rule 'analyze' not implemented yet!"

update-corpus:
	$(R_DYNTRACE) --file=scripts/make-package-list.R --args $(CORPUS_FILEPATH)

install-dependencies:
	$(R_DYNTRACE) --file=scripts/install-dependencies.R

lint:
	$(R_DYNTRACE) -e "lintr::lint('scripts/extract-package-code.R')"
	$(R_DYNTRACE) -e "lintr::lint('scripts/install-dependencies.R')"
	$(R_DYNTRACE) -e "lintr::lint('scripts/make-package-list.R')"

.PHONY: trace analyze update-corpus install-dependencies lint
