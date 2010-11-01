ifeq ($(origin AFSEQ_ROOT), undefined)
AFSEQ_ROOT := $(shell pwd)
endif

ifeq ($(origin AFSEQ_BINDIR), undefined)
AFSEQ_BINDIR := $(AFSEQ_ROOT)
endif

ifeq ($(origin ERL), undefined)
ERL := ERL_LIBS="$(AFSEQ_ROOT):." erl
endif

ifeq ($(origin ERLC), undefined)
ERLC := erlc $(EFLAGS)
endif

SOURCES := $(shell ls *.erl | sort)
OBJECTS=$(patsubst %.erl,$(AFSEQ_BINDIR)/%.beam,$(SOURCES))

.PHONY : all
all: build

build: $(SOURCES) bindir $(OBJECTS)
	@cd tests; make build

.PHONY : test
test: build
	@cd tests; make test

bindir:
	@mkdir -p $(AFSEQ_BINDIR)

$(AFSEQ_BINDIR)/%.beam : %.erl
	$(ERLC) -o $(AFSEQ_BINDIR) $<

.PHONY : clean
clean:
	@find $(AFSEQ_ROOT) -name erl_crash.dump -exec rm -f {} \;
	@find $(AFSEQ_ROOT) -name \*.beam -exec rm -f {} \;

.PHONY : check
check:
	@-rm *.beam
	@EFLAGS='+debug_info' make build
	@dialyzer -n *.beam

.PHONY : stats
stats:
	@echo "Code:"
	@find . \( -name \*.erl -o -name \*.escript \) | grep -v /tests/ | xargs wc -l | tail -1 | sed -r -e 's/^\s*([0-9]+)\s+total/    \1 total/'
	@echo "Test:"
	@find . \( -name \*.erl -o -name \*.escript \) | grep /tests/ | xargs wc -l | tail -1 | sed -r -e 's/^\s*([0-9]+)\s+total/    \1 total/'
