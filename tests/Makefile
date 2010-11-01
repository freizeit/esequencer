export SHELL := /bin/bash

ifeq ($(origin AFSEQ_ROOT), undefined)
AFSEQ_ROOT := $(shell dirname `pwd`)
endif

ifeq ($(origin AFSEQ_TMPDIR), undefined)
AFSEQ_TMPDIR := $(shell pwd)
endif

ifeq ($(origin ERL), undefined)
ERL := erl -pz $(AFSEQ_ROOT) $(AFSEQ_TMPDIR)
endif

ifeq ($(origin ERLC), undefined)
ERLC := erlc $(EFLAGS)
endif

SOURCES := $(shell ls *.erl | sort)
OBJECTS=$(patsubst %.erl,$(AFSEQ_TMPDIR)/%.beam,$(SOURCES))

.PHONY : all
all: build

build: $(SOURCES) bindir $(OBJECTS)

bindir:
	@mkdir -p $(AFSEQ_TMPDIR)

.PHONY : test
test: $(OBJECTS)
	@for tmod in $(basename $(^F)) ; \
	do \
	echo "" ; \
	if [[ -z "$(TMOD)" || $$tmod == *$(TMOD)* ]]; \
	then \
		echo "**** $$tmod" ; \
		$(ERL) -noshell -s $$tmod test -s init stop; \
	else \
		echo "==== ignored $$tmod" ; \
	fi ; \
	done

$(AFSEQ_TMPDIR)/%.beam : %.erl
	$(ERLC) -o $(AFSEQ_TMPDIR) $<
