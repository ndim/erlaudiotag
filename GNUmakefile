-include local.mk
TEST_FILE ?= does-not-exist.mp3
ERL_MODS = id3v2
ERLC_FLAGS =
ERLC_FLAGS += +report
ERLC_FLAGS += +verbose
# ERLC_FLAGS += +return

ALL_BEAMS = $(foreach base,$(ERL_MODS),$(base).beam)

.PHONY: check
check: all id3parse-test.dump orig.dump
	colordiff -u orig.dump id3parse-test.dump | less -r

.PHONY: all
all: $(ALL_BEAMS)

.PHONY: help
help:
	@echo TEST_FILE=$(TEST_FILE)

%.beam: %.erl
	erlc $(ERLC_FLAGS) "$<"

id3parse-test.mp3: GNUmakefile $(ALL_BEAMS)
	erl -noshell -s id3v2 test "$(TEST_FILE)" -s init stop

id3parse-test.dump: id3parse-test.mp3
	hexdump -C "$<" | sed '/00 00 00 ff fb/q' > "$@"

orig.dump: GNUmakefile
	hexdump -C "$(TEST_FILE)" | sed '/00 00 00 ff fb/q' > "$@"
