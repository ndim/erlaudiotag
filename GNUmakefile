-include local.mk
TEST_FILE ?= does-not-exist.mp3

ERL_MODS = id3v2
ERLC_FLAGS =
ERLC_FLAGS += +debug_info
ERLC_FLAGS += +report
ERLC_FLAGS += +verbose
# ERLC_FLAGS += +return

CLEAN_FILES =
CLEAN_FILES += *~
CLEAN_FILES += erl_crash.dump

ALL_BEAMS = $(foreach base,$(ERL_MODS),$(base).beam)
CLEAN_FILES += $(ALL_BEAMS)

.PHONY: check
check: all orig.dump id3parse-test.mp3 id3parse-test.dump
	cmp id3parse-test.dump orig.dump || \
		colordiff -u orig.dump id3parse-test.dump | less -r
	cmp "$(TEST_FILE)" id3parse-test.mp3

.PHONY: all
all: $(ALL_BEAMS)

.PHONY: help
help:
	@echo TEST_FILE=$(TEST_FILE)

%.beam: %.erl
	erlc $(ERLC_FLAGS) "$<"

CLEAN_FILES += id3parse-test.mp3
id3parse-test.mp3: GNUmakefile $(ALL_BEAMS)
	erl -noshell -s id3v2 test "$(TEST_FILE)" -s init stop

CLEAN_FILES += id3parse-test.dump
id3parse-test.dump: id3parse-test.mp3
	hexdump -C "$<" | sed '/00 00 00 ff fb/q' > "$@"

CLEAN_FILES += orig.dump
orig.dump: GNUmakefile
	hexdump -C "$(TEST_FILE)" | sed '/00 00 00 ff fb/q' > "$@"

.PHONY: clean
clean:
	rm -f $(CLEAN_FILES)
