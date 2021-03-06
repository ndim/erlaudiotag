-include local.mk
TEST_FILE ?= does-not-exist.mp3

ERL_MODS =
ERL_MODS += unicode
ERL_MODS += ndim_bpu
ERL_MODS += id3v2
ERL_MODS += ogg

ERLC_FLAGS =
ERLC_FLAGS += +debug_info
ERLC_FLAGS += +report
ERLC_FLAGS += +verbose
ERLC_FLAGS += -o ebin
ERLC_FLAGS += -I include
# ERLC_FLAGS += +return

CLEAN_FILES =
CLEAN_FILES += *~
CLEAN_FILES += erl_crash.dump

ALL_BEAMS = $(foreach base,$(ERL_MODS),ebin/$(base).beam)
CLEAN_FILES += $(ALL_BEAMS)

.PHONY: check
check: all orig.dump id3parse-test.mp3 id3parse-test.dump
	cmp id3parse-test.dump orig.dump || \
		colordiff -u orig.dump id3parse-test.dump | less -r

# The following test does not work with the new parser
# which only reads the ID3v2 tag instead of the whole MP3 file.
# cmp "$(TEST_FILE)" id3parse-test.mp3

.PHONY: all
all: $(ALL_BEAMS)

.PHONY: help
help:
	@echo TEST_FILE=$(TEST_FILE)

ebin/%.beam: src/%.erl
	erlc $(ERLC_FLAGS) "$<"

CLEAN_FILES += id3parse-test.mp3
id3parse-test.mp3: GNUmakefile $(ALL_BEAMS)
	erl -noshell -pa ebin -s id3v2 test "$(TEST_FILE)" -s init stop

CLEAN_FILES += id3parse-test.dump
id3parse-test.dump: id3parse-test.mp3
	hexdump -C "$<" | sed '/00 00 00 ff fb/q' > "$@"

CLEAN_FILES += orig.dump
orig.dump: GNUmakefile
	hexdump -C "$(TEST_FILE)" | sed '/00 00 00 ff fb/q' > "$@"

.PHONY: clean
clean:
	rm -f $(CLEAN_FILES)
