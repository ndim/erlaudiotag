-include local.mk
TEST_FILE ?= does-not-exist.mp3

.PHONY: check
check: all id3parse-test.dump orig.dump
	colordiff -u orig.dump id3parse-test.dump | less -r

.PHONY: all
all: id3parse.beam

id3parse.beam: id3parse.erl
	erlc $(ERLC_OPT) "$<"

id3parse-test.mp3: GNUmakefile $(wildcard *.beam)
	erl -noshell -s id3parse test "$(TEST_FILE)" -s init stop

id3parse-test.dump: id3parse-test.mp3
	hexdump -C "$<" > "$@"

orig.dump: GNUmakefile
	hexdump -C "$(TEST_FILE)" | sed -n '1,/00 00 00 ff fb/p' > "$@"
