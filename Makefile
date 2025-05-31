RUST_SRC := $(wildcard src/*)
ALEXC     = target/release/alexc
CC        = gcc

$(ALEXC): $(RUST_SRC)
	@echo "[cargo] Building $(ALEXC)"
	cargo build --release > /dev/null

%.s: %.ac $(ALEXC)
	@echo "[alexc] Compiling $< to $*.s"
	$(ALEXC) $< -o $*.s

%: %.s
	@echo "[gcc] Assembling and Linking $*.s to $*"
	$(CC) -no-pie -nostartfiles -o $@.out $*.s

# Clean rule to remove all build artifacts
.PHONY: clean
clean:
	rm -f *.s *.out