# Compiler and tools
ALEXC     = target/release/alexc
CC        = gcc

# Build rule: make alexc binary if it doesn't already exist
$(ALEXC):
	cargo build --release

# Build rule: make <filename> builds <filename>.ac → .s → .out
%: %.ac
	@echo "[alexc] Compiling $< to $*.s"
	$(ALEXC) $< -o $*.s

	@echo "[gcc] Assembling and Linking $*.s to $*"
	$(CC) -no-pie -nostartfiles -o $@.out $*.s

# Clean rule to remove all build artifacts
.PHONY: clean
clean:
	rm -f *.s *.out