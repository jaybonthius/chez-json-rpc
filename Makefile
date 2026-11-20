SCHEME ?= scheme
CAP ?=
JSON_DIR ?= ../chez-json
OBJ := build/obj
DEPENDENCIES := $(JSON_DIR)/build/obj/src
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib:$(DEPENDENCIES)
RUN := $(SCHEME) --libdirs $(LIBDIRS) --script tests/run.ss
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 600
TESTS := $(sort $(wildcard tests/*-test.sps))

.PHONY: deps build test clean $(TESTS:%=run/%)

deps:
	$(MAKE) -C $(JSON_DIR) SCHEME=$(SCHEME) CAP='$(CAP)' build

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build:
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --libdirs $(DEPENDENCIES) --script build.ss

# One process per test file, so a crash or leaked global state stays inside
# its file.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$(*:.sps=.so)

clean:
	rm -rf build
