MMC      := mmc
DEL_FILE := rm -f
DEL_DIR  := rm -fR

TEST_MODULES := $(patsubst %.m,%,$(wildcard test_*.m))

.PHONY: all
all: $(TEST_MODULES)

test_%: test_%.m generic_*.m
	$(MMC) -m $@

.PHONY: clean
clean:
	$(foreach TEST,$(TEST_MODULES),$(MMC) -m $(TEST).clean;)

.PHONY: realclean
realclean:
	$(foreach TEST,$(TEST_MODULES), \
		$(MMC) -m $(TEST).realclean; $(DEL_FILE) $(TEST).bat;)
	$(DEL_DIR) Mercury
