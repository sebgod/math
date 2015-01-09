MMC      := mmc
DEL_FILE := rm -f
DEL_DIR  := rm -fR

TEST_MODULES := $(patsubst %.m,%,$(wildcard test_*.m))

.PHONY: all
all: $(TEST_MODULES) .gitignore

test_%: test_%.m generic_*.m
	$(MMC) -m $@

.%: .%-tmpl
	cp -f $< $@
	@echo "">>$@
	echo "# executables">>$@
	@$(foreach TEST,$(TEST_MODULES), \
		echo $(TEST) >>$@; echo $(TEST).bat >>$@;)

.PHONY: clean
clean:
	$(foreach TEST,$(TEST_MODULES),$(MMC) -m $(TEST).clean;)

.PHONY: realclean
realclean:
	$(foreach TEST,$(TEST_MODULES), \
		$(MMC) -m $(TEST).realclean; $(DEL_FILE) $(TEST).bat;)
	$(DEL_DIR) Mercury
