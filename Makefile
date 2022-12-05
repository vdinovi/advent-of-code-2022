targets = $(shell \
	find . -type f -name 'Day*.hs' \
	| sed -n 's/.*\(Day[[:digit:]]\).*/\1/p')

.DEFAULT_GOAL: build
.PHONY: build run test clean

GREEN=\033[0;32m
RED=\033[0;31m
NC=\033[0m

build: $(targets)

run: $(addsuffix .run, $(targets))

test: $(addsuffix .test, $(targets))

clean:
	find . -type f -name "Day*" ! -name "*.*" -delete
	rm -f *.o *.hi

define make-target
.PHONY += $1.run $1.test $1.clean

$1: $1.hs
	@echo "==> Building $1"
	ghc $$^ -o $$@
	
$1.run: $1
	@echo "==> Running $1"
	./$$< inputs/$$<.txt

$1.test: $1
	@echo "==> Testing $1"
	@/bin/bash -c "diff <(./$1 inputs/$1.txt) <(cat outputs/$1.txt)" && \
		echo "[ $(GREEN)OK$(NC) ]" || \
		echo "[ $(RED)FAIL$(NC) ]"

$1.clean:
	@echo "==> Cleaning $1"
	rm -f $1 $1.o $1.hi
endef
$(foreach t,$(targets),$(eval $(call make-target,$(t))))

