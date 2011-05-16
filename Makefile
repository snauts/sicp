all:
	make scheme
	make sicp

%.com: %.scm
	mit-scheme --eval '(cf "$*")' --eval '(%exit)'

sicp: chapter2.com query.com simulator.com pic-lang/pic-imag.com \
	pic-lang/pic-reco.com pic-lang/pic-read.com pic-lang/pic-ops.com \
	pic-lang/hend.com pic-lang/picture.com pic-lang/psgo.com \
	pic-lang/hutils.com pic-lang/prmpnt.com pic-lang.com \
	load.com chapter3.com chapter5.com lazy.com chapter1.com \
	evaluator.com parallel.com chapter4.com amb.com lazy-eceval.com \
	eceval.com regsim.com analyze.com constraint.com compiler.com \
	eceval-ok.com compile-eval.com
	@echo SICP compile done

scheme: scheme.c scheme.h
	gcc scheme.c -Wall -Wextra -pedantic -std=c99 -O2 -o scheme

scheme.h:
	mit-scheme \
		--eval "(define nil '())" \
		--eval '(load "evaluator")' \
		--eval '(load "compiler")' \
		--eval '(load "c_compiler")' \
		--eval '(%exit)'

test: scheme
	./scheme < test.scm

run:
	mit-scheme --eval '(load "load")' --eval '(%exit)'

pic:
	mit-scheme --eval '(load "pic-lang")'

clean:
	find . -name '*.bci' | xargs rm -f
	find . -name '*.com' | xargs rm -f
	find . -name '*.bin' | xargs rm -f
	rm scheme scheme.h -f
