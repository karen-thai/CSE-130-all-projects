
#####################################################################################################
COURSE=cs130sp19
ASGN=05
NAME=classes
STACK=stack --allow-different-user
BUILD_OPTS=--ghc-options -O0 
#####################################################################################################

test: clean
	$(STACK) test $(BUILD_OPTS)

bin:
	$(STACK) build $(BUILD_OPTS)

repl: 
	$(STACK) run

clean: 
	$(STACK) clean
	rm -rf tests/input/*.log

distclean: clean 
	rm -rf .stack-work 

tags:
	hasktags -x -c src/

turnin:
	git commit -a -m "turnin"
	git push origin master

upstream:
	git remote add upstream https://github.com/ucsd-cse130/05-classes.git

update:
	git pull upstream master

ghci:
	$(STACK) exec -- ghci
