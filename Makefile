LINK=./bin/link
BREW=./bin/brew-setup

all: link brew

link: ~*
	@$(LINK) $^

brew:
	@$(BREW)
