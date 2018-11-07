EXCLUDE := .git . .. .gitignore .DS_Store
ALL_FILES := $(filter-out $(EXCLUDE), $(wildcard ~*))
LINK=./bin/link
BREW=./bin/brew-setup

all: link brew

link: $(ALL_FILES)
	@$(LINK) $^

brew:
	@$(BREW)
