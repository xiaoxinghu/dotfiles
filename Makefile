EXCLUDE := .git . .. .gitignore .DS_Store
ALL_FILES := $(filter-out $(EXCLUDE), $(wildcard .*) Library)
LINK=./bin/link

link: $(ALL_FILES)
	@$(LINK) $^
