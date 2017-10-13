CLASSDIR = classes
SRCPATH = sources
ENTRY = PlayGame
JC = javac
JR = java
TEMP := $(shell mktemp /tmp/make_files.XXXXXX)
JFLAGS = -cp $(CLASSDIR) -d $(CLASSDIR)
JFILES = $(shell find $(SRCPATH) -name "*.java")
CFILES = $(patsubst $(SRCPATH)%.java,$(CLASSDIR)%.class,$(JFILES))
.SUFFIXES: .java .class

default: $(CFILES)
	@if [ -s $(TEMP) ]; then \
		cat $(TEMP) && $(JC) $(JFLAGS) @$(TEMP); \
	else \
		echo "All classes are up-to-date"; \
	fi;
	@rm $(TEMP)

$(CLASSDIR)/%.class: $(SRCPATH)/%.java
	@echo $< >> $(TEMP)

run:
	$(JR) -cp $(CLASSDIR) $(ENTRY)

.PHONY: clean

clean:
	@find . -name "*.class" -exec rm "{}" \;
	@rm -rf $(CLASSDIR)/*
