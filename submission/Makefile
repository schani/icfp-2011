date = $(shell date +"%Y%m%d_%H%M%S")

all: tar sha

sha: tar
	@echo
	@echo "Generated file fikdm$(date).tar.gz for submission with SHA-512 hash"
	@echo "$(shell sha512sum fikdm$(date).tar.gz | sed 's/\W.*//')"
	@echo "Don't forget to copy it to http://www.tilab.tuwien.ac.at/~heinz/fikdm11/"
	@echo

tar:
	@tar -czf fikdm$(date).tar.gz install run README src

clean:
	rm -f fikdm*.tar.gz 
