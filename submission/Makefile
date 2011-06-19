date = $(shell date +"%Y%m%d_%H%M%S")

all: sha

sha: tar.update
	@echo
	@echo "Generated file fikdm$(date).tar.gz for submission with SHA-512 hash"
	@echo "$(shell sha512sum fikdm$(date).tar.gz | sed 's/\W.*//')"
	@echo "Don't forget to copy it to http://www.tilab.tuwien.ac.at/~heinz/fikdm11/fikdm$(date).tar.gz"
	@echo
	@rm -f tar.update

tar.update:
	@[ -s src/compiler ] || ln -s ../../compiler src/compiler
	@[ -s src/interpreter ] || ln -s ../../interpreter src/interpreter
	@[ -s src/vis ] || ln -s ../../vis src/vis
	@[ -s functions ] || ln -s ../functions functions
	@tar -h -czf fikdm$(date).tar.gz install run functions README src
	@touch tar.update
	@echo "created fikdm$(date).tar.gz"

clean:
	rm -f tar.update 
