.PHONY: read_data clean_data all clean

all: clean_data

read_data:
	$(MAKE) -f makefiles/Makefile_read_data

clean_data:
	$(MAKE) -f makefiles/Makefile_clean_data

clean:
	$(MAKE) -f makefiles/Makefile_read_data clean

