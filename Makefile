.PHONY: read_data read_data_norev gen_hddmrl_data clean_data all clean

all: read_data read_data_norev clean_data clean_data_rev_norev gen_hddmrl_data

read_data:
	$(MAKE) -f makefiles/Makefile_read_data

read_data_norev:
	$(MAKE) -f makefiles/Makefile_read_data_norev

gen_hddmrl_data:
	$(MAKE) -f makefiles/Makefile_gen_hddmrl_data

clean_data:
	$(MAKE) -f makefiles/Makefile_clean_data

clean_data_rev_norev:
	$(MAKE) -f makefiles/Makefile_clean_data_rev_norev

clean:
	$(MAKE) -f makefiles/Makefile_read_data clean
	$(MAKE) -f makefiles/Makefile_read_data_norev clean
	$(MAKE) -f makefiles/Makefile_clean_data clean
	$(MAKE) -f makefiles/Makefile_clean_data_rev_norev clean
	$(MAKE) -f makefiles/Makefile_gen_hddmrl_data clean
