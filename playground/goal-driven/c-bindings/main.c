
#include "pint_gdr_wrap.h"

#include <stdio.h>
#include <string.h>

int main(int argc, char* argv[]) {

	if (argc != 2) {
		fprintf(stderr, "Usage: %s <file.ph>\n", argv[0]);
		return 1;
	}

	caml_startup(argv);

	char* filename = strdup(argv[1]);

	fprintf(stdout, "opening %s\n", filename);
	value ph_ctx = ph_parse(filename);
	value ph = Field(ph_ctx, 0);
	int size = ph_size(ph);
	fprintf(stdout, "ph as %d transitions\n", size);

	return 0;
}

