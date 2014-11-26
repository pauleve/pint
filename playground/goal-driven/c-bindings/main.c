
#include "pint_gdr_wrap.h"

#include <stdio.h>
#include <string.h>

int main(int argc, char* argv[]) {

	if (argc != 4) {
		fprintf(stderr, "Usage: %s <file.ph> <a> <i>\n", argv[0]);
		return 1;
	}

	caml_startup(argv);

	char* filename = argv[1];
	char* a = argv[2];
	int i = atoi(argv[3]);

	value ph, ctx;

	fprintf(stdout, "opening %s\n", filename);
	ph_parse(filename, &ph, &ctx);
	fprintf(stdout, "init_env with a=%s, i=%d\n", a, i);
	value env = ph_init_env(ph, a, i);
	fprintf(stdout, "done\n");

	return 0;
}

