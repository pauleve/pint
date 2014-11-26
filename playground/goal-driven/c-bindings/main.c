
#include "pint_gdr_wrap.h"

#include <stdio.h>
#include <string.h>

int main(int argc, char* argv[]) {

	if (argc != 6) {
		fprintf(stderr, "Usage: %s <file.ph> <a> <i> <b> <j>\n", argv[0]);
		return 1;
	}

	caml_startup(argv);

	char* filename = argv[1];
	char* a = argv[2];
	int i = atoi(argv[3]);
	char* b = argv[4];
	int j = atoi(argv[5]);

	value ph, ctx;

	ph_parse(filename, &ph, &ctx);
	value env = ph_init_env(ph, a, i);
	value glc = ph_worth_glc(env, ctx);
	int w = ph_is_worth(glc, b, j);
	fprintf(stdout, "worth to reach %s %d? %d\n", b, j, w);

	return 0;
}

