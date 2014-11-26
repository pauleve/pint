
#include "pint_gdr_wrap.h"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>

#include <stdio.h>

#define _CLOSURE(mlfunc) static value* mlfunc ## _closure = NULL; \
			if (mlfunc ## _closure == NULL) mlfunc ## _closure = caml_named_value(#mlfunc);

void ph_parse(const char* filename, value* ph, value* ctx) { 
	CAMLparam0 ();
	_CLOSURE(ph_parse)
	CAMLlocal1 (caml_filename);
	caml_filename = caml_copy_string(filename);
	value r = caml_callback(*ph_parse_closure, caml_filename);
	*ph = Field(r, 0);
	*ctx = Field(r, 1);
	CAMLreturn0;
}

value ph_init_env(value ph, const char* a, int i) {
	CAMLparam1 (ph);
	CAMLlocal1 (a_);
	a_ = caml_copy_string(a);
	_CLOSURE(ph_init_env);
	CAMLreturn (caml_callback3(*ph_init_env_closure, ph, a_, Val_int(i)));
}

