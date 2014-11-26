
#include "pint_gdr_wrap.h"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>

#include <stdio.h>

#define _CLOSURE(mlfunc) static value* mlfunc ## _closure = NULL; \
			if (mlfunc ## _closure == NULL) mlfunc ## _closure = caml_named_value(#mlfunc);

value ph_parse(const char* filename) { 
	CAMLparam0 ();
	_CLOSURE(ph_parse)
	CAMLlocal1 (caml_filename);
	caml_filename = caml_copy_string(filename);
	CAMLreturn (caml_callback(*ph_parse_closure, caml_filename));
}

int ph_size(value ph) { _CLOSURE(ph_size)
	return Int_val(caml_callback(*ph_size_closure, ph));
}

