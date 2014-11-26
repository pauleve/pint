
#include "pint_gdr_wrap.h"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>

#include <stdio.h>

#define _CLOSURE(mlfunc) static value* mlfunc ## _closure = NULL; \
			if (mlfunc ## _closure == NULL) mlfunc ## _closure = caml_named_value(#mlfunc);

value pint_ctx_set(value ctx, const char* a, int i) { _CLOSURE(pint_ctx_set)
	CAMLparam1 (ctx);
	CAMLlocal1 (a_);
	a_ = caml_copy_string(a);
	CAMLreturn (caml_callback3(*pint_ctx_set_closure, ctx, a_, Val_int(i)));
}

void ph_parse(const char* filename, value* ph, value* ctx) { _CLOSURE(ph_parse)
	CAMLparam0 ();
	CAMLlocal1 (caml_filename);
	caml_filename = caml_copy_string(filename);
	value r = caml_callback(*ph_parse_closure, caml_filename);
	*ph = Field(r, 0);
	*ctx = Field(r, 1);
	CAMLreturn0;
}

value ph_init_env(value ph, const char* a, int i) { _CLOSURE(ph_init_env)
	CAMLparam1 (ph);
	CAMLlocal1 (a_);
	a_ = caml_copy_string(a);
	CAMLreturn (caml_callback3(*ph_init_env_closure, ph, a_, Val_int(i)));
}

value ph_worth_glc(value env, value ctx) { _CLOSURE(ph_worth_glc)
	CAMLparam2 (env, ctx);
	CAMLlocal1 (glc);
	glc = caml_callback2(*ph_worth_glc_closure, env, ctx);
	CAMLreturn (glc);
}

int ph_is_worth(value glc, const char* a, int i) { _CLOSURE(ph_is_worth)
	CAMLparam1 (glc);
	CAMLlocal2 (a_, r);
	a_ = caml_copy_string(a);
	r = caml_callback3(*ph_is_worth_closure, glc, a_, Val_int(i));
	CAMLreturnT(int, Bool_val(r));
}


