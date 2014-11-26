#include <caml/mlvalues.h>

value pint_ctx_set(value ctx, const char* a, int i);

void ph_parse(const char* filename, value* ph, value* ctx);

value ph_init_env(value ph, const char* a, int i);

value ph_worth_glc(value env, value ctx);

int ph_is_worth(value glc, const char* a, int i);

