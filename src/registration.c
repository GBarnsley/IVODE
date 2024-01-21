#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void dynamic_model_initmod_desolve(void *);
extern void dynamic_model_rhs_dde(void *);
extern void dynamic_model_rhs_desolve(void *);
extern void static_model_initmod_desolve(void *);
extern void static_model_rhs_dde(void *);
extern void static_model_rhs_desolve(void *);

/* .Call calls */
extern SEXP dynamic_model_contents(void *);
extern SEXP dynamic_model_create(void *);
extern SEXP dynamic_model_initial_conditions(void *, void *);
extern SEXP dynamic_model_metadata(void *);
extern SEXP dynamic_model_rhs_r(void *, void *, void *);
extern SEXP dynamic_model_set_initial(void *, void *, void *, void *);
extern SEXP dynamic_model_set_user(void *, void *);
extern SEXP static_model_contents(void *);
extern SEXP static_model_create(void *);
extern SEXP static_model_initial_conditions(void *, void *);
extern SEXP static_model_metadata(void *);
extern SEXP static_model_rhs_r(void *, void *, void *);
extern SEXP static_model_set_initial(void *, void *, void *, void *);
extern SEXP static_model_set_user(void *, void *);

static const R_CMethodDef CEntries[] = {
    {"dynamic_model_initmod_desolve", (DL_FUNC) &dynamic_model_initmod_desolve, 1},
    {"dynamic_model_rhs_dde",         (DL_FUNC) &dynamic_model_rhs_dde,         1},
    {"dynamic_model_rhs_desolve",     (DL_FUNC) &dynamic_model_rhs_desolve,     1},
    {"static_model_initmod_desolve",  (DL_FUNC) &static_model_initmod_desolve,  1},
    {"static_model_rhs_dde",          (DL_FUNC) &static_model_rhs_dde,          1},
    {"static_model_rhs_desolve",      (DL_FUNC) &static_model_rhs_desolve,      1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"dynamic_model_contents",           (DL_FUNC) &dynamic_model_contents,           1},
    {"dynamic_model_create",             (DL_FUNC) &dynamic_model_create,             1},
    {"dynamic_model_initial_conditions", (DL_FUNC) &dynamic_model_initial_conditions, 2},
    {"dynamic_model_metadata",           (DL_FUNC) &dynamic_model_metadata,           1},
    {"dynamic_model_rhs_r",              (DL_FUNC) &dynamic_model_rhs_r,              3},
    {"dynamic_model_set_initial",        (DL_FUNC) &dynamic_model_set_initial,        4},
    {"dynamic_model_set_user",           (DL_FUNC) &dynamic_model_set_user,           2},
    {"static_model_contents",            (DL_FUNC) &static_model_contents,            1},
    {"static_model_create",              (DL_FUNC) &static_model_create,              1},
    {"static_model_initial_conditions",  (DL_FUNC) &static_model_initial_conditions,  2},
    {"static_model_metadata",            (DL_FUNC) &static_model_metadata,            1},
    {"static_model_rhs_r",               (DL_FUNC) &static_model_rhs_r,               3},
    {"static_model_set_initial",         (DL_FUNC) &static_model_set_initial,         4},
    {"static_model_set_user",            (DL_FUNC) &static_model_set_user,            2},
    {NULL, NULL, 0}
};

void R_init_IVODE(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
