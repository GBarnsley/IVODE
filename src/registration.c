#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void deterministic_gz_initmod_desolve(void *);
extern void deterministic_gz_rhs_dde(void *);
extern void deterministic_gz_rhs_desolve(void *);
extern void deterministic_initmod_desolve(void *);
extern void deterministic_output_dde(void *);
extern void deterministic_rhs_dde(void *);
extern void deterministic_rhs_desolve(void *);

/* .Call calls */
extern SEXP deterministic_contents(void *);
extern SEXP deterministic_create(void *);
extern SEXP deterministic_gz_contents(void *);
extern SEXP deterministic_gz_create(void *);
extern SEXP deterministic_gz_initial_conditions(void *, void *);
extern SEXP deterministic_gz_metadata(void *);
extern SEXP deterministic_gz_rhs_r(void *, void *, void *);
extern SEXP deterministic_gz_set_initial(void *, void *, void *, void *);
extern SEXP deterministic_gz_set_user(void *, void *);
extern SEXP deterministic_initial_conditions(void *, void *);
extern SEXP deterministic_metadata(void *);
extern SEXP deterministic_rhs_r(void *, void *, void *);
extern SEXP deterministic_set_initial(void *, void *, void *, void *);
extern SEXP deterministic_set_user(void *, void *);

static const R_CMethodDef CEntries[] = {
    {"deterministic_gz_initmod_desolve", (DL_FUNC) &deterministic_gz_initmod_desolve, 1},
    {"deterministic_gz_rhs_dde",         (DL_FUNC) &deterministic_gz_rhs_dde,         1},
    {"deterministic_gz_rhs_desolve",     (DL_FUNC) &deterministic_gz_rhs_desolve,     1},
    {"deterministic_initmod_desolve",    (DL_FUNC) &deterministic_initmod_desolve,    1},
    {"deterministic_output_dde",         (DL_FUNC) &deterministic_output_dde,         1},
    {"deterministic_rhs_dde",            (DL_FUNC) &deterministic_rhs_dde,            1},
    {"deterministic_rhs_desolve",        (DL_FUNC) &deterministic_rhs_desolve,        1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"deterministic_contents",              (DL_FUNC) &deterministic_contents,              1},
    {"deterministic_create",                (DL_FUNC) &deterministic_create,                1},
    {"deterministic_gz_contents",           (DL_FUNC) &deterministic_gz_contents,           1},
    {"deterministic_gz_create",             (DL_FUNC) &deterministic_gz_create,             1},
    {"deterministic_gz_initial_conditions", (DL_FUNC) &deterministic_gz_initial_conditions, 2},
    {"deterministic_gz_metadata",           (DL_FUNC) &deterministic_gz_metadata,           1},
    {"deterministic_gz_rhs_r",              (DL_FUNC) &deterministic_gz_rhs_r,              3},
    {"deterministic_gz_set_initial",        (DL_FUNC) &deterministic_gz_set_initial,        4},
    {"deterministic_gz_set_user",           (DL_FUNC) &deterministic_gz_set_user,           2},
    {"deterministic_initial_conditions",    (DL_FUNC) &deterministic_initial_conditions,    2},
    {"deterministic_metadata",              (DL_FUNC) &deterministic_metadata,              1},
    {"deterministic_rhs_r",                 (DL_FUNC) &deterministic_rhs_r,                 3},
    {"deterministic_set_initial",           (DL_FUNC) &deterministic_set_initial,           4},
    {"deterministic_set_user",              (DL_FUNC) &deterministic_set_user,              2},
    {NULL, NULL, 0}
};

void R_init_IVODE(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
