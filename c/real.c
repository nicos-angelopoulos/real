#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#define CSTACK_DEFNS // to enable disabling of stack checks
#include <Rembedded.h>
#include <R.h>
#include <Rinternals.h>
// #if HAVE_RINTERFACE_H || !defined(_YAP_NOT_INSTALLED_) // nicos: commented out 14.4.21
#if !(__WINDOWS__)
#define R_INTERFACE_PTRS 1
#include <Rinterface.h>
#define R_SIGNAL_HANDLERS 1
#endif
#if (__WINDOWS__)
uintptr_t R_CStackLimit=(uintptr_t)-1;
#endif
#include <Rdefines.h>
#include <R_ext/Parse.h>
#include <assert.h>

#define REAL_LIST (63) // fixme: Remove once YAP start supporting PL_LIST_PAIR

static functor_t FUNCTOR_dot2;
static functor_t FUNCTOR_equal2;
static functor_t FUNCTOR_boolop1;
static functor_t FUNCTOR_plus1;
static atom_t ATOM_true;
static atom_t ATOM_false;
static atom_t ATOM_empty;
static atom_t ATOM_nan;

install_t install_real(void);

static int float_vector_sexp(term_t t, size_t len, SEXP *ansP);
static int pl_sexp(term_t t, SEXP *ansP);
static int put_sexp(term_t t, SEXP s);
static int NaN_value(term_t t);
static int matrix_type(term_t t);
static int vector_type(term_t t);
// static void bindRIO(void);  // Nicos: 17.11.22 bindRIO() disabled

static size_t
list_length(term_t list)
{ term_t tail = PL_new_term_ref();
  size_t len;

  switch(PL_skip_list(list, tail, &len))
  { case PL_LIST:
      return len;
    case PL_PARTIAL_LIST:
      PL_instantiation_error(list);
      return -1;
    case PL_CYCLIC_TERM:
    case PL_NOT_A_LIST:
      PL_type_error("list", list);
      return (size_t)-1;
    default:
      assert(0);
  }
}

static int
NaN_value( atom_t t ) {
  if ( t == ATOM_empty )
    return TRUE;
  if ( t == ATOM_nan )
    return TRUE;
  return FALSE;
}

/* REAL_term_type(term_t t)

   Returns the (REAL) term type of t.
   Note that on 14.12.25, we change semantics.
   we now introduce REAL_LIST which is returned when t is a list.
*/
static int
REAL_term_type( term_t t )
{ int objtype=PL_term_type(t);

    switch(objtype)
    {
      case PL_ATOM:
#ifdef PL_NIL
      case PL_NIL:
#endif
         { int got_v = 0;
           int bool_vP = 0;
           atom_t tmp_atom;

     if ( (got_v = PL_get_bool(t,&bool_vP)) )
               return PL_BOOL;

           if ( !PL_get_atom(t,&tmp_atom) )
                  PL_type_error("atom",t);

            if (tmp_atom==ATOM_true || tmp_atom==ATOM_false )
               return PL_BOOL;
             else
               return PL_ATOM;
        }
        break;
#ifdef PL_LIST_PAIR
      case PL_LIST_PAIR:
     return REAL_LIST;
      break;
#endif
      case PL_TERM:
         { //fixme: (nicos) booleans are all over the place...?!
            if ( PL_is_functor(t, FUNCTOR_dot2) )
          return REAL_LIST;
            if ( PL_is_functor(t, FUNCTOR_boolop1) )
              return PL_BOOL;
              // return PL_type_error("R-term (in type)", t);

           term_t arg1 = PL_new_term_ref();
           atom_t a;
           if ( !PL_get_arg(1, t, arg1) )
              return PL_type_error("R-term (in type, 2)", t);
           if ( !PL_get_atom(arg1,&a) )  // no way they can by T/F then
          return PL_TERM;
              // return PL_type_error("R-term (in type, 3)", t);
           if ( a == ATOM_true || a == ATOM_false )
              return PL_BOOL;
            else
               return PL_TERM;
         }
         break;
      default:
         return objtype;
    }
}

static int
logical_vector_sexp(term_t t, size_t len, SEXP *ansP)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  size_t index;
  SEXP ans;
  int nprotect = 0;

  PROTECT(ans=NEW_LOGICAL(len));
  nprotect++;

  for(index=0; PL_get_list(tail, head, tail); index++)
  { int val;

    if ( PL_get_bool(head, &val) )
    { LOGICAL_DATA(ans)[index] = val;
    } else
    {
       term_t arg1 = PL_new_term_ref();
       atom_t a;
       if (PL_is_functor(head,FUNCTOR_boolop1) && PL_get_arg(1,head,arg1) && PL_get_atom(arg1,&a)
               && (a == ATOM_true || a == ATOM_false)  )
          {
            if ( a == ATOM_true )
               LOGICAL_DATA(ans)[index] = 1;     // FIXME: use the R T/F values here
           else if (a == ATOM_false )
               LOGICAL_DATA(ans)[index] = 0;
           else                                // it should never come here
           { UNPROTECT(nprotect);
             return FALSE;
           }
          } else
          {
               /* FIXME? Destroy ans */
               UNPROTECT(nprotect);
              return FALSE;
          }
    }
  }

  UNPROTECT(nprotect);
  *ansP = ans;

  return TRUE;
}


/* int_vector_sexp(term_t +t, size_t +len, SEXP +Rvar)

     Transfer Prolog perceived integers list to Rvar.

*/
static int
int_vector_sexp(term_t t, size_t len, SEXP *ansP)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  size_t index;
  SEXP ans;
  int nprotect = 0;
  atom_t tmp_atom;

  PROTECT(ans=NEW_INTEGER(len));
  nprotect++;

  for(index=0; PL_get_list(tail, head, tail); index++)
  { int64_t val;

    if ( PL_get_atom(head,&tmp_atom) && NaN_value(tmp_atom) ) {
                    INTEGER_DATA(ans)[index] = NA_INTEGER;
                }
   else if ( PL_get_int64(head, &val) )
             {
             INTEGER_DATA(ans)[index] = val;
             }
      else if ( PL_is_float(head) )
                  { /* FIXME: Destroy ans */
                         return float_vector_sexp(t, len, ansP);
                  }
         else
                  { /* FIXME: Destroy ans */
                         return FALSE;
                  }
  }

  UNPROTECT(nprotect);
  *ansP = ans;

  return TRUE;
}

static int
float_vector_sexp(term_t t, size_t len, SEXP *ansP)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  size_t index;
  SEXP ans;
  int nprotect = 0;
  atom_t tmp_atom;

  PROTECT(ans=NEW_NUMERIC(len));
  nprotect++;

  for(index=0; PL_get_list(tail, head, tail); index++)
  { double val;

    if ( PL_get_atom(head,&tmp_atom) && NaN_value(tmp_atom) ) {
                    NUMERIC_DATA(ans)[index] = NA_REAL;
                }
   else
      if ( PL_get_float_ex(head, &val) )
             { NUMERIC_DATA(ans)[index] = val;
             } else
                { /* FIXME: Destroy ans */
                return FALSE;      /* type error */
             }
  }

  UNPROTECT(nprotect);
  *ansP = ans;

  return TRUE;
}

static int
char_vector_sexp(term_t t, size_t len, SEXP *ansP)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  size_t index;
  SEXP ans;
  int nprotect = 0;

  PROTECT(ans=NEW_CHARACTER(len));
  nprotect++;

  for(index=0; PL_get_list(tail, head, tail); index++)
    { char *s;

    restart:
    if ( PL_get_chars(head, &s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_DISCARDABLE|REP_UTF8) )
      { CHARACTER_DATA(ans)[index] = mkCharCE(s, CE_UTF8);
    } else if (PL_is_functor(head,FUNCTOR_plus1))
      { if ( !PL_get_arg(1, head, head) )
    return PL_type_error("R-term (in char vect, 2)", head);

      goto restart;
    } else if (PL_is_functor(head,FUNCTOR_boolop1))
    { term_t arg1 = PL_new_term_ref();
      atom_t a;

      if ( !PL_get_arg(1, head, arg1) )
       return PL_type_error("R-term (in char vect, 2)", head);

      if ( ! PL_get_atom(arg1,&a) )
            return PL_type_error("R-term (char vect, 3)", head);

      if (a == ATOM_true)
       CHARACTER_DATA(ans)[index] = mkChar("true");
        else
          if (a == ATOM_false)
           CHARACTER_DATA(ans)[index] = mkChar("false");
          else
            CHARACTER_DATA(ans)[index] = mkCharCE(s, CE_LATIN1);
                 // could also use this :
                 // return PL_type_error("@atom", t );
    } else
       return FALSE;
  }

  UNPROTECT(nprotect);
  *ansP = ans;

  return TRUE;
}

//nicos: 2014/08/19   support x <- [c(1,2,3),c(4,5,6)].
/* term_length(term_t +t)

    returns the length of list t or arity of term t.

   for now it errors if t is none of the above.
   for our purposes, this is just fine. this is always called
   after we established it is going to be one of the two.

   in general everything else should have length 1.
   variables maybe 0?

*/
static int
term_length(term_t t)
{
  size_t ans = 0;
  term_t tname;
  int objtype;
  objtype = REAL_term_type(t);

  switch(objtype)
  {
      case REAL_LIST:
           return list_length(t);
        break;
      case PL_LIST_PAIR:
           return list_length(t);
        break;
      case PL_TERM:
           if (!PL_get_name_arity(t, &tname, &ans) || ans<= 0) {
               return FALSE;
           };
           return ans;
       break;
     default:
            return PL_type_error("List or term expected", t);
  }
  return FALSE;
}

// nicos: YAP's version of PL_get_arg/3 does not fail when arity < index
static int
REAL_PL_get_arg(int i, term_t cols, term_t cell)
{  size_t  arity;
   term_t  name;

  if ( !PL_get_name_arity(cols, &name, &arity) || arity <=0 || arity < i   )
     return FALSE;

  return PL_get_arg( i, cols, cell );
}

//
static int
REAL_PL_get_nth(int rowtype, int col_i, term_t cols, term_t cell )
{
      // term_t a = PL_new_term_ref();
  switch(rowtype)
  {
    //#ifdef PL_LIST_PAIR
    case REAL_LIST:
      return PL_get_list( cols, cell, cols);
    // #endif
    case PL_TERM:
      /* if (PL_is_functor(cols, FUNCTOR_dot2)) {
        return PL_get_list( cols, cell, cols );
      };
      */
      return PL_get_arg(col_i, cols, cell); //  && PL_unify(a,cell);
      // return REAL_PL_get_arg( col_i, cols, a) && PL_unify(a,cell);
     default:
      return PL_type_error("Wrong row type in matrix", cols);

  }
}

static int
named_list_sexp(term_t t, size_t len, SEXP *ansP)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  term_t name = PL_new_term_ref();
  term_t value = PL_new_term_ref();
  size_t index;
  SEXP ans, names;
  int nprotect = 0;

  PROTECT(ans=NEW_LIST(len));
  nprotect++;
  names = allocVector(STRSXP, len);

  for(index=0; PL_get_list(tail, head, tail); index++)
  { if ( PL_is_functor(head, FUNCTOR_equal2) )
    { char *nm = NULL;
      SEXP sexp;

      if ( PL_get_arg(1, head, name) &&
     PL_get_arg(2, head, value) &&
     PL_get_chars(name, &nm, CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_MALLOC|REP_UTF8) &&
     pl_sexp(value, &sexp) )
  { SET_STRING_ELT(names, index, mkCharCE(nm, CE_UTF8));
  SET_ELEMENT(ans, index, sexp);
  PL_free(nm);
      } else
      { /* FIXME: Destroy ans and names */
  if (nm) PL_free(nm);
  return FALSE;
      }
    } else
    { /* FIXME: Destroy ans and names */
      return PL_type_error("R-pair", head);
    }
  }

  SET_NAMES(ans, names);
  UNPROTECT(nprotect);
  *ansP = ans;

  return TRUE;
}

/* vector_type(term_t +t)
    returns the type of first non NA element in the vector.
*/
static int
vector_type(term_t t)
{
  int    valstype, vtype, count_i;
  term_t vals, cell;
  atom_t tmp_atom;

  cell = PL_new_term_ref();
  vals = PL_copy_term_ref(t);

  for(count_i=1; REAL_PL_get_nth(REAL_LIST, count_i, vals, cell); count_i++) {
      if ( PL_get_atom(cell,&tmp_atom) ) {
        if (!NaN_value(tmp_atom)) {
           return REAL_term_type(cell);
        }
      }  else {
           return REAL_term_type(cell);
      }
  }
  return FALSE;
}


/* matrix_type(term_t +t)
    returns the type of first non NA element in the matrix.
*/
static int
  matrix_type(term_t t)
{
  term_t rows, cols, cell;
  int colstype, mtype, row_i, col_i;
   atom_t tmp_atom;
    cell = PL_new_term_ref();
   cols = PL_new_term_ref();
  rows = PL_copy_term_ref(t);
  for (row_i=0; PL_get_list(rows, cols, rows); row_i++)
  {
      colstype = REAL_term_type(cols);
      for(col_i=1; REAL_PL_get_nth(colstype, col_i, cols, cell); col_i++) {
      if ( PL_get_atom(cell,&tmp_atom) ) {
      if (!NaN_value(tmp_atom) ) {
        return REAL_term_type(cell);
        }
    }  else {
          return REAL_term_type(cell);
    }
    }
  }
  return FALSE;
}

static int
matrix_sexp(term_t t, term_t head, size_t len, int itype, SEXP *ansP)
{ // int headlen = list_length(head);
  int headlen = term_length(head);  // to support [c(1,2,3),c(4,5,6)]
  SEXP ans;
  int nprotect = 0;
  int colstype;

  if ( headlen >= 0 )      /* head is a proper list */
  { term_t cell, cols, rows; // , waste;
    int row_i, col_i;
    // size_t row_i, col_i;
    int objtype;
    cell = PL_new_term_ref();
    // waste = PL_new_term_ref();
    // if ( !PL_get_list(head, cell, waste) && !PL_get_arg(1,head,cell) )
    if ( !PL_get_head(head, cell ) && !PL_get_arg(1,head,cell) )
      return PL_type_error("Prolog matrix", t);

    if ( itype )
        objtype = itype;
      else
        // objtype = REAL_term_type(cell);
      {
      objtype = matrix_type(t);
      };

    switch(objtype)
    { case PL_VARIABLE:
  return PL_instantiation_error(cell);
      case PL_INTEGER:
  PROTECT(ans = allocMatrix(INTSXP, len, headlen));
        break;
      case PL_FLOAT:
  PROTECT(ans = allocMatrix(REALSXP, len, headlen));
        break;
      case PL_ATOM:
      case PL_STRING:
  PROTECT(ans = allocMatrix(STRSXP, len, headlen));
        break;
      case PL_BOOL:
  PROTECT(ans = allocMatrix(LGLSXP, len, headlen));
        break;
      case PL_TERM:
  return PL_type_error("Nested list of level two", t);
      default:
  assert(0);
    }

    cols = PL_new_term_ref();
    rows = PL_copy_term_ref(t);
    atom_t tmp_atom;

    for(row_i=0; PL_get_list(rows, cols, rows); row_i++)
    { colstype = REAL_term_type(cols); //fixme: should we do this per cell?

     if ( term_length(cols) != headlen )   // was list_length
      { /*FIXME: call the doctor*/;
         printf( "Warning: mismatch of matrix column lengths \n" );
      };

      for(col_i=1; REAL_PL_get_nth(colstype, col_i, cols, cell); col_i++)
      { size_t index = (col_i -1)*len + row_i;
          switch(objtype)
          { case PL_INTEGER:
             { int64_t val;
               if ( PL_get_int64(cell, &val) )
                  {
                   INTEGER_DATA(ans)[index] = val;
                  } else
                 {
                     // if ( PL_get_atom(cell,&tmp_atom) && tmp_atom == ATOM_na )
                     if ( PL_get_atom(cell,&tmp_atom) && NaN_value(tmp_atom) ) {
                  INTEGER_DATA(ans)[index] = NA_INTEGER;
                // INTSXP: NA_INTEGER
                     // LGLSXP: NA_LOGICAL
                     // STRSXP: NA_STRING
                      } else
                       { /* FIXME: deallocate work */
                         // return FALSE;
                          objtype=PL_term_type(cell);
                          switch(objtype)
                              { case PL_FLOAT:
                                      /* FIXME: deallocate work */
                                       return matrix_sexp(t, head, len, objtype,  ansP);
                                default:
                                     return FALSE;

                              }
                      }
                  }
            }
             break;
            case PL_FLOAT:
             { double val;
               if ( PL_get_float(cell, &val) )
               { NUMERIC_DATA(ans)[index] = val;
               } else
               { /* FIXME: deallocate work */
                  // if ( PL_get_atom(cell,&tmp_atom) && tmp_atom == ATOM_na )
                  if ( PL_get_atom(cell,&tmp_atom) && NaN_value(tmp_atom) ) {
                    NUMERIC_DATA(ans)[index] = NA_REAL;
                  } else {
                    return FALSE;
                  }
               }
             }
              break;
            case PL_ATOM:
            case PL_STRING:
             { char *s;
                if ( PL_get_chars(cell, &s,
             CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_DISCARDABLE|REP_UTF8) )
                 {
                    CHARACTER_DATA(ans)[index] = mkCharCE(s, CE_UTF8);
                 } else
                 { /* FIXME: deallocate work */
                     if ( PL_is_functor(cell, FUNCTOR_boolop1) )
                           { term_t arg1 = PL_new_term_ref();
                             atom_t a;
                             if ( !PL_get_arg(1, cell, arg1) )
                                return PL_type_error("R-term (in type, 2)", cell);
                             if ( ! PL_get_atom(arg1,&a) )
                                return PL_type_error("R-term (in type, 3)", cell);
                             if (a == ATOM_true)
                                CHARACTER_DATA(ans)[index] = mkChar("true");
                             else
                                 if (a == ATOM_false)
                                     CHARACTER_DATA(ans)[index] = mkChar("false");
                                 else
                           return PL_type_error("atom or codes", cell);

                           } else
                                 return FALSE;
                 }
             }
              break;
            case PL_BOOL:
             { int val;
                //  if ( (got_v = PL_get_bool(t,&bool_vP)) )
               if ( PL_get_bool_ex(cell, &val) )
                     LOGICAL_DATA(ans)[index] = val;
               else
                     /* FIXME: deallocate work */
                     return FALSE;
              }
              break;
            default:
                 assert(0);
          }
        }
    }
    UNPROTECT(nprotect);
    *ansP = ans;
    return TRUE;
  }
  return FALSE;
}


// fixme: (nicos) incorporate NaN values in vectors
/* pl_sexp( +Term, +Rvar ).
       transfer data (Term) from Prolog to R
*/
static int
pl_sexp(term_t t, SEXP *ansP)
{ int nprotect = 0;
  SEXP ans = R_NilValue;

  switch(REAL_term_type(t))
  { case PL_VARIABLE:
      return PL_type_error("R-term", t);
    case PL_ATOM:
    case PL_STRING:
    { char *s;

      if ( PL_is_functor(t, FUNCTOR_boolop1) )
      {
        if ( !PL_get_arg(1, t, t) ) return PL_type_error("R-term (in pl_sexp, 9)", t);
      }

      if ( PL_get_chars(t, &s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_DISCARDABLE|REP_UTF8) )
      { PROTECT(ans = NEW_CHARACTER(1));
       nprotect++;
       CHARACTER_DATA(ans)[0] = mkCharCE(s, CE_UTF8);
      }
      break;
    }
    case PL_INTEGER:
    { int64_t val;

      if ( PL_get_int64_ex(t, &val) )
      { PROTECT(ans = NEW_INTEGER(1));
  nprotect++;
  INTEGER_DATA(ans)[0] = val;
      } else
  return FALSE;

      break;
    }
    case PL_FLOAT:
    { double val;

      if ( PL_get_float_ex(t, &val) )
      { PROTECT(ans = NEW_NUMERIC(1));
  nprotect++;
  NUMERIC_DATA(ans)[0] = val;
      } else
  return FALSE;

      break;
    }
   case PL_BOOL:
     {  int val;
        if ( PL_get_bool_ex(t, &val) )
        { PROTECT(ans = NEW_LOGICAL(1));
          nprotect++; LOGICAL_DATA(ans)[0] = val;
        }
     break;
     }
    case REAL_LIST:
      { term_t head = PL_new_term_ref() ;
       size_t len = list_length(t);
  if (len == (size_t)-1)
    return FALSE; // fixme: (nicos) comment this, it doesn't seem obvious!?

  switch(vector_type(t))
  // switch(PL_term_type(head))
  { case PL_VARIABLE:
      return PL_instantiation_error(t);
    case PL_INTEGER:
      return int_vector_sexp(t, len, ansP);
    case PL_FLOAT:
      return float_vector_sexp(t, len, ansP);
    case PL_ATOM:
    case PL_STRING:
      return char_vector_sexp(t, len, ansP);
    case REAL_LIST:
    {
    if (!PL_get_head(t, head ))
      return FALSE; // fixme:
     return matrix_sexp(t, head, len, 0, ansP);
      break;
    }
    case PL_TERM:
	 {
    if (!PL_get_head(t, head ))
      return FALSE; // fixme:
      { if ( PL_is_functor(head, FUNCTOR_equal2) )
           return named_list_sexp(t, len, ansP);
        else if ( PL_is_functor(head, FUNCTOR_plus1) )
           return  char_vector_sexp(t, len, ansP);
        else
           {  REAL_PL_get_nth(PL_TERM, 1, t, head );
              return matrix_sexp(t, head, len, 0, ansP);
                           // return PL_type_error("R-termo", head);
           }
        break;
      }
	}
  case PL_BOOL:
     // {  int val; if ( PL_get_bool_ex(t, &val) ) { PROTECT(ans = NEW_LOGICAL(1)); nprotect++; LOGICAL_DATA(ans)[0] = val; } }
    return logical_vector_sexp( t, len, ansP );
    break;
  default:
             return PL_type_error("R-thermo-list", t);
    // assert(0);
   }
    break;
      }
    case PL_TERM:
   {
     if ( !PL_is_functor(t, FUNCTOR_boolop1) )
      { term_t arg1 = PL_new_term_ref();
         atom_t a;
          if ( !PL_get_arg(1, t, arg1) ) return PL_type_error("R-term (in pl_sexp, 2)", t);
          if ( !PL_get_atom(arg1,&a) ) return PL_type_error("R-term (in pl_sexp, 3)", t);
          return char_vector_sexp(arg1, 1, ansP);
        } else
             return PL_type_error("R-terma", t);
      break;
  }
    default:
      assert(0);
  }

  UNPROTECT(nprotect);
  *ansP = ans;

  return TRUE;
}

/*
static foreign_t
pl_rtest1(term_t t)
{ SEXP sexp;

  if ( pl_sexp(t, &sexp) )
  { PrintValue(sexp);

    return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_rtest2(term_t t, term_t out)
{ SEXP sexp;

  if ( pl_sexp(t, &sexp) )
  { term_t tmp = PL_new_term_ref();

    if ( put_sexp(tmp, sexp) )
      return PL_unify(out, tmp);
  }

  return FALSE;
}

PL_register_foreign("rtest",      1, pl_rtest1,         0);
PL_register_foreign("rtest",      2, pl_rtest2,         0);
*/


     /*******************************
     *     SEXP --> Prolog  *
     *******************************/


static int
sexp_rank(SEXP sexp)
{
  /* Return the number of dimensions for the buffer
   * (e.g., a vector will return 1, a matrix 2, ...)
   */
   /* Copied from rpy2 */
  SEXP dim = getAttrib(sexp, R_DimSymbol);
  if (dim == R_NilValue)
    return TRUE;
  return GET_LENGTH(dim);
}


/* Copied, with slight mods from rpy2 */
static int
sexp_shape(SEXP sexp, int nd, int *shape)
{
  /* Set 'shape', containing the size of each dimension (see sexp_rank).  */
  int i;
  SEXP dim = getAttrib(sexp, R_DimSymbol);
  if (dim == R_NilValue)
    shape[0] = LENGTH(sexp);
  else for (i = 0; i < nd; i++) {
      shape[i] = INTEGER(dim)[i];
    }
    return TRUE;
}


/* put_sexp( term_t -t, SEXP s)

   Put sexp s to term t.

*/
static int
put_sexp(term_t t, SEXP s)
{ int rank = sexp_rank(s);
  int shape[2];

  if ( rank > 2 )
    return PL_representation_error("multi-dimensional arrays");

  sexp_shape(s, rank, shape);

  switch (rank)
  { case 1:
    { int i;

      switch (TYPEOF(s))
      { case NILSXP:
    PL_put_nil(t);
    return TRUE;
        case SYMSXP:
    /* FIXME: Get culprit from s */
    return PL_existence_error("R-term", t);
        case REALSXP:
  { term_t head = PL_new_term_ref();
    term_t tail = PL_new_term_ref();

    PL_put_nil(tail);
    for (i = shape[0]-1; i>=0; i--)
   {
      if (NUMERIC_DATA(s)[i]==NA_REAL) {
        if (!PL_put_atom_chars(head,"$NaN"))  {
        return FALSE;
       }
     } else if ( !PL_put_float(head, NUMERIC_DATA(s)[i]) )
       return FALSE;
      if (!PL_cons_list(tail, head, tail) )
         return FALSE;
    }
    if ( !PL_put_term(t, tail) )
      return FALSE;
    break;
  }
  case INTSXP:
  { term_t head = PL_new_term_ref();
    term_t tail = PL_new_term_ref();

    PL_put_nil(tail);
    for (i = shape[0]-1; i>=0; i--)
   {
       if (INTEGER_DATA(s)[i]==NA_INTEGER) {
        if (!PL_put_atom_chars(head,"$NaN"))  {
        return FALSE;
     }
     } else if (!PL_put_int64(head, INTEGER_DATA(s)[i]))
        return FALSE;

    if (!PL_cons_list(tail, head, tail))
      return FALSE;
    }
    if ( !PL_put_term(t, tail) )
      return FALSE;
    break;
  }
  case LGLSXP:
  { term_t head = PL_new_term_ref();
    term_t tail = PL_new_term_ref();

    PL_put_nil(tail);
    for (i = shape[0]-1; i>=0; i--)
    { if ( !PL_put_variable(head) ||  /* TBD: All PL_put_bool() */
     !PL_unify_bool(head, LOGICAL_DATA(s)[i]) ||
     !PL_cons_list(tail, head, tail) )
        return FALSE;
    }
    if ( !PL_put_term(t, tail) )
      return FALSE;
    break;
  }
        case VECSXP:
  { SEXP names = GET_NAMES(s);
    term_t av = PL_new_term_refs(2);
    term_t head = PL_new_term_ref();
    term_t tail = PL_new_term_ref();

    PL_put_nil(tail);
    for (i = LENGTH(s)-1; i>=0; i--)
    { SEXP elem = VECTOR_ELT(s,i) ;

     /* R allows for unamed, named lists */
    /* fixme: temporary sticky tape */
    /* Currently we are passing back an integer, probably best to pass the atom of the integer */
     if (names == R_NilValue) {
        if (!PL_put_integer(av+0, i+1)) return FALSE;
       } else {
          if (!PL_put_atom_chars(av+0, CHAR(STRING_ELT(names,i)))) return FALSE;
      };

      if (
     /* !PL_put_atom_chars(av+0, CHAR(STRING_ELT(names,i))) || */

     /* !PL_put_atom_chars(av+0, nmcache[i]) || */
     !put_sexp(av+1, elem) ||
     !PL_cons_functor_v(head, FUNCTOR_equal2, av) ||
     !PL_cons_list(tail, head, tail) )
        return FALSE;
    }
    if ( !PL_put_term(t, tail) )
      return FALSE;
    break;
  }
        case STRSXP:
  { term_t tail = PL_new_term_ref();

    PL_put_nil(tail);
    for (i = shape[0]-1; i>=0; i--)
      { const char *chars = CHAR(CHARACTER_DATA(s)[i]);
        term_t head = PL_new_term_ref();

        if ( !PL_unify_chars(head, PL_ATOM|REP_UTF8, -1, chars) ||
     !PL_cons_list(tail, head, tail) )
        return FALSE;
    }
    if ( !PL_put_term(t, tail) )
      return FALSE;
    break;
  }
        default:
  { printf("unsupported r-type, with id: %d, %d \n", TYPEOF(s), NILSXP );
    return FALSE;        /* FIXME: error */
  }
      }
      if ( shape[0] == 1 )
      { if ( !PL_get_arg(1, t, t) )    /* Just return the head */
    assert(0);
      }
      break;
    }
    case 2:
    { SEXP adims = getAttrib(s, R_DimSymbol);
      int nrows = INTEGER(adims)[0];
      int ncols = INTEGER(adims)[1];
      term_t tail = PL_new_term_ref();
      term_t nest_tail = PL_new_term_ref();
      term_t nest_head = PL_new_term_ref();
      int i,j,c;

      PL_put_nil(tail);

      for (i = (nrows-1); i > -1 ; i--)
      { PL_put_nil(nest_tail);
  for (j=(ncols-1); j > -1 ; j--)
  { c = (j*nrows)+i;
      // { size_t index = col_i*len + row_i;

    switch (TYPEOF(s))
    { case REALSXP:
        if ( !PL_put_float(nest_head, NUMERIC_DATA(s)[c]) )
    return FALSE;
        break;
      case INTSXP:
        if (INTEGER_DATA(s)[c]==NA_INTEGER) {
        if (!PL_put_atom_chars(nest_head,"$NaN") )  {
              return FALSE;
            }
        } else {
           if ( !PL_put_int64(nest_head, INTEGER_DATA(s)[c]) )
            return FALSE;
          }
        break;
      case STRSXP:
        nest_head = PL_new_term_ref();
        if ( !PL_unify_chars(nest_head,  PL_ATOM|REP_UTF8, -1, CHAR(CHARACTER_DATA(s)[c])) )
    return FALSE;
        break;
      case LGLSXP:
        if ( !PL_put_variable(nest_head) ||
       !PL_unify_bool(nest_head, LOGICAL_DATA(s)[c]) )
    return FALSE;
        break;
    }
    if ( !PL_cons_list(nest_tail, nest_head, nest_tail) )
      return FALSE;
  }
  if ( !PL_cons_list(tail, nest_tail, tail) )
    return FALSE;
      }

      if ( !PL_put_term(t, tail) )
	return FALSE;
      break;
    }
    default:
      assert(0);
   }

   return TRUE;
}

     /*******************************
     *        START/END    *
     *******************************/

static foreign_t
init_R(void)
{ int argc = 2;
  char * argv[] = {"R", "--slave","--vanilla"};

  Rf_endEmbeddedR(0);

#if R_SIGNAL_HANDLERS
  R_SignalHandlers=0;
#endif
  Rf_initEmbeddedR(argc, argv);
  R_CStackLimit = (uintptr_t)-1; // SA 2015-03 added to disable stack checking
  // bindRIO();  // Nicos: 17.11.22  bindRIO() disabled

  return TRUE;
}

static foreign_t
stop_R(void)
{ Rf_endEmbeddedR(0);
  R_dot_Last();
  R_RunExitFinalizers();
  R_gc();

  return TRUE;
}


     /*******************************
     *   EXECUTE COMMAND  *
     *******************************/

static SEXP
process_expression(const char * expression)
{ SEXP e, tmp, val;
  int hadError;
  ParseStatus status;

  //  PROTECT(tmp = mkString(expression));
  PROTECT( tmp = ScalarString(mkCharCE(expression, CE_UTF8)) );
  PROTECT( e = R_ParseVector(tmp, 1, &status, R_NilValue) );

  if (status != PARSE_OK)
     {
     Sdprintf("Error: %d, in parsing R expression.\n", status );
     /* do not continue with R_tryEval() */
         UNPROTECT(2);
         /* PL_unify_term(except, PL_FUNCTOR_CHARS, "r_expression_syntax_error", 2, PL_CHARS, expression, PL_INTEGER, status ); */
                  /*FIXME: return the expression too (as atom) */
                  /* PL_FUNCTOR_CHARS, "r_expression_syntax_error", 2, PL_CHARS, "atom", PL_TERM, to; */
         /* return PL_raise_exception(except); */
         return NULL;
     }

  /* FIXME: Check status (nicos: it seems to be always 1 though? */
  // nicos: 17.11.22 added PROTECT aourd this:
  PROTECT( val = R_tryEval(VECTOR_ELT(e, 0), R_GlobalEnv, &hadError) );

  if ( !hadError )
    {
      UNPROTECT(3);
      return val;
    }

  UNPROTECT(3);
  return NULL;
}

static foreign_t
send_r_command(term_t cmd)
{ char *s = NULL;
  term_t except = PL_new_term_ref();

  if ( PL_get_chars(cmd, &s, CVT_ALL|REP_UTF8|CVT_EXCEPTION|BUF_MALLOC) )
    { if ( process_expression(s) ) {
  PL_free(s);
  return TRUE;
      }
      PL_free(s);
    if( PL_unify_term(except, PL_FUNCTOR_CHARS, "real_error", 1, PL_CHARS, "correspondence") )
      return PL_raise_exception(except) ;
    return FALSE;
    /* return PL_representation_error("Error in R\n"); */
  }

    Sdprintf("Error in get_nchars\n");      /* FIXME: Exception */
  return FALSE;
}

// fast copy of a Prolog vector to R
static foreign_t
send_c_vector(term_t tvec, term_t tout)
{ char *s;
  size_t arity, i;
  atom_t name;
  term_t targ = PL_new_term_ref();
  SEXP rho =  R_GlobalEnv, ans;

  if ( !PL_get_name_arity(tvec, &name, &arity) ||
       arity == 0) {
    return FALSE;
  }
  if ( !PL_get_atom_chars(tout, &s) ) {
    return FALSE;
  }
  _PL_get_arg(1, tvec, targ);
  if (PL_is_number(targ)) {
    int ints = TRUE;

    for (i = 0; i < arity; i++) {
      _PL_get_arg(i+1, tvec, targ);
      if (!PL_is_integer(targ)) {
  ints = FALSE;
  if (!PL_is_float(targ)) {
    UNPROTECT(1);
    return FALSE;
  }
      }
    }
    if (ints) {
      int *vec;

      PROTECT(ans = allocVector(INTSXP, arity));
      if (!ans)
  return FALSE;
      vec = INTEGER(ans);
      for (i = 0; i < arity; i++) {
  int64_t j;
  _PL_get_arg(i+1, tvec, targ);
  if (!PL_get_int64_ex(targ, &j)) {
    UNPROTECT(1);
    return FALSE;
  }
  vec[i] = j;
      }
    } else {
      double *vec;

      PROTECT(ans = allocVector(REALSXP, arity));
      if (!ans)
  return FALSE;
      vec = REAL(ans);
      for (i = 0; i < arity; i++) {
  _PL_get_arg(i+1, tvec, targ);
  if (!PL_get_float(targ, vec+i)) {
    int64_t j;
    if (!PL_get_int64_ex(targ, &j))
      return FALSE;
    vec[i] = j;
  }
      }
    }
  } else if (PL_is_atom(targ) || PL_is_string(targ)) {

    PROTECT(ans = allocVector(STRSXP, arity));
    if (!ans)
      return FALSE;
    for (i = 0; i < arity; i++) {
      char *str;

      _PL_get_arg(i+1, tvec, targ);
      if ( PL_get_chars(targ, &str, CVT_ALL|CVT_EXCEPTION|BUF_DISCARDABLE|REP_UTF8) )
  {
    SET_STRING_ELT(ans, i, mkCharCE(str, CE_UTF8) );
  } else {
  UNPROTECT(1);
  return FALSE;
      }
    }
  } else {
    return FALSE;
  }
  defineVar(install(s), ans, rho);
  UNPROTECT(1);
  return TRUE;
}


static foreign_t
rexpr_to_pl_term(term_t in, term_t out)
{ char *s;

  if ( PL_get_chars(in, &s, CVT_ALL|CVT_EXCEPTION|BUF_MALLOC|REP_UTF8) )
  { SEXP sexp;

    if ( (sexp=process_expression(s)) )
    { term_t tmp = PL_new_term_ref();

      PL_free(s);
      if ( put_sexp(tmp, sexp) )
  return PL_unify(out, tmp);

      return FALSE;
    } else
    { /* FIXME: Throw exception */
      PL_free(s);
    }
  }

  return FALSE;
}


static foreign_t
robj_to_pl_term(term_t name, term_t out)
{ char *plname;

  if ( PL_get_chars(name, &plname, CVT_ALL|CVT_EXCEPTION|BUF_DISCARDABLE|REP_UTF8) )
  { SEXP s;
    int nprotect = 0;
    term_t tmp = PL_new_term_ref();
    int rc;

    PROTECT( s= findVar(install(plname), R_GlobalEnv) );
    nprotect ++;
    if (TYPEOF(s)==SYMSXP)
      return PL_existence_error("r_variable", name);

    rc = put_sexp(tmp, s);
    UNPROTECT(nprotect);

    if ( rc )
      return PL_unify(out, tmp);
  }

  return FALSE;
}

static foreign_t
set_r_variable(term_t rvar, term_t value)
{ char *vname = NULL;
  SEXP sexp;

  if ( PL_get_chars(rvar, &vname, CVT_ALL|CVT_EXCEPTION|BUF_MALLOC|REP_UTF8) &&
       pl_sexp(value, &sexp) )
  {
    defineVar(Rf_install(vname), sexp, R_GlobalEnv);
    PL_free(vname);
    // UNPROTECT(1);        /* FIXME: Dubious */
    return TRUE;
  }
  if (vname)
    PL_free(vname);
  return FALSE;
}

static foreign_t
is_r_variable(term_t t)
{
  SEXP name,o;
  char *s;

  /* is this variable defined in R?.  */
  if ( PL_get_chars(t, &s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION|BUF_DISCARDABLE|REP_UTF8) )
    { PROTECT(name = NEW_CHARACTER(1));
      CHARACTER_DATA(name)[0] = mkCharCE(s, CE_UTF8);
    }
  else {
    UNPROTECT(1);
    return FALSE;
  }

  PROTECT(o = findVar(install(CHAR(STRING_ELT(name, 0))), R_GlobalEnv));
  UNPROTECT(2);
  return o != R_UnboundValue;
}


		 /*******************************
		 *         REDIRECT I/O		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See http://www.math.ncu.edu.tw/~chenwc/R_note/reference/package/R-exts.pdf
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* Nicos: 17.11.22, bindRIO() is currently disabled, on my linux it mangles
                              library names when loading libraries
#ifdef RINTERFACE_H_			// Rinterface.h is loaded

// FIXME: What encoding does the console message have?

static void
showMessage(const char *s)		// FIXME: call printMessage?
{ for(; *s; s++)
    Sputcode(*s&0xff, Suser_error);
}

static void
writeConsole(const char *buf, int buflen)
{ int i;

  for(i=0; i<buflen; i++)
    Sputcode(buf[i]&0xff, Scurrent_output);
}

static void
writeConsoleEx(const char *buf, int buflen, int otype)
{ IOSTREAM *out = otype ? Suser_error : Scurrent_output;
  int i;

  for(i=0; i<buflen; i++)
    Sputcode(buf[i]&0xff, out);
}

static void
flushConsole(void)
{ Sflush(Scurrent_output);		// error is not buffered
}

static void
clearerrConsole(void)
{ Sclearerr(Suser_error);
  Sclearerr(Scurrent_output);
}


static void
bindRIO(void)
{ ptr_R_ShowMessage     = showMessage;
  ptr_R_WriteConsole    = NULL;
  ptr_R_WriteConsoleEx  = writeConsoleEx;
  ptr_R_FlushConsole    = flushConsole;
  ptr_R_ClearerrConsole = clearerrConsole;

  R_Outputfile  = NULL;
  R_Consolefile = NULL;
}

#else //RINTERFACE_H_
static void bindRIO(void) {}
#endif
*/


		 /*******************************
		 *	     REGISTER		*
		 *******************************/

#ifndef ATOM_dot
#define ATOM_dot PL_new_atom(".")
#endif

install_t
install_real(void)
{ /* FUNCTOR_dot2 = PL_new_functor(PL_new_atom("."), 2); */

  FUNCTOR_dot2 = PL_new_functor(ATOM_dot, 2);
  FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);
  FUNCTOR_boolop1 = PL_new_functor(PL_new_atom("@"), 1);
  FUNCTOR_plus1 = PL_new_functor(PL_new_atom("+"), 1);
  ATOM_true  = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");
  ATOM_empty = PL_new_atom("");
  ATOM_nan = PL_new_atom("$NaN");

  PL_register_foreign("init_r",      0, init_R,         0);
  PL_register_foreign("stop_r",      0, stop_R,         0);
  PL_register_foreign("send_r_command",    1, send_r_command,   0);
  PL_register_foreign("send_c_vector",    2, send_c_vector,    0);
  PL_register_foreign("rexpr_to_pl_term", 2, rexpr_to_pl_term, 0);
  PL_register_foreign("robj_to_pl_term",  2, robj_to_pl_term,  0);
  PL_register_foreign("set_r_variable",   2, set_r_variable,   0);
  PL_register_foreign("is_r_variable",    1, is_r_variable,    0);
}
