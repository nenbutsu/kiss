/*  -*- coding: utf-8 -*-
  general_vector.c --- defines the general vector mechanism of ISLisp processor KISS.

  Copyright (C) 2017 Yuji Minejima.

  This file is part of ISLisp processor KISS.

  KISS is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  KISS is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

*/
#include "kiss.h"

/*
  <object>
     |
     +--> <basic-array>
              |
              +--> <basic-array*>
              |        |
              |        +--> <general-array*>
              |
              +--> <basic-vector>
                       |
                       +--> <general-vector>
                       +--> <string>
 */

static kiss_general_vector_t* kiss_make_general_vector(size_t n, kiss_obj* obj) {
    kiss_general_vector_t* p = Kiss_GC_Malloc(sizeof(kiss_general_vector_t));
    kiss_obj** v = Kiss_Malloc(n * sizeof(kiss_obj*));
    size_t i;
    p->type = KISS_GENERAL_VECTOR;
    p->v = v;
    p->n = n;
    for (i = 0; i < n; i++) { v[i] = obj; }
    return p;
}

/* function: (create-general-vector i [initial-element]) → <general-vector>
   Returns a general-vector of length i. If initial-element is given,
   the elements of the new vector are initialized with this object, otherwise
   it is initialized with nil. An error shall be signaled if the requested
   vector cannot be allocated (error-id. cannot-create-vector ).
   An error shall be signaled if i is not a non-negative integer
   (error-id. domain-error ). initial-element may be any LISP object.*/
kiss_obj* kiss_create_general_vector(kiss_obj* i, kiss_obj* rest) {
    kiss_integer_t* n = Kiss_Non_Negative_Integer(i);
    kiss_obj* obj;
    if (rest == KISS_NIL) { obj = KISS_NIL; }
    else                  { obj = KISS_CAR(rest); }
    return (kiss_obj*)kiss_make_general_vector(n->i, obj);
}

/* function: (vector obj*) → <general-vector> 
   Returns a new general-vector whose elements are its obj arguments.
   The length of the newly created vector is, therefore, the number of
   objs passed as arguments. The vector is indexed by integers ranging
   from 0 to dimension−1. An error shall be signaled if the requested
   vector cannot be allocated (error-id. cannot-create-vector ).
   Each obj may be any LISP object. */
kiss_obj* kiss_vector(kiss_obj* objs) {
    size_t n = kiss_clength(objs);
    kiss_general_vector_t* v = kiss_make_general_vector(n, KISS_NIL);
    size_t i;
    for (i = 0; i < n; i++) {
	v->v[i] = KISS_CAR(objs);
	objs = KISS_CDR(objs);
    }
    return (kiss_obj*)v;
}

/* function: (general-vector-p obj) → boolean
   general-vector-p returns t if obj is a general vector; otherwise,
   returns nil. obj may be any LISP object. */
kiss_obj* kiss_general_vector_p(kiss_obj* obj) {
    if (KISS_IS_GENERAL_VECTOR(obj)) { return KISS_T; }
    else { return KISS_NIL; }
}

/* function: (gvref general-vector index) -> <object>
   gvref returns the object stored in the component of the general-vector
   specified by integer index.
   An error shall be signaled if general-vector is not a general-vector
   (error-id. domain-error ). An error shall be signaled if index is not
   a non-negative integer (error-id. domain-error ).
 */
kiss_obj* kiss_gvref(kiss_obj* general_vector, kiss_obj* index) {
    kiss_general_vector_t* gv = Kiss_General_Vector(general_vector);
    kiss_integer_t* i = Kiss_Non_Negative_Integer(index);
    if (i->i >= gv->n) {
	Kiss_Err(L"index is too large. ~S", index);
    }
    return gv->v[i->i];
}

/* function: (set-gvref obj general-vector index) → <object>
   Replace the object obtainable by gvref with obj . The returned value is
   obj. The constraints on the general-vector and index are the same as for
   gvref. */
kiss_obj* kiss_set_gvref(kiss_obj* obj, kiss_obj* general_vector,
			 kiss_obj* index)
{
    kiss_general_vector_t* gv = Kiss_General_Vector(general_vector);
    kiss_integer_t* i = Kiss_Non_Negative_Integer(index);
    if (i->i >= gv->n) {
	Kiss_Err(L"index is too large. ~S", index);
    }
    gv->v[i->i] = obj;
    return obj;
}
