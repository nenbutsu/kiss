/*  -*- coding: utf-8 -*-
  vector.c --- defines the general vector mechanism of ISLisp processor KISS.

  Copyright (C) 2017, 2018 Yuji Minejima <yuji@minejima.jp>

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

kiss_general_vector_t* kiss_make_general_vector(const size_t n, const kiss_obj* const obj) {
    kiss_general_vector_t* p = Kiss_GC_Malloc(sizeof(kiss_general_vector_t));
    kiss_obj** v = Kiss_Malloc(n * sizeof(kiss_obj*));
    p->type = KISS_GENERAL_VECTOR;
    p->v = v;
    p->n = n;
    for (size_t i = 0; i < n; i++) { v[i] = (kiss_obj*)obj; }
    return p;
}

/* function: (create-general-vector i [initial-element]) -> <general-vector>
   Returns a general-vector of length I. If INITIAL-ELEMENT is given,
   the elements of the new vector are initialized with this object, otherwise
   it is initialized with nil. An error shall be signaled if the requested
   vector cannot be allocated (error-id. cannot-create-vector ).
   An error shall be signaled if I is not a non-negative integer
   (error-id. domain-error ). INITIAL-ELEMENT may be any LISP object.*/
kiss_obj* kiss_create_general_vector(const kiss_obj* const i, const kiss_obj* const rest) {
    kiss_ptr_int n = Kiss_Non_Negative_Fixnum(i);
    kiss_obj* obj = rest == KISS_NIL ? KISS_NIL : KISS_CAR(rest);
    return (kiss_obj*)kiss_make_general_vector(n, obj);
}

/* function: (vector obj*) -> <general-vector> 
   Returns a new general-vector whose elements are its OBJ arguments.
   The length of the newly created vector is, therefore, the number of
   OBJs passed as arguments. The vector is indexed by integers ranging
   from 0 to dimension−1. An error shall be signaled if the requested
   vector cannot be allocated (error-id. cannot-create-vector ).
   Each OBJ may be any LISP object. */
kiss_obj* kiss_vector(const kiss_obj* objs) {
    size_t n = kiss_c_length(objs);
    kiss_general_vector_t* v = kiss_make_general_vector(n, KISS_NIL);
    for (size_t i = 0; i < n; i++) {
	v->v[i] = KISS_CAR(objs);
	objs = KISS_CDR(objs);
    }
    return (kiss_obj*)v;
}

/* function: (basic-vector-p obj) -> boolean
   Returns t if OBJ is a basic-vector (instance of class <basic-vector>);
   otherwise, returns nil. OBJ may be any ISLISP object. */
inline kiss_obj* kiss_basic_vector_p(const kiss_obj* const obj) {
     return kiss_general_vector_p(obj) || kiss_stringp(obj) ? KISS_T : KISS_NIL;
}


/* function: (general-vector-p obj) -> boolean
   Returns t if OBJ is a general vector; otherwise, returns nil.
   OBJ may be any LISP object. */
inline kiss_obj* kiss_general_vector_p(const kiss_obj* const obj) {
     return KISS_IS_GENERAL_VECTOR(obj) ? KISS_T : KISS_NIL;
}

/* Kiss function: (gvref general-vector index) -> <object>
   Returns the object stored in the component of the GENERAL-VECTOR
   specified by integer INDEX.
   An error shall be signaled if GENERAL-VECTOR is not a general-vector
   (error-id. domain-error ). An error shall be signaled if INDEX is not
   a non-negative integer (error-id. domain-error ).
 */
kiss_obj* kiss_gvref(const kiss_obj* const general_vector, const kiss_obj* const index) {
     const kiss_general_vector_t* const gv = Kiss_General_Vector(general_vector);
     kiss_ptr_int i = Kiss_Non_Negative_Fixnum(index);
     if (i >= gv->n) {
          Kiss_Err(L"Index is too large. ~S", index);
     }
     return gv->v[i];
}

/* function: (set-gvref obj general-vector index) -> <object>
   Replace the object obtainable by gvref with obj . The returned value is
   obj. The constraints on the general-vector and index are the same as for
   gvref. */
kiss_obj* kiss_set_gvref(const kiss_obj* const obj, kiss_obj* general_vector, const kiss_obj* const index)
{
    kiss_general_vector_t* const gv = Kiss_General_Vector(general_vector);
    kiss_ptr_int i = Kiss_Non_Negative_Fixnum(index);
    if (i >= gv->n) {
	Kiss_Err(L"Index is too large. ~S", index);
    }
    gv->v[i] = (kiss_obj*)obj;
    return (kiss_obj*)obj;
}

kiss_obj* kiss_str_to_vec(const kiss_obj* const obj) {
     kiss_string_t* str = Kiss_String(obj);
     kiss_general_vector_t* vec = (kiss_general_vector_t*)kiss_create_general_vector(kiss_make_fixnum(str->n), KISS_NIL);
     for (size_t n = 0; n < str->n; n++) {
          vec->v[n] = kiss_make_char(str->str[n]);
     }
     return (kiss_obj*)vec;
}
