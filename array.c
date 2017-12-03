/*  -*- coding: utf-8 -*-
  array.c --- defines the general array mechanism of ISLisp processor KISS.

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

static kiss_general_vector_t* kiss_make_ga(const kiss_obj* const dimensions, kiss_general_vector_t* const vector, const kiss_obj* const obj)
{
     size_t rank = kiss_c_length(dimensions);
     if (rank == 0) {
	  for (size_t i = 0; i < vector->n; i++) {
	       vector->v[i] = (kiss_obj*)obj;
	  }
	  return vector;
     }
     for (size_t i = 0; i < vector->n; i++) {
	  vector->v[i] = kiss_create_general_vector(kiss_car(dimensions), KISS_NIL);
	  kiss_make_ga(kiss_cdr(dimensions), ((kiss_general_vector_t*)vector->v[i]), obj);
     }
     return vector;
}

static kiss_general_array_t* kiss_make_general_array(const kiss_obj* const dimensions, const kiss_obj* const obj)
{
    kiss_general_array_t* array = Kiss_GC_Malloc(sizeof(kiss_general_array_t));
    array->type = KISS_GENERAL_ARRAY_S;
    array->rank = kiss_c_length(dimensions);
    if (array->rank == 0) {
	 array->vector = (kiss_obj*)obj;
	 return array;
    }
    
    kiss_general_vector_t* vector = (kiss_general_vector_t*)kiss_create_general_vector(kiss_car(dimensions), KISS_NIL);
    array->vector = (kiss_obj*)kiss_make_ga(kiss_cdr(dimensions), vector, obj);
    return array;
}

/* function: (create-array dimensions [initial-element]) -> <basic-array>
   This function creates an array of the given DIMENSIONS.
   The DIMENSIONS argument is a list of non-negative integers.
   The result is of class <general-vector> if there is only one dimension, 
   or of class <general-array*> otherwise.
   If INITIAL-ELEMENT is given, the elements of the new array are initialized with this object,
   otherwise the initialization is implementation defined.
   An error shall be signaled if the requested array cannot be allocated 
   (error-id. cannot-create-array).
   An error shall be signaled if DIMENSIONS is not a proper list of non-negative integers
   (error-id. domain-error).
   INITIAL-ELEMENT may be any ISLISP object. */
kiss_obj* kiss_create_array(const kiss_obj* const dimensions, const kiss_obj* const rest) {
     Kiss_Proper_List(dimensions);
     kiss_c_mapc1((kiss_cf1_t)Kiss_Non_Negative_Fixnum, dimensions);
     size_t rank = kiss_c_length(dimensions);

     if (rank == 1) {
	  return kiss_create_general_vector(kiss_car(dimensions), rest);
     } else {
	  kiss_obj* obj = (rest == KISS_NIL) ? KISS_NIL : kiss_car(rest);
	  return (kiss_obj*)kiss_make_general_array(dimensions, obj);
     }
}

static kiss_obj* kiss_ga_s_ref(const kiss_obj* const vector, const kiss_obj* const rest) {
     if (!KISS_IS_CONS(rest)) {
          return (kiss_obj*)vector;
     } else {
          return kiss_ga_s_ref(kiss_gvref(vector, kiss_car(rest)), kiss_cdr(rest));
     }
}

static kiss_obj* kiss_set_ga_s_ref(const kiss_obj* const obj, kiss_obj* const vector, const kiss_obj* const rest)
{
     size_t rank = kiss_c_length(rest);
     if (rank == 1) {
	  return kiss_set_gvref(obj, vector, kiss_car(rest));
     }
     return kiss_set_ga_s_ref(obj, kiss_gvref(vector, kiss_car(rest)), kiss_cdr(rest));
}

/* function (garef general-array z*) -> <object>
   Is like aref but an error shall be signaled if its first argument,
   GENERAL-ARRAY, is not an object of class <general-vector> or of class <general-array*>
   (error-id. domain-error). */
kiss_obj* kiss_garef(const kiss_obj* const array, const kiss_obj* const rest) {
     Kiss_General_Array(array);
     switch (KISS_OBJ_TYPE(array)) {
     case KISS_GENERAL_VECTOR:
	  if (kiss_c_length(rest) != 1) {
	       Kiss_Err(L"Invalid vector index dimension ~S", kiss_length(rest));
	  }
	  return kiss_gvref(array, kiss_car(rest));
     case KISS_GENERAL_ARRAY_S: {
	  return kiss_ga_s_ref(((kiss_general_array_t*)array)->vector, rest);
     }
     default:
	  fwprintf(stderr, L"garef: unexpeced primitive obj type %d", KISS_OBJ_TYPE(array));
	  exit(EXIT_FAILURE);
     }
}

/* function: (aref basic-array z*) -> <object>
   Returns the object stored in the component of the BASIC-ARRAY specified
   by the sequence of integers Z.
   This sequence must have exactly as many elements as there are dimensions in the
   BASIC-ARRAY, and each one must satisfy 0 <= Zi < di, di the i th dimension and
   0 <= i < d, d the number of dimensions.
   Arrays are indexed 0 based, so the ith row is accessed via the index i − 1.
   An error shall be signaled if BASIC-ARRAY is not a basic-array  (error-id. domain-error).
   An error shall be signaled if any Z is not a non-negative integer (error-id. domain-error). */
kiss_obj* kiss_aref(const kiss_obj* const array, const kiss_obj* const rest) {
     Kiss_Basic_Array(array);
     switch (KISS_OBJ_TYPE(array)) {
     case KISS_STRING: {
	  if (kiss_c_length(rest) != 1) {
	       Kiss_Err(L"Invalid string dimension ~S", kiss_length(rest));
	  }
	  return kiss_elt(array, kiss_car(rest));
     }
     case KISS_GENERAL_VECTOR:
     case KISS_GENERAL_ARRAY_S:
	  return kiss_garef(array, rest);
     default:
	  fwprintf(stderr, L"aref: unexpected primitive obj type %d", KISS_OBJ_TYPE(array));
	  exit(EXIT_FAILURE);
     }
}

/* (set-aref obj basic-array z*) → <object>
   Replace the object obtainable by aref with OBJ.
   The returned value is OBJ. The constraints on the BASIC-ARRAY and 
   the sequence of indices Z is the same as for aref. */
kiss_obj* kiss_set_aref(const kiss_obj* const obj, kiss_obj* const array, const kiss_obj* const rest)
{
     Kiss_Basic_Array(array);
     switch (KISS_OBJ_TYPE(array)) {
     case KISS_STRING: {
	  if (kiss_c_length(rest) != 1) {
	       Kiss_Err(L"Invalid string dimension ~S", kiss_length(rest));
	  }
	  return kiss_set_elt(obj, array, kiss_car(rest));
     }
     case KISS_GENERAL_VECTOR:
     case KISS_GENERAL_ARRAY_S:
	  return kiss_set_garef(obj, array, rest);
     default:
	  fwprintf(stderr, L"aref: unexpected primitive obj type %d", KISS_OBJ_TYPE(array));
	  exit(EXIT_FAILURE);
     }
}

/* function: (set-garef obj general-array z*) -> <object>
   Replace the object obtainable by garef with obj.
   The returned value is obj. The constraints on the general-array,
   and the sequence of indices z is the same as for garef. */
kiss_obj* kiss_set_garef(const kiss_obj* const obj, kiss_obj* const array, const kiss_obj* const rest)
{
     Kiss_General_Array(array);
     switch (KISS_OBJ_TYPE(array)) {
     case KISS_GENERAL_VECTOR:
	  if (kiss_c_length(rest) != 1) {
	       Kiss_Err(L"Invalid vector dimension ~S", kiss_length(rest));
	  }
	  return kiss_set_gvref(obj, array, kiss_car(rest));
     case KISS_GENERAL_ARRAY_S: {
	  kiss_general_array_t* a = (kiss_general_array_t*)array;
	  if (a->rank == 0) {
	       a->vector = (kiss_obj*)obj;
	       return (kiss_obj*)obj;
	  }
	  return kiss_set_ga_s_ref(obj, a->vector, rest);
     }
     default:
	  fwprintf(stderr, L"set-garef: unexpeced primitive obj type %d", KISS_OBJ_TYPE(array));
	  exit(EXIT_FAILURE);
     }
}

static kiss_obj* kiss_ga_s_to_list(const size_t rank, const kiss_obj* const gv) {
     const kiss_general_vector_t* const vector = Kiss_General_Vector(gv);
     kiss_obj* p = KISS_NIL;
     if (rank == 1) {
	  for (size_t i = 0; i < vector->n; i++) {
	       kiss_push(vector->v[i], &p);
	  }
     } else {
	  for (size_t i = 0; i < vector->n; i++) {
	       kiss_push(kiss_ga_s_to_list(rank - 1, vector->v[i]), &p);
	  }
     }
     return kiss_nreverse(p);
}

kiss_obj* kiss_general_array_s_to_list (const kiss_obj* const garray) {
     const kiss_general_array_t* const p = Kiss_General_Array_S(garray);
     if (p->rank == 0) {
	  return p->vector;
     } else {
          return kiss_ga_s_to_list(p->rank, p->vector);
     }
}

static kiss_obj* kiss_ga_dimensions(const kiss_general_array_t* const array) {
     if (array->rank == 0) { return KISS_NIL; }

     kiss_obj* dimensions = KISS_NIL;
     kiss_general_vector_t* p = (kiss_general_vector_t*)array->vector;
     for (size_t rank = array->rank; rank > 0; rank--) {
	  kiss_push((kiss_obj*)kiss_make_fixnum(p->n), &dimensions);
	  p = (kiss_general_vector_t*)(p->v[0]);
     }
     return kiss_nreverse(dimensions);
}

/* function: (array-dimensions basic-array) -> <list>
   Returns a list of the dimensions of a given BASIC-ARRAY.
   An error shall be signaled if BASIC-ARRAY is not a basic-array  (error-id. domain-error).
   The consequences are undefined if the returned list is modified. */
kiss_obj* kiss_array_dimensions(const kiss_obj* const array) {
     Kiss_Basic_Array(array);
     switch (KISS_OBJ_TYPE(array)) {
     case KISS_STRING:
	  return kiss_cons((kiss_obj*)kiss_make_fixnum(Kiss_String(array)->n), KISS_NIL);
     case KISS_GENERAL_VECTOR:
	  return kiss_cons((kiss_obj*)kiss_make_fixnum(Kiss_General_Vector(array)->n), KISS_NIL);
     case KISS_GENERAL_ARRAY_S:
	  return kiss_ga_dimensions((kiss_general_array_t*)array);
     default:
	  fwprintf(stderr, L"array-dimensions: unexpected primitive obj type %d",
                  KISS_OBJ_TYPE(array));
	  exit(EXIT_FAILURE);
     }
     
}

/* function: (basic-array-p obj) -> boolean
   Rturns t if OBJ is a basic-array (instance of class <basic-array>);
   otherwise, returns nil. OBJ may be any ISLISP object. */
kiss_obj* kiss_basic_array_p (const kiss_obj* const obj) {
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_CONS:
     case KISS_SYMBOL:
     case KISS_CHARACTER:
     case KISS_FIXNUM:
     case KISS_BIGNUM:
     case KISS_FLOAT:
     case KISS_STREAM:
     case KISS_HASH_TABLE:
     case KISS_LFUNCTION:
     case KISS_LMACRO:
     case KISS_CFUNCTION:
     case KISS_CMACRO:
     case KISS_CATCHER:
     case KISS_CLEANUP:
     case KISS_BLOCK:
     case KISS_TAGBODY:
     case KISS_ILOS_OBJ:
	  return KISS_NIL;
     case KISS_STRING:
     case KISS_GENERAL_VECTOR:
     case KISS_GENERAL_ARRAY_S:
	  return KISS_T;
     default:
	  fwprintf(stderr, L"basic-array-p: unknown primitive obj type %d", KISS_OBJ_TYPE(obj));
	  exit(EXIT_FAILURE);
     }
}

/* function: (basic-array*-p obj) -> boolean
   basic-array*-p returns t if OBJ is a basic-array* (instance of class <basic-array*>);
   otherwise, returns nil. OBJ may be any ISLISP object. */
kiss_obj* kiss_basic_array_s_p (const kiss_obj* const obj) {
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_CONS:
     case KISS_SYMBOL:
     case KISS_CHARACTER:
     case KISS_FIXNUM:
     case KISS_BIGNUM:
     case KISS_FLOAT:
     case KISS_STREAM:
     case KISS_HASH_TABLE:
     case KISS_LFUNCTION:
     case KISS_LMACRO:
     case KISS_CFUNCTION:
     case KISS_CMACRO:
     case KISS_CATCHER:
     case KISS_CLEANUP:
     case KISS_BLOCK:
     case KISS_TAGBODY:
     case KISS_ILOS_OBJ:
     case KISS_STRING:
     case KISS_GENERAL_VECTOR:
	  return KISS_NIL;
     case KISS_GENERAL_ARRAY_S:
	  return KISS_T;
     default:
	  fwprintf(stderr, L"basic-array*-p: unknown primitive obj type %d", KISS_OBJ_TYPE(obj));
	  exit(EXIT_FAILURE);
     }
}

/* function: (general-array*-p obj) -> boolean
   general-array*-p returns t if OBJ is a general-array* (instance of class <general-array*>);
   otherwise, returns nil. OBJ may be any ISLISP object. */
kiss_obj* kiss_general_array_s_p (const kiss_obj* const obj) {
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_CONS:
     case KISS_SYMBOL:
     case KISS_CHARACTER:
     case KISS_FIXNUM:
     case KISS_BIGNUM:
     case KISS_FLOAT:
     case KISS_STREAM:
     case KISS_HASH_TABLE:
     case KISS_LFUNCTION:
     case KISS_LMACRO:
     case KISS_CFUNCTION:
     case KISS_CMACRO:
     case KISS_CATCHER:
     case KISS_CLEANUP:
     case KISS_BLOCK:
     case KISS_TAGBODY:
     case KISS_ILOS_OBJ:
     case KISS_STRING:
     case KISS_GENERAL_VECTOR:
	  return KISS_NIL;
     case KISS_GENERAL_ARRAY_S:
	  return KISS_T;
     default:
	  fwprintf(stderr, L"general-array*-p: unknown primitive obj type %d", KISS_OBJ_TYPE(obj));
	  exit(EXIT_FAILURE);
     }
}

