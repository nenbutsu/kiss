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

static kiss_general_vector_t* kiss_make_ga(kiss_obj* dimensions, kiss_general_vector_t* vector,
				    kiss_obj* obj)
{
     size_t rank = kiss_clength(dimensions);
     if (rank == 0) {
	  for (size_t i = 0; i < vector->n; i++) {
	       vector->v[i] = obj;
	  }
	  return vector;
     }
     for (size_t i = 0; i < vector->n; i++) {
	  vector->v[i] = kiss_create_general_vector(kiss_car(dimensions), KISS_NIL);
	  kiss_make_ga(kiss_cdr(dimensions), ((kiss_general_vector_t*)vector->v[i]), obj);
     }
     return vector;
}

static kiss_general_array_t* kiss_make_general_array(kiss_obj* dimensions, kiss_obj* obj) {
    kiss_general_array_t* array = Kiss_GC_Malloc(sizeof(kiss_general_array_t));
    array->type = KISS_GENERAL_ARRAY;
    array->rank = kiss_clength(dimensions);
    if (array->rank == 0) {
	 array->vector = obj;
	 return array;
    }
    
    kiss_general_vector_t* vector = (kiss_general_vector_t*)kiss_create_general_vector(kiss_car(dimensions), KISS_NIL);
    array->vector = (kiss_obj*)kiss_make_ga(kiss_cdr(dimensions), vector, obj);
    return array;
}

/* function: (create-array dimensions [initial-element]) -> <basic-array>
   This function creates an array of the given dimensions.
   The dimensions argument is a list of non-negative integers.
   The result is of class <general-vector> if there is only one dimension, 
   or of class <general-array*> otherwise.
   If initial-element is given, the elements of the new array are initialized with this object,
   otherwise the initialization is implementation defined.
   An error shall be signaled if the requested array cannot be allocated
   (error-id. cannot-create-array).
   An error shall be signaled if dimensions is not a proper list of non-negative integers
   (error-id. domain-error).
   initial-element may be any ISLISP object.
*/
kiss_obj* kiss_create_array(kiss_obj* dimensions, kiss_obj* rest) {
     dimensions = Kiss_Proper_List(dimensions);
     kiss_cmapc((kiss_cf1_t)Kiss_Non_Negative_Integer, dimensions);
     
     if (kiss_clength(dimensions) == 1) {
	  return kiss_create_general_vector(kiss_car(dimensions), rest);
     } else {
	  kiss_obj* obj = (rest == KISS_NIL) ? KISS_NIL : kiss_car(rest);
	  kiss_make_general_array(dimensions, obj);
     }
}

static kiss_obj* kiss_ga_s_ref(kiss_obj* vector, kiss_obj* rest) {
     if (!KISS_IS_CONS(rest)) { return vector; }
     return kiss_ga_s_ref(kiss_gvref(vector, kiss_car(rest)), kiss_cdr(rest));
}

static kiss_obj* kiss_set_ga_s_ref(kiss_obj* obj, kiss_obj* vector, kiss_obj* rest) {
     size_t rank = kiss_clength(rest);
     if (rank == 1) {
	  return kiss_set_gvref(obj, vector, kiss_car(rest));
     }
     return kiss_set_ga_s_ref(obj, kiss_gvref(vector, kiss_car(rest)), kiss_cdr(rest));
}

/* function (garef general-array z*) -> <object>
   garef is like aref but an error shall be signaled if its first argument,
   general-array, is not an object of class <general-vector> or of class <general-array*>
   (error-id. domain-error).
 */
kiss_obj* kiss_garef(kiss_obj* array, kiss_obj* rest) {
     array = Kiss_General_Array(array);
     switch (KISS_OBJ_TYPE(array)) {
     case KISS_GENERAL_VECTOR:
	  if (kiss_clength(rest) != 1) {
	       Kiss_Err(L"Invalid vector dimension ~S", kiss_length(rest));
	  }
	  return kiss_gvref(array, kiss_car(rest));
     case KISS_GENERAL_ARRAY:
	  return kiss_ga_s_ref(((kiss_general_array_t*)array)->vector, rest);
     default:
	  Kiss_Err(L"unexpeced primitive obj type ~S", KISS_OBJ_TYPE(array));
     }
}

/* function: (set-garef obj general-array z*) -> <object>
   Replace the object obtainable by garef with obj.
   The returned value is obj. The constraints on the general-array,
   and the sequence of indices z is the same as for garef.
*/
kiss_obj* kiss_set_garef(kiss_obj* obj, kiss_obj* array, kiss_obj* rest) {
     array = Kiss_General_Array(array);
     switch (KISS_OBJ_TYPE(array)) {
     case KISS_GENERAL_VECTOR:
	  if (kiss_clength(rest) != 1) {
	       Kiss_Err(L"Invalid vector dimension ~S", kiss_length(rest));
	  }
	  return kiss_set_gvref(obj, array, kiss_car(rest));
     case KISS_GENERAL_ARRAY: {
	  kiss_general_array_t* a = (kiss_general_array_t*)array;
	  if (a->rank == 0) {
	       a->vector = obj;
	       return obj;
	  }
	  return kiss_set_ga_s_ref(obj, a->vector, rest);
     }
     default:
	  Kiss_Err(L"unexpeced primitive obj type ~S", KISS_OBJ_TYPE(array));
     }
}

kiss_obj* kiss_ga_s_to_list(size_t rank, kiss_obj* obj) {
     kiss_general_vector_t* vector = Kiss_General_Vector(obj);
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

kiss_obj* kiss_general_array_s_to_list (kiss_obj* obj) {
     kiss_general_array_t* array = Kiss_General_Array_S(obj);
     if (array->rank == 0) {
	  return array->vector;
     }

     return kiss_ga_s_to_list(array->rank, array->vector); 
}

/* function: (basic-array-p obj) -> boolean
   basic-array-p returns t if obj is a basic-array (instance of class <basic-array>);
   otherwise, returns nil. obj may be any ISLISP object.
 */
kiss_obj* kiss_basic_array_p (kiss_obj* obj) {
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_CONS:
     case KISS_SYMBOL:
     case KISS_CHARACTER:
     case KISS_INTEGER:
     case KISS_FLOAT:
     case KISS_STREAM:
     case KISS_FUNCTION:
     case KISS_MACRO:
     case KISS_CFUNCTION:
     case KISS_CMACRO:
     case KISS_CATCHER:
     case KISS_CLEANUP:
     case KISS_BLOCK:
     case KISS_TAGBODY:
     case KISS_OO_OBJ:
	  return KISS_NIL;
     case KISS_STRING:
     case KISS_GENERAL_VECTOR:
     case KISS_GENERAL_ARRAY:
	  return KISS_T;
     default:
	  fwprintf(stderr, L"basic-array-p: unknown primitive obj type %d", KISS_OBJ_TYPE(obj));
     }
}

/* function: (basic-array*-p obj) -> boolean
   basic-array*-p returns t if obj is a basic-array* (instance of class <basic-array*>);
   otherwise, returns nil. obj may be any ISLISP object.
*/

kiss_obj* kiss_basic_array_s_p (kiss_obj* obj) {
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_CONS:
     case KISS_SYMBOL:
     case KISS_CHARACTER:
     case KISS_INTEGER:
     case KISS_FLOAT:
     case KISS_STREAM:
     case KISS_FUNCTION:
     case KISS_MACRO:
     case KISS_CFUNCTION:
     case KISS_CMACRO:
     case KISS_CATCHER:
     case KISS_CLEANUP:
     case KISS_BLOCK:
     case KISS_TAGBODY:
     case KISS_OO_OBJ:
     case KISS_STRING:
     case KISS_GENERAL_VECTOR:
	  return KISS_NIL;
     case KISS_GENERAL_ARRAY:
	  return KISS_T;
     default:
	  fwprintf(stderr, L"basic-array*-p: unknown primitive obj type %d", KISS_OBJ_TYPE(obj));
     }
}

/* function: (general-array*-p obj) -> boolean
   general-array*-p returns t if obj is a general-array* (instance of class <general-array*>);
   otherwise, returns nil. obj may be any ISLISP object.
*/
kiss_obj* kiss_general_array_s_p (kiss_obj* obj) {
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_CONS:
     case KISS_SYMBOL:
     case KISS_CHARACTER:
     case KISS_INTEGER:
     case KISS_FLOAT:
     case KISS_STREAM:
     case KISS_FUNCTION:
     case KISS_MACRO:
     case KISS_CFUNCTION:
     case KISS_CMACRO:
     case KISS_CATCHER:
     case KISS_CLEANUP:
     case KISS_BLOCK:
     case KISS_TAGBODY:
     case KISS_OO_OBJ:
     case KISS_STRING:
     case KISS_GENERAL_VECTOR:
	  return KISS_NIL;
     case KISS_GENERAL_ARRAY:
	  return KISS_T;
     default:
	  fwprintf(stderr, L"general-array*-p: unknown primitive obj type %d", KISS_OBJ_TYPE(obj));
     }
}

