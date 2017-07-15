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

static kiss_general_vector_t* kiss_make_general_array(kiss_obj* dimensions, kiss_obj* list) {
    kiss_general_vector_t* p = Kiss_GC_Malloc(sizeof(kiss_general_vector_t));
    kiss_obj** v = NULL;
    p->type = KISS_GENERAL_ARRAY;
    p->v = v;

    return p;
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
