/*  -*- coding: utf-8 -*-
  sequence.c --- defines the sequence handling mechanism of ISLisp processor KISS.

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

size_t kiss_clength(kiss_obj* p) {
    p = Kiss_Sequence(p);
    switch (KISS_OBJ_TYPE(p)) {
    case KISS_SYMBOL: return 0;
    case KISS_CONS: {
	    size_t n = 0;
	    for (; KISS_IS_CONS(p); p = KISS_CDR(p)) { n++; }
	    return n;
	}
    case KISS_STRING: return ((kiss_string_t*)p)->n;
    default:
	Kiss_Err(L"clength internal error, unknown sequence ~S", p);
    }
}

/* function: (length sequence) → <integer> 
   Returns the length of sequence as an integer greater than or equal to 0.
   When sequence is a vector, length returns its dimension.
   When sequence is a list, the result is the number of elements in the list;
   if an element is itself a list, the elements within this sublist are not
   counted. In the case of dotted lists, length returns the number of
   conses at the uppermost level of the list. For example,
   (length ’(a b . c)) ⇒ 2,
   since ’(a b . c) ≡ (cons ’a (cons ’b ’c)).
   An error shall be signaled if sequence is not a basic-vector or a list
   (error-id. domain-error ).
 */
kiss_obj* kiss_length(kiss_obj* sequence) {
    return (kiss_obj*)kiss_make_integer(kiss_clength(sequence));
}

/* function: (elt sequence z) → <object>
   Given a sequence and an integer z satisfying 0 ≤ z < (length sequence),
   elt returns the element of sequence that has index z. Indexing is
   0-based; i.e., z = 0 designates the first element. An error shall be
   signaled if z is an integer outside of the mentioned range
   (error-id. index-out-of-range).
   An error shall be signaled if sequence is not a basic-vector or a list or
   if z is not an integer (error-id. domain-error). */
kiss_obj* kiss_elt(kiss_obj* sequence, kiss_obj* z) {
    Kiss_Check_Sequence_Index_Range(sequence, z);
    kiss_integer_t* integer = Kiss_Integer(z);
    switch (KISS_OBJ_TYPE(sequence)) {
    case KISS_SYMBOL: {
	Kiss_Err(L"elt internal error: zero-length-sequence ~S", sequence);
    }
    case KISS_CONS: {
	kiss_cons_t* list = Kiss_Cons(sequence);
	for (; integer->i > 0; integer->i--) { list = KISS_CDR(list); }
	return KISS_CAR(list);
    }
    case KISS_STRING: {
	kiss_string_t* string = Kiss_String(sequence);
	return (kiss_obj*)kiss_make_character(string->str[integer->i]);
    }
    default:
	Kiss_Err(L"elt internal error, unknown sequence ~S", sequence);
    }
    
}
