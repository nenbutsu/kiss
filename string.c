/*  -*- coding: utf-8 -*-
  string.c --- defines the string handling mechanism of ISLisp processor KISS.

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


kiss_string_t* kiss_make_string(wchar_t* s) {
    kiss_string_t* p = Kiss_GC_Malloc(sizeof(kiss_string_t));
    p->type = KISS_STRING;
    p->str = wcscpy(Kiss_Malloc(sizeof(wchar_t) * (wcslen(s) + 1)), s);
    p->n = wcslen(s);
    return p;
}


/* function: (create-string i [initial-character]) → <string>
   Returns a string of length i. If initial-character is given,
   then the characters of the new string are initialized with
   this character, otherwise it is initialized with \0.
   An error shall be signaled if the requested string cannot be allocated
   (error-id. cannot-create-string). An error shall be signaled if i is not
   a non-negative integer or if initial-character is not a character
   (error-id. domain-error ).
 */
kiss_obj* kiss_create_string(kiss_obj* i, kiss_obj* rest) {
    kiss_integer_t* n = Kiss_Non_Negative_Integer(i);
    wchar_t* s = Kiss_Malloc(sizeof(wchar_t) * (n->i + 1));
    wchar_t c;
    size_t j;
    if (rest == KISS_NIL) { c = L'\0'; }
    else                  { c = Kiss_Character(KISS_CAR(rest))->c; }
    for (j = 0; j < n->i; j++) { s[j] = c; }
    s[n->i] = 0;
    return (kiss_obj*)kiss_make_string(s);
}

/* function: (stringp obj) → boolean 
   Returns t if obj is a string (instance of class string);
   otherwise, returns nil. obj may be any LISP object. */
kiss_obj* kiss_stringp(kiss_obj* obj) {
    if (KISS_IS_STRING(obj)) { return KISS_T; }
    else                     { return KISS_NIL; }
}



kiss_string_t* kiss_chars_to_str(kiss_obj* chars) {
    size_t n = kiss_clength(chars);
    wchar_t* str = Kiss_Malloc(sizeof(wchar_t) * n + 1);
    wchar_t* s = str;
    kiss_obj* p = chars;
    while (KISS_IS_CONS(p)) {
	kiss_obj* c = KISS_CAR(p);
	*s = ((kiss_character_t*)c)->c;
	s++;
	p = KISS_CDR(p);
    }
    *s = L'\0';
    return kiss_make_string(str);
}

kiss_obj* kiss_str_to_chars(kiss_string_t* str) {
    size_t n;
    kiss_obj* chars = KISS_NIL;
    for (n = str->n; n > 0; n--) {
	kiss_push((kiss_obj*)kiss_make_character(str->str[n-1]), &chars);
    }
    return chars;
}

/* function: (string-append string*) → <string>
   Returns a single string containing a sequence of characters that results from
   appending the sequences of characters of each of the strings, 
   or "" if given no strings.
   An error shall be signaled if any string is not a string (error-id. domain-error).
   This function does not modify its arguments. 
   It is implementation defined whether and when the result shares structure withn its
   string arguments.
   An error shall be signaled if the string cannot be allocated 
   (error-id. cannot-create-string).
 */
kiss_obj* kiss_string_append(kiss_obj* rest) {
     long n = 0;
     kiss_obj* args = rest;
     kiss_string_t* str;
     wchar_t* p;
     while (KISS_IS_CONS(args)) {
	  n += ((kiss_integer_t*)kiss_length(KISS_CAR(args)))->i;
	  args = KISS_CDR(args);
     }
     str = (kiss_string_t*)kiss_create_string((kiss_obj*)kiss_make_integer(n), KISS_NIL);

     args = rest;
     p = str->str;
     while (KISS_IS_CONS(args)) {
	  wcscpy(p, Kiss_String(KISS_CAR(args))->str);
	  p += ((kiss_integer_t*)kiss_length(KISS_CAR(args)))->i;
	  args = KISS_CDR(args);
     }
     return (kiss_obj*)str;
}
