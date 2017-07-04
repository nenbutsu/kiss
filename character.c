/*  -*- coding: utf-8 -*- 
  character.c --- defines character handling mechanism of ISLisp processor KISS.

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

kiss_character_t* kiss_make_character(wchar_t c) {
    kiss_character_t* p = Kiss_GC_Malloc(sizeof(kiss_character_t));
    p->type = KISS_CHARACTER;
    p->c = c;
    return p;
}

/* function: (characterp obj) → boolean
     Returns t if obj is a character (instance of class <character>);
     otherwise, returns nil. obj may be any ISLISP object.
 */
kiss_obj* kiss_characterp (kiss_obj* obj) {
    if (KISS_IS_CHARACTER(obj)) {
	return KISS_T;
    } else {
	return KISS_NIL;
    }
}

/* An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error ).*/
kiss_obj* kiss_char_eq(kiss_obj* character1, kiss_obj* character2) {
    kiss_character_t* char1 = Kiss_Character(character1);
    kiss_character_t* char2 = Kiss_Character(character2);
    if (char1->c == char2->c) {
	return KISS_T;
    } else {
	return KISS_NIL;
    }
}

kiss_obj* kiss_char_lessthan(kiss_obj* character1, kiss_obj* character2) {
    kiss_character_t* char1 = Kiss_Character(character1);
    kiss_character_t* char2 = Kiss_Character(character2);
    if (char1->c < char2->c) {
	return KISS_T;
    } else {
	return KISS_NIL;
    }
}
