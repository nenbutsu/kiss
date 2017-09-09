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

/*  function: (characterp obj) -> boolean
     Returns t if OBJ is a character (instance of class <character>);
     otherwise, returns nil. OBJ may be any ISLISP object. */
kiss_obj* kiss_characterp (kiss_obj* obj) {
    if (KISS_IS_CHARACTER(obj)) {
	return KISS_T;
    } else {
	return KISS_NIL;
    }
}

/* An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error ).*/
kiss_obj* kiss_char_eq(const kiss_obj* const character1, const kiss_obj* const character2) {
    wchar_t char1 = Kiss_Character(character1);
    wchar_t char2 = Kiss_Character(character2);
    if (char1 == char2) {
	return KISS_T;
    } else {
	return KISS_NIL;
    }
}

kiss_obj* kiss_char_lessthan(kiss_obj* character1, kiss_obj* character2) {
    wchar_t char1 = Kiss_Character(character1);
    wchar_t char2 = Kiss_Character(character2);
    if (char1 < char2) {
	return KISS_T;
    } else {
	return KISS_NIL;
    }
}
