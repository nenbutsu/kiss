/*  -*- coding: utf-8 -*-
  format_object.c --- defines the formatting mechanism of ISLisp processor KISS.

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

static kiss_obj* kiss_format_string(kiss_obj* out, kiss_obj* str, kiss_obj* escapep) {
     kiss_string_t* s = Kiss_String(str);
     wchar_t* p;
     for (p = s->str; *p != L'\0'; p++) {
	  if (escapep != KISS_NIL) {
	       switch (*p) {
	       case L'"': case L'\\':
		    kiss_format_char(out, kiss_make_char(L'\\'));
		    break;
	       default:
		    break;
	       }
	  }
	  kiss_format_char(out, kiss_make_char(*p));
     }
     return KISS_NIL;
}

static kiss_obj* kiss_format_list(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep) {
     kiss_obj* p = Kiss_List(obj);
     if (obj == KISS_NIL) {
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L"()"), KISS_NIL);
	  return KISS_NIL;
     }

     kiss_obj* op = KISS_CAR(obj);
     if (op == (kiss_obj*)&KISS_Squote && kiss_c_length(obj) == 2) {
          kiss_format_char(out, kiss_make_char(L'\''));
          kiss_format_object(out, kiss_car(KISS_CDR(p)), escapep);
          return KISS_NIL;
     }
     
     kiss_format_char(out, kiss_make_char(L'('));
     kiss_format_object(out, KISS_CAR(p), escapep);
     for (p = KISS_CDR(p); KISS_IS_CONS(p); p = KISS_CDR(p)) {
	  kiss_format_char(out, kiss_make_char(L' '));
	  kiss_format_object(out, KISS_CAR(p), escapep);
     } 
     if (p != KISS_NIL) {
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L" . "), KISS_NIL);
	  kiss_format_object(out, p, escapep);
     }
     kiss_format_char(out, kiss_make_char(L')'));
     return KISS_NIL;
}

static kiss_obj* kiss_format_general_vector(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep) {
     kiss_general_vector_t* v = Kiss_General_Vector(obj);
     size_t i;
     if (v->n == 0) {
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#()"), KISS_NIL);
	  return KISS_NIL;
     }
     kiss_format_char(out, kiss_make_char(L'#'));
     kiss_format_char(out, kiss_make_char(L'('));
     kiss_format_object(out, v->v[0], escapep);
     for (i = 1; i < v->n; i++) {
	  kiss_format_char(out, kiss_make_char(L' '));
	  kiss_format_object(out, v->v[i], escapep);
     } 
     kiss_format_char(out, kiss_make_char(L')'));
     return KISS_NIL;
    
}

static kiss_obj* kiss_format_general_array(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep) {
     kiss_general_array_t* array = Kiss_General_Array_S(obj);
     kiss_format_char(out, kiss_make_char(L'#'));
     kiss_format_integer(out, (kiss_obj*)kiss_make_fixnum(array->rank), (kiss_obj*)kiss_make_fixnum(10));
     kiss_format_char(out, kiss_make_char(L'a'));
     if (array->rank == 0) {
          kiss_format_char(out, kiss_make_char(L'('));
	  kiss_format_object(out, array->vector, escapep);
          kiss_format_char(out, kiss_make_char(L')'));
     } else {
	  kiss_format_list(out, kiss_general_array_s_to_list(obj), escapep);
     }
     return KISS_NIL;
}

static int kiss_is_simple_name(wchar_t* name) {
     if (wcscmp(name, L"+")  == 0 || wcscmp(name, L"-")  == 0 ||
	 wcscmp(name, L"1+") == 0 || wcscmp(name, L"1-") == 0)
     {
	  return 1;
     }
     if (!iswalpha(name[0]) && !wcschr(L":<>/*=?_!$%[]^{}~", name[0])) {
	  return 0;
     } else {
	  wchar_t* p = name + 1;
	  while (*p != L'\0') {
	       wchar_t c = *p++;
	       if (!iswalnum(c) && !wcschr(L"+-<>/*=?_!$%[]^{}~", c)) {
		    return 0;
	       }
	  }
	  return 1;
     }
}

static kiss_obj* kiss_format_escaped_symbol(kiss_obj* out, kiss_obj* obj) {
     kiss_symbol_t* symbol = Kiss_Symbol(obj);
     wchar_t* p = symbol->name;
     kiss_format_char(out, kiss_make_char(L'|'));
     for (p = symbol->name; *p != L'\0'; p++) {
	  wchar_t c = *p;
	  switch (c) {
	  case L'|':
	       kiss_format_string(out, (kiss_obj*)kiss_make_string(L"\\|"), KISS_NIL);
	       break;
	  case L'\\':
	       kiss_format_string(out, (kiss_obj*)kiss_make_string(L"\\\\"), KISS_NIL);
	       break;
	  default:
	       kiss_format_char(out, kiss_make_char(c));
	       break;
	  }
     }
     kiss_format_char(out, kiss_make_char(L'|'));
     return KISS_NIL;
}

static kiss_obj* kiss_format_symbol(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep) {
     kiss_symbol_t* symbol = Kiss_Symbol(obj);
     //if (!kiss_is_interned(symbol)) {
     //	kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#:"), KISS_NIL);
     //}
     if (escapep == KISS_NIL || kiss_is_simple_name(symbol->name)) {
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(symbol->name), KISS_NIL);
     } else {
	  kiss_format_escaped_symbol(out, obj);
     }
     return KISS_NIL;
}

static kiss_obj* kiss_format_escaped_char(kiss_obj* out, kiss_obj* obj) {
     wchar_t c = Kiss_Character(obj);
     switch (c) {
     case L'\n':
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#\\newline"), KISS_NIL);
	  break;
     case L' ':
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#\\space"), KISS_NIL);
	  break;
     default:
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#\\"), KISS_NIL);
	  kiss_format_char(out, obj);
     }
     return KISS_NIL;
}

/* function: (format-integer output-stream integer radix) -> <null> */
kiss_obj* kiss_format_fixnum(kiss_obj* const out, const kiss_obj* const obj,
                             const kiss_obj* const radix)
{
     long int i = Kiss_Fixnum(obj);
     long int r = Kiss_Fixnum(radix);
     int is_minus = i < 0 ? 1 : 0;
     wchar_t* digits = L"0123456789ABCDEFGHIJKLMNOPQRSTUV";
     kiss_obj* stack = KISS_NIL;
     long int quotient, remainder;
     if (r < 2 || r > 32) {
	  Kiss_Err(L"Radix must be between 2 and 36, inclusive ~S", radix);
     }
     /* be careful when changing sign, abs(LONG_MIN) == abs(LONG_MAX) + 1 */
     remainder = i % r;
     if (remainder < 0) { remainder = -remainder; }
     kiss_push(kiss_make_char(*(digits + remainder)), &stack);
    
     quotient = i / r;
     if (quotient < 0) { quotient = -quotient; };
    
     while (quotient > 0) {
	  remainder = quotient % r;
	  kiss_push(kiss_make_char(*(digits + remainder)), &stack);
	  quotient = quotient / r;
     }
     if (is_minus) { kiss_push(kiss_make_char(L'-'), &stack); }
     kiss_format_string(out, (kiss_obj*)kiss_chars_to_str(stack), KISS_NIL);
     return KISS_NIL;
}

kiss_obj* kiss_format_bignum(kiss_obj* const out, const kiss_obj* const obj,
                             const kiss_obj* const radix)
{
     char* str = mpz_get_str(NULL, Kiss_Fixnum(radix), Kiss_Bignum(obj)->mpz);
     wchar_t* wcs = kiss_mbstowcs(str);
     free(str);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(wcs), KISS_NIL);
     free(wcs);
     return KISS_NIL;
}

kiss_obj* kiss_format_integer(kiss_obj* out, kiss_obj* obj, kiss_obj* radix) {
     Kiss_Integer(obj);
     if (KISS_IS_FIXNUM(obj)) {
          return kiss_format_fixnum(out, obj, radix);
     } else {
          return kiss_format_bignum(out, obj, radix);
     }
     return KISS_NIL;
}

/* function: (format-float output-stream float) -> <null> */
kiss_obj* kiss_format_float(kiss_obj* const out, const kiss_obj* const obj) {
     kiss_float_t* f = Kiss_Float(obj);
     wchar_t wcs[100];
     if (swprintf(wcs, 100, L"%#g", f->f) < 0) {
          fwprintf(stderr, L"kiss_format_float: internal error. buffer is too small.");
          abort();
     }
     kiss_format_string(out, (kiss_obj*)kiss_make_string(wcs), KISS_NIL);
     return KISS_NIL;
}

static kiss_obj* kiss_format_stream(kiss_obj* out, kiss_obj* obj) {
     if (KISS_IS_INPUT_STREAM(obj)) {
          if (KISS_IS_OUTPUT_STREAM(obj)) {
               kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<I/O "), KISS_NIL);
          } else if (obj == kiss_standard_input()) {
               kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<Standard input "), KISS_NIL);
          } else {
               kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<Input "), KISS_NIL);
          }
     } else if (KISS_IS_OUTPUT_STREAM(obj)) {
          if (obj == kiss_standard_output()) {
               kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<Standard output "), KISS_NIL);
          } else if (obj == kiss_error_output()) {
               kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<Error output "), KISS_NIL);
          } else {
               kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<Output "), KISS_NIL);
          }
     } else {
          fwprintf(stderr, L"kiss_format_stream: internal error. stream with unknown I/O direction.");
          abort();
     }
     
     if (KISS_IS_CHARACTER_STREAM(obj)) {
          kiss_format_string(out, (kiss_obj*)kiss_make_string(L"character "), KISS_NIL);
     } else if (KISS_IS_BYTE_STREAM(obj)) {
          kiss_format_string(out, (kiss_obj*)kiss_make_string(L"byte "), KISS_NIL);
     } else {
          fwprintf(stderr, L"kiss_format_stream: internal error. stream with unknown element.");
          abort();
     }

     if (KISS_IS_FILE_STREAM(obj)) {
          kiss_format_string(out, (kiss_obj*)kiss_make_string(L"file stream"), KISS_NIL);
     } else if (KISS_IS_STRING_STREAM(obj)) {
          kiss_format_string(out, (kiss_obj*)kiss_make_string(L"string stream"), KISS_NIL);
     } else {
          fwprintf(stderr, L"kiss_format_stream: internal error. stream with unknown source.");
          abort();
     }
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L"("), KISS_NIL);
     kiss_format_pointer(out, obj);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L")>"), KISS_NIL);
     return KISS_NIL;
}

static kiss_obj* kiss_format_function(kiss_obj* out, kiss_obj* obj) {
     kiss_function_t* f = Kiss_LFunction(obj);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<"), KISS_NIL);
     if (f->name == NULL) {
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L"anonymous function: "), KISS_NIL);
     } else {
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L"function "), KISS_NIL);
	  kiss_format_symbol(out, (kiss_obj*)f->name, KISS_T);
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L": "), KISS_NIL);
     }
     kiss_format_object(out, f->lambda, KISS_T);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L">"), KISS_NIL);
     return KISS_NIL;
}

static kiss_obj* kiss_format_macro(kiss_obj* out, kiss_obj* obj) {
     kiss_function_t* f = Kiss_LMacro(obj);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<"), KISS_NIL);
     assert(f->name != NULL);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L"macro "), KISS_NIL);
     kiss_format_symbol(out, (kiss_obj*)f->name, KISS_T);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L": "), KISS_NIL);
     kiss_format_object(out, f->lambda, KISS_T);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L">"), KISS_NIL);
     return KISS_NIL;
}

kiss_obj* kiss_format_pointer(kiss_obj* out, kiss_obj* obj) {
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#x"), KISS_NIL);
     kiss_format_integer(out, (kiss_obj*)kiss_make_fixnum((long int)obj),
			 (kiss_obj*)kiss_make_fixnum(16));
     return KISS_NIL;
}

static kiss_obj* kiss_format_cfunction(kiss_obj* out, kiss_obj* obj) {
     kiss_cfunction_t* f = Kiss_CFunction(obj);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<c function "),
			KISS_NIL);
     kiss_format_symbol(out, (kiss_obj*)f->name, KISS_T);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L">"), KISS_NIL);
     return KISS_NIL;
}

static kiss_obj* kiss_format_cmacro(kiss_obj* out, kiss_obj* obj) {
     kiss_cfunction_t* f = Kiss_CSpecial(obj);
     if (f->name->flags & KISS_SPECIAL_OPERATOR) {
          kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<special operator "), KISS_NIL);
     } else if (f->name->flags & KISS_DEFINING_OPERATOR) {
          kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<defining operator "), KISS_NIL);
     } else {
          kiss_format_string(out, (kiss_obj*)kiss_make_string(L"#<c macro "), KISS_NIL);
     }
     kiss_format_symbol(out, (kiss_obj*)f->name, KISS_T);
     kiss_format_string(out, (kiss_obj*)kiss_make_string(L">"), KISS_NIL);
     return KISS_NIL;
}

kiss_obj* kiss_format_ilos_obj(kiss_obj* out, kiss_obj* object) {
     kiss_ilos_obj_t* obj = Kiss_ILOS_Obj(object);
     kiss_obj* plist = obj->plist;
     kiss_obj* name = kiss_plist_get(plist, (kiss_obj*)&KISS_Skw_name);
     kiss_obj* class = kiss_plist_get(plist, (kiss_obj*)&KISS_Skw_class);
     kiss_obj* class_name = kiss_plist_get(((kiss_ilos_obj_t*)class)->plist, (kiss_obj*)&KISS_Skw_name);
     if (name == KISS_NIL) {
          name = (kiss_obj*)kiss_make_string(L"an instance");
     }
     kiss_format(out, (kiss_obj*)kiss_make_string(L"#{ILOS: ~A of ~S}"),
                 kiss_c_list(2, name, class_name));
     return KISS_NIL;
}


/* (format-fresh-line output-stream) -> <null> */
kiss_obj* kiss_format_fresh_line(kiss_obj* output) {
     kiss_stream_t* out = Kiss_Output_Char_Stream(output);
     if (out->column == 0) { return KISS_NIL; }
     else { return kiss_format_char(output, kiss_make_char(L'\n')); }
}

/* function: (format-object output-stream obj escape-p) -> <null> */
kiss_obj* kiss_format_object(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep) {
     switch (KISS_OBJ_TYPE(obj)) {
     case KISS_SYMBOL: kiss_format_symbol(out, obj, escapep); break;
     case KISS_CONS: kiss_format_list(out, obj, escapep); break;
     case KISS_STRING:
	  if (escapep != KISS_NIL) {
	       kiss_format_char(out, kiss_make_char(L'"'));
	  }
	  kiss_format_string(out, obj, escapep);
	  if (escapep != KISS_NIL) {
	       kiss_format_char(out, kiss_make_char(L'"'));
	  }
	  break;
     case KISS_GENERAL_VECTOR: kiss_format_general_vector(out, obj, escapep);
	  break;
     case KISS_GENERAL_ARRAY_S: kiss_format_general_array(out, obj, escapep);
	  break;
     case KISS_CHARACTER: {
	  if (escapep == KISS_NIL) { kiss_format_char(out, obj); }
	  else                     { kiss_format_escaped_char(out, obj); }
	  break;
     }
     case KISS_FIXNUM: case KISS_BIGNUM:
          kiss_format_integer(out, obj, (kiss_obj*)kiss_make_fixnum(10));
	  break;
     case KISS_FLOAT: kiss_format_float(out, obj); break;
     case KISS_STREAM: kiss_format_stream(out, obj); break;
     case KISS_LFUNCTION: kiss_format_function(out, obj); break;
     case KISS_LMACRO: kiss_format_macro(out, obj); break;
     case KISS_CFUNCTION: kiss_format_cfunction(out, obj); break;
     case KISS_CSPECIAL: kiss_format_cmacro(out, obj); break;
     case KISS_ILOS_OBJ: kiss_format_ilos_obj(out, obj); break;
     default:
	  kiss_format_string(out, (kiss_obj*)kiss_make_string(L"unprintable object"), escapep);
	  break;
     }
     return KISS_NIL;
}

/* function: (format output-stream format-string obj*) -> <null> */
kiss_obj* kiss_format(kiss_obj* out, kiss_obj* format, kiss_obj* args) {
     size_t i = 0;
     size_t n = kiss_c_length(format);
     kiss_string_t* str = Kiss_String(format);
     wchar_t c;
     while (i < n) {
	  c = str->str[i++];
	  if (c != L'~') {
	       kiss_format_char(out, kiss_make_char(c));
	  } else {
	       c = str->str[i++];
	       switch (c) {
	       case L'A':
		    /* obj is printed as it would with ~S, but without escape characters.
		       Characters are output directly without any conversion.
		       That is, the output generated using this format directive is
		       suitable for being read by a human reader.
		       This effect is implemented by (format-object output-stream obj nil) */
		    kiss_format_object(out, kiss_car(args), KISS_NIL);
		    args = KISS_CDR(args);
		    break;
	       case L'B':
		    /* This effect is implemented by (format-integer output-stream obj 2) */
		    kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_fixnum(2));
		    args = KISS_CDR(args);
		    break;
	       case L'C':
		    /* This effect is implemented by (format-char output-stream obj)*/
		    kiss_format_char(out, kiss_car(args));
		    args = KISS_CDR(args);
		    break;
	       case L'D':
		    /* This effect is implemented by (format-integer output-stream obj 10) */
		    kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_fixnum(10));
		    args = KISS_CDR(args);
		    break;
	       case L'G':
		    /* This effect is implemented by (format-float output-stream obj) */
		    kiss_format_float(out, kiss_car(args));
		    args = KISS_CDR(args);
		    break;
	       case L'O':
		    /* This effect is implemented by (format-integer output-stream obj 8) */
		    kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_fixnum(8));
		    args = KISS_CDR(args);
		    break;
	       case L'S':
		    /* This format directive outputs the textual representation of
		       obj, with escape characters a s needed. That is, the output
		       generated using this format directive is suitable for input to
		       the function read. This effect is implemented by 
		       (format-object output-stream obj t). */
		    kiss_format_object(out, kiss_car(args), KISS_T);
		    args = KISS_CDR(args);
		    break;
	       case L'X':
		    /* This effect is implemented by (format-integer output-stream obj 16)*/
		    kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_fixnum(16));
		    args = KISS_CDR(args);
		    break;
	       case L'%':
		    /* (format-char output-stream #\newline) */
		    kiss_format_char(out, kiss_make_char('\n'));
		    break;
	       case L'&':
		    /* conditional newline: output a #\newline character if it cannot be
		       determined that the output stream is at the beginning of a fresh line;
		       This effect is implemented by (format-fresh-line output-stream).*/
		    /* kiss_fresh_line(out); */
		    kiss_format_fresh_line(out);
		    break;
	       case L'~':
		    /* tilde: output a tilde ~̃. This effect is implemented by
		       (format-char output-stream #\~) */
		    kiss_format_char(out, kiss_make_char('~'));
		    break;
	       case L'0': case L'1': case L'2': case L'3': case L'4':
	       case L'5': case L'6': case L'7': case L'8': case L'9': {
		    wchar_t* tailptr;
		    long int m = wcstol(str->str + i - 1, &tailptr, 10);
		    if (*tailptr != L'T' && *tailptr != L'R') {
			 Kiss_Err(L"Invalid format string ~S", format);
		    }
		    if (*tailptr == L'T') {
			 m = m - ((kiss_stream_t*)out)->column - 1;
			 kiss_format_char(out, kiss_make_char(L' '));
			 for (; m > 0; --m) {
			      kiss_format_char(out, kiss_make_char(L' '));
			 }
			 i = tailptr - str->str + 1;
			 break;
		    } else if (*tailptr == L'R') {
			 kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_fixnum(m));
			 args = KISS_CDR(args);
			 break;
		    } else {
			 fwprintf(stderr, L"format: internal error unsupported format\n");
			 exit(EXIT_FAILURE);
		    }
	       }
	       default:
		    kiss_format_string(out,
				       (kiss_obj*)kiss_make_string(L"unsupported format char"),
				       KISS_NIL);
		    break;
	       }
	  }
     }
     return KISS_NIL;
}

kiss_obj* kiss_print(kiss_obj* obj) {
     kiss_format_object(kiss_standard_output(), obj, KISS_T);
     kiss_format_char(kiss_standard_output(), kiss_make_char(L'\n'));
     fflush(stdout);
     return KISS_NIL;
}
