/*  -*- coding: utf-8 -*-
  format_object.c --- defines the formatting mechanism of ISLisp processor KISS.

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

kiss_obj* kiss_format_string(kiss_obj* out, kiss_obj* str, kiss_obj* escapep) {
    kiss_string_t* s = Kiss_String(str);
    char* p;
    for (p = s->str; *p != '\0'; p++) {
	if (escapep != KISS_NIL) {
	    switch (*p) {
	    case '"':
		kiss_format_char(out, (kiss_obj*)kiss_make_character('\\'));
		kiss_format_char(out, (kiss_obj*)kiss_make_character('"'));
		break;
	    case '\\':
		kiss_format_char(out, (kiss_obj*)kiss_make_character('\\'));
		kiss_format_char(out, (kiss_obj*)kiss_make_character('\\'));
		break;
	    default:
		kiss_format_char(out, (kiss_obj*)kiss_make_character(*p));
	    }
	} else {
	    kiss_format_char(out, (kiss_obj*)kiss_make_character(*p));
	}
    }
    return KISS_NIL;
}

kiss_obj* kiss_format_list(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep) {
    kiss_obj* p = Kiss_List(obj);
    if (obj == KISS_NIL) {
	kiss_format_string(out, (kiss_obj*)kiss_make_string("()"), KISS_NIL);
	return KISS_NIL;
    }
    kiss_format_char(out, (kiss_obj*)kiss_make_character('('));
    kiss_format_object(out, KISS_CAR(p), escapep);
    for (p = KISS_CDR(p); KISS_IS_CONS(p); p = KISS_CDR(p)) {
	kiss_format_char(out, (kiss_obj*)kiss_make_character(' '));
	kiss_format_object(out, KISS_CAR(p), escapep);
    } 
    if (p != KISS_NIL) {
	kiss_format_string(out, (kiss_obj*)kiss_make_string(" . "), KISS_NIL);
	kiss_format_object(out, p, escapep);
    }
    kiss_format_char(out, (kiss_obj*)kiss_make_character(')'));
    return KISS_NIL;
}

kiss_obj* kiss_format_general_vector(kiss_obj* out, kiss_obj* obj,
				     kiss_obj* escapep)
{
    kiss_general_vector_t* v = Kiss_General_Vector(obj);
    size_t i;
    if (v->n == 0) {
	kiss_format_string(out, (kiss_obj*)kiss_make_string("#()"), KISS_NIL);
	return KISS_NIL;
    }
    kiss_format_char(out, (kiss_obj*)kiss_make_character('#'));
    kiss_format_char(out, (kiss_obj*)kiss_make_character('('));
    kiss_format_object(out, v->v[0], escapep);
    for (i = 1; i < v->n; i++) {
	kiss_format_char(out, (kiss_obj*)kiss_make_character(' '));
	kiss_format_object(out, v->v[i], escapep);
    } 
    kiss_format_char(out, (kiss_obj*)kiss_make_character(')'));
    return KISS_NIL;
    
}

static int kiss_is_simple_name(char* name) {
    if (strcmp(name, "+")  == 0 || strcmp(name, "-")  == 0 ||
	strcmp(name, "1+") == 0 || strcmp(name, "1-") == 0) {
	return 1;
    }
    if (!isalpha(name[0]) && !strchr("<>/*=?_!$%[]^{}~", name[0])) {
	return 0;
    } else {
	char* p = name + 1;
	while (*p != '\0') {
	    char c = *p++;
	    if (!isalnum(c) && !strchr("+-<>/*=?_!$%[]^{}~", c)) {
		return 0;
	    }
	}
	return 1;
    }
}

static kiss_obj* kiss_format_escaped_symbol(kiss_obj* out, kiss_obj* obj) {
    kiss_symbol_t* symbol = Kiss_Symbol(obj);
    char* p = symbol->name;
    kiss_format_char(out, (kiss_obj*)kiss_make_character('|'));
    for (p = symbol->name; *p != '\0'; p++) {
	wchar_t c = *p;
	switch (c) {
	case '|':
	    kiss_format_string(out, (kiss_obj*)kiss_make_string("\\|"),
			       KISS_NIL);
	    break;
	case '\\':
	    kiss_format_string(out, (kiss_obj*)kiss_make_string("\\\\"),
			       KISS_NIL); break;
	default:
	    kiss_format_char(out, (kiss_obj*)kiss_make_character(c));
	    break;
	}
    }
    kiss_format_char(out, (kiss_obj*)kiss_make_character('|'));
    return KISS_NIL;
}

kiss_obj* kiss_format_symbol(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep) {
    kiss_symbol_t* symbol = Kiss_Symbol(obj);
    if (!kiss_is_interned(symbol)) {
	kiss_format_string(out, (kiss_obj*)kiss_make_string("#:"), KISS_NIL);
    }
    if (escapep == KISS_NIL || kiss_is_simple_name(symbol->name)) {
	kiss_format_string(out, (kiss_obj*)kiss_make_string(symbol->name),
			   KISS_NIL);
    } else {
	kiss_format_escaped_symbol(out, obj);
    }
    return KISS_NIL;
}

static kiss_obj* kiss_format_escaped_char(kiss_obj* out, kiss_obj* obj) {
    kiss_character_t* c = Kiss_Character(obj);
    switch (c->c) {
    case '\n':
	kiss_format_string(out, (kiss_obj*)kiss_make_string("#\\newline"),
			   KISS_NIL); break;
    case ' ':
	kiss_format_string(out, (kiss_obj*)kiss_make_string("#\\space"),
			   KISS_NIL); break;
    default:
	kiss_format_string(out, (kiss_obj*)kiss_make_string("#\\"),
			   KISS_NIL);
	kiss_format_char(out, obj);
    }
    return KISS_NIL;
}

kiss_obj* kiss_format_integer(kiss_obj* out, kiss_obj* obj, kiss_obj* radix) {
    kiss_integer_t* i = Kiss_Integer(obj);
    kiss_integer_t* r= Kiss_Integer(radix);
    int is_minus = i->i < 0 ? 1 : 0;
    char* digits = "0123456789ABCDEFGHIJKLMNOPQRSTUV";
    kiss_obj* stack = KISS_NIL;
    long int quotient, remainder;
    if (r->i < 2 || r->i > 32) {
	Kiss_Err("Radix must be between 2 and 36, inclusive ~S", radix);
    }
    /* be careful when changing sign, abs(LONG_MIN) == abs(LONG_MAX) + 1 */
    remainder = i->i % r->i;
    if (remainder < 0) { remainder = -remainder; }
    kiss_push((kiss_obj*)kiss_make_character(*(digits + remainder)), &stack);
    
    quotient = i->i / r->i;
    if (quotient < 0) { quotient = -quotient; };
    
    while (quotient > 0) {
	remainder = quotient % r->i;
	kiss_push((kiss_obj*)kiss_make_character(*(digits + remainder)),
		  &stack);
	quotient = quotient / r->i;
    }
    if (is_minus) { kiss_push((kiss_obj*)kiss_make_character('-'), &stack); }
    kiss_format_string(out, (kiss_obj*)kiss_chars_to_str(stack), KISS_NIL);
    return KISS_NIL;
}

kiss_obj* kiss_format_float(kiss_obj* out, kiss_obj* obj) {
  kiss_float_t* f = Kiss_Float(obj);
  char buff[1024];
  char* p;
  /* strfromf(buff, 100, "%f", f->f); */
  sprintf(buff, "%g", f->f);
  if (!strchr(buff, '.') && !strchr(buff, 'e')) {
    strcpy(buff + strlen(buff), ".0");
  }
  for (p = buff; *p != '\0'; p++) {
    kiss_format_char(out, (kiss_obj*)kiss_make_character(*p));
  }
}

kiss_obj* kiss_format_function(kiss_obj* out, kiss_obj* obj) {
    kiss_function_t* f = Kiss_Function(obj);
    kiss_format_string(out, (kiss_obj*)kiss_make_string("#<"), KISS_NIL);
    if (f->name == NULL) {
	kiss_format_string(out, (kiss_obj*)kiss_make_string("anonymous function: "),
			   KISS_NIL);
    } else {
	kiss_format_string(out, (kiss_obj*)kiss_make_string("function "),
			   KISS_NIL);
	kiss_format_symbol(out, (kiss_obj*)f->name, KISS_T);
	kiss_format_string(out, (kiss_obj*)kiss_make_string(": "), KISS_NIL);
    }
    kiss_format_object(out, f->lambda, KISS_T);
    kiss_format_string(out, (kiss_obj*)kiss_make_string(">"), KISS_NIL);
    return KISS_NIL;
}

kiss_obj* kiss_format_macro(kiss_obj* out, kiss_obj* obj) {
    kiss_function_t* f = Kiss_Macro(obj);
    kiss_format_string(out, (kiss_obj*)kiss_make_string("#<"), KISS_NIL);
    assert(f->name != NULL);
    kiss_format_string(out, (kiss_obj*)kiss_make_string("macro "), KISS_NIL);
    kiss_format_symbol(out, (kiss_obj*)f->name, KISS_T);
    kiss_format_string(out, (kiss_obj*)kiss_make_string(": "), KISS_NIL);
    kiss_format_object(out, f->lambda, KISS_T);
    kiss_format_string(out, (kiss_obj*)kiss_make_string(">"), KISS_NIL);
    return KISS_NIL;
}

kiss_obj* kiss_format_pointer(kiss_obj* out, kiss_obj* obj) {
    kiss_format_string(out, (kiss_obj*)kiss_make_string("#x"), KISS_NIL);
    kiss_format_integer(out, (kiss_obj*)kiss_make_integer((long int)obj),
			(kiss_obj*)kiss_make_integer(16));
    return KISS_NIL;
}

kiss_obj* kiss_format_cfunction(kiss_obj* out, kiss_obj* obj) {
    kiss_cfunction_t* f = Kiss_CFunction(obj);
    kiss_format_string(out, (kiss_obj*)kiss_make_string("#<c function "),
		       KISS_NIL);
    kiss_format_symbol(out, (kiss_obj*)f->name, KISS_T);
    kiss_format_string(out, (kiss_obj*)kiss_make_string(": "), KISS_NIL);
    kiss_format_pointer(out, f->fun);
    kiss_format_string(out, (kiss_obj*)kiss_make_string(">"), KISS_NIL);
    return KISS_NIL;
}

kiss_obj* kiss_format_cmacro(kiss_obj* out, kiss_obj* obj) {
    kiss_cfunction_t* f = Kiss_CMacro(obj);
    kiss_format_string(out, (kiss_obj*)kiss_make_string("#<c macro "),
		       KISS_NIL);
    kiss_format_symbol(out, (kiss_obj*)f->name, KISS_T);
    kiss_format_string(out, (kiss_obj*)kiss_make_string(": "), KISS_NIL);
    kiss_format_pointer(out, f->fun);
    kiss_format_string(out, (kiss_obj*)kiss_make_string(">"), KISS_NIL);
    return KISS_NIL;
}

kiss_obj* kiss_format_oo_object(kiss_obj* out, kiss_obj* obj) {
    kiss_cfuncall("kiss::format-oo-object",
		  kiss_clist(3, out, obj, KISS_NIL));
    /*
    kiss_object_t* p = Kiss_OO_Object(obj);
    kiss_format_string(out, (kiss_obj*)kiss_make_string("#<oo-object: "),
		       KISS_NIL);
    kiss_format_list(out, p->info, KISS_T);
    kiss_format_string(out, (kiss_obj*)kiss_make_string(">"), KISS_NIL);
    */
    return KISS_NIL;
}

/* function: (format-object output-stream obj escape-p) → <null> */
kiss_obj* kiss_format_object(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep) {
    switch (obj->type) {
    case KISS_SYMBOL: kiss_format_symbol(out, obj, escapep); break;
    case KISS_CONS: kiss_format_list(out, obj, escapep); break;
    case KISS_STRING: kiss_format_string(out, obj, escapep); break;
    case KISS_GENERAL_VECTOR: kiss_format_general_vector(out, obj, escapep);
	break;
    case KISS_CHARACTER: {
	if (escapep == KISS_NIL) { kiss_format_char(out, obj); }
	else                { kiss_format_escaped_char(out, obj); }
	break;
    }
    case KISS_INTEGER: kiss_format_integer(out, obj,
					   (kiss_obj*)kiss_make_integer(10));
	break;
    case KISS_FLOAT: kiss_format_float(out, obj); break;
    case KISS_OBJECT: kiss_format_oo_object(out, obj); break;
    case KISS_FUNCTION: kiss_format_function(out, obj); break;
    case KISS_MACRO: kiss_format_macro(out, obj); break;
    case KISS_CFUNCTION: kiss_format_cfunction(out, obj); break;
    case KISS_CMACRO: kiss_format_cmacro(out, obj); break;
    default:
	 kiss_format_string(out, (kiss_obj*)kiss_make_string("unprintable object"), escapep);
	 break;
    }
    return KISS_NIL;
}

/* function: (format output-stream format-string obj*) → <null>
*/
kiss_obj* kiss_format(kiss_obj* out, kiss_obj* format, kiss_obj* args) {
     size_t i = 0;
     size_t n = kiss_clength(format);
     kiss_string_t* str = Kiss_String(format);
     char c;
     while (i < n) {
	  c = str->str[i++];
	  if (c != '~') {
	       kiss_format_char(out, (kiss_obj*)kiss_make_character(c));
	  } else {
	       c = str->str[i++];
	       switch (c) {
	       case 'A':
		    kiss_format_object(out, kiss_car(args), KISS_NIL);
		    args = KISS_CDR(args);
		    break;
	       case 'B':
		    kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_integer(2));
		    args = KISS_CDR(args);
		    break;
	       case 'C':
		    kiss_format_char(out, kiss_car(args));
		    args = KISS_CDR(args);
		    break;
	       case 'D':
		    kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_integer(10));
		    args = KISS_CDR(args);
		    break;
	       case 'G':
		    kiss_format_float(out, kiss_car(args));
		    args = KISS_CDR(args);
		    break;
	       case 'O':
		    kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_integer(8));
		    args = KISS_CDR(args);
		    break;
	       case 'S':
		    kiss_format_object(out, kiss_car(args), KISS_T);
		    args = KISS_CDR(args);
		    break;
	       case 'X':
		    kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_integer(16));
		    args = KISS_CDR(args);
		    break;
	       case '%':
		    kiss_format_char(out, (kiss_obj*)kiss_make_character('\n'));
		    break;
	       case '&':
		    /* kiss_fresh_line(out); */
		    kiss_format_char(out, (kiss_obj*)kiss_make_character('\n'));
		    break;
	       case '~':
		    kiss_format_char(out, (kiss_obj*)kiss_make_character('~'));
		    break;
	       case '0': case '1': case '2': case '3': case '4':
	       case '5': case '6': case '7': case '8': case '9': {
		    char* tailptr;
		    long m = strtol(str->str + i - 1, &tailptr, 10);
		    assert(*tailptr == 'T' || *tailptr == 'R');
		    if (*tailptr == 'T') {
			 m = m - ((kiss_file_stream_t*)out)->column - 1;
			 kiss_format_char(out, (kiss_obj*)kiss_make_character(' '));
			 for (; m > 0; --m) {
			      kiss_format_char(out, (kiss_obj*)kiss_make_character(' '));
			 }
			 i = tailptr - str->str + 1;
			 break;
		    } else {
			 kiss_format_integer(out, kiss_car(args), (kiss_obj*)kiss_make_integer(m));
			 args = KISS_CDR(args);
			 break;
		    }
	       }
	       default:
		    kiss_format_string(out,
				       (kiss_obj*)kiss_make_string("unsupported format char"),
				       KISS_NIL);
		    break;
	       }
	  }
     }
     return KISS_NIL;
}

kiss_obj* kiss_print(kiss_obj* obj) {
     if (KISS_IS_STRING(obj)) {
	  kiss_format(kiss_standard_output(), (kiss_obj*)kiss_make_string("\"~S\""),
		      kiss_cons(obj, KISS_NIL));
     } else {
	  kiss_format_object(kiss_standard_output(), obj, KISS_T);
     }
    kiss_format_object(kiss_standard_output(), (kiss_obj*)kiss_make_character('\n'), KISS_NIL);
    fflush(stdout);
    return KISS_NIL;
}
