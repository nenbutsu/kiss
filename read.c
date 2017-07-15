/*  -*- coding: utf-8 -*-
  read.c --- defines the reader mechanism of ISLisp processor KISS.

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

kiss_symbol_t KISS_Sfunction, KISS_Slist, KISS_Sappend_s;
#define KISS_SFUNCTION ((kiss_obj*)(&KISS_Sfunction))
#define KISS_SLIST     ((kiss_obj*)(&KISS_Slist))
#define KISS_SAPPEND_S   ((kiss_obj*)(&KISS_Sappend_s))
kiss_symbol_t KISS_Udot, KISS_Urparen, KISS_Ucomma, KISS_Ucomma_at;
#define KISS_DOT       ((kiss_obj*)(&KISS_Udot))
#define KISS_RPAREN    ((kiss_obj*)(&KISS_Urparen))
#define KISS_COMMA     ((kiss_obj*)(&KISS_Ucomma))
#define KISS_COMMA_AT  ((kiss_obj*)(&KISS_Ucomma_at))

static kiss_obj* kiss_read_lexeme(kiss_obj* in);

/*
  Delimiters are separators along with the following characters:
  ( ) ‘ , ’

  6.1 Separators
  Separators are as follows: blank, comments, newline, and an
  implementation-defined set of characters, (e.g., tabs). Separators
  have no meaning and can be replaced by each other without changing the
  meaning of the ISLISP text.

 */
static int kiss_is_delimiter(wint_t c) {
    return iswspace(c) || wcschr(L"()`,'\"#;", c);
}


static kiss_obj* kiss_read_list(kiss_obj* in) {
    kiss_cons_t* p = (kiss_cons_t*)kiss_cons(KISS_NIL, KISS_NIL);
    kiss_cons_t* tail = p;
    while(1) {
	kiss_obj* x = kiss_read_lexeme(in);
	if (x == NULL) Kiss_Err(L"Missing closing parenthesis");
	if (x == KISS_RPAREN) { break; }
	if (x == KISS_DOT) {
	    kiss_obj* rest;
	    if (p == tail) { Kiss_Err(L"Illegal consing dot"); }
	    rest = kiss_read_lexeme(in);
	    if (rest == NULL || rest == KISS_RPAREN || rest == KISS_DOT)
		Kiss_Err(L"Illegal consing dot");
	    if (kiss_read_lexeme(in) != KISS_RPAREN)
		Kiss_Err(L"Closing parenthesis is needed");
	    tail->cdr = rest;
	    break;
	}
	tail->cdr = kiss_cons(x, KISS_NIL);
	tail = (kiss_cons_t*)tail->cdr;
    }
    return p->cdr;
}

static kiss_obj* kiss_read_string(kiss_obj* in) {
    kiss_obj* stack = KISS_NIL;
    while (1) {
	kiss_obj* x = kiss_cread_char(in, KISS_NIL, KISS_EOS);
	if (x == KISS_EOS) { Kiss_Err(L"Missing closing double quotation"); }
	else {
	    kiss_character_t* c = (kiss_character_t*)x;
	    switch (c->c) {
	    case L'"': goto end;
	    case L'\\':
		x = kiss_cread_char(in, KISS_NIL, KISS_EOS);
		if (x == KISS_EOS) {
		    Kiss_Err(L"Missing char after backquote in a string");
		}
		kiss_push(x, &stack);
		break;
	    default:
		kiss_push((kiss_obj*)c, &stack); break;
	    }
	}
    }
end:
    return (kiss_obj*)kiss_chars_to_str(kiss_nreverse(stack));
}

static void kiss_push_lexeme_char(kiss_character_t* c) {
    kiss_environment_t* env = Kiss_Get_Environment();
    env->lexeme_chars = kiss_cons((kiss_obj*)c, env->lexeme_chars);
}

static void kiss_read_single_escape(kiss_obj* in) {
    kiss_obj* p = kiss_cread_char(in, KISS_NIL, KISS_EOS);
    if (p == KISS_EOS) Kiss_Err(L"Missing single-escaped character");
    kiss_push_lexeme_char((kiss_character_t*)p);
}

static void kiss_read_multiple_escape(kiss_obj* in) {
    while(1) {
	kiss_obj* p = kiss_cread_char(in, KISS_NIL, KISS_EOS);
	kiss_character_t* c;
	if (p == KISS_EOS) Kiss_Err(L"Missing closing multiple-escape");
	c = (kiss_character_t*)p;
	switch (c->c) {
	case L'|': return;
	case L'\\': kiss_read_single_escape(in); continue;
	default: kiss_push_lexeme_char(c); break;
	}
    }
}

static void kiss_collect_lexeme_chars(kiss_obj* in, int* escaped) {
    kiss_obj* p;
    kiss_character_t* c;
    *escaped = 0;
    while (1) {
	p = kiss_cpreview_char(in, KISS_NIL, KISS_EOS);
	if (p == KISS_EOS) { return; }
	c = (kiss_character_t*)p;
	if (kiss_is_delimiter(c->c)) { return; }
	switch (c->c) {
	case L'|':
	    *escaped = 1;
	    kiss_cread_char(in, KISS_NIL, KISS_NIL);
	    kiss_read_multiple_escape(in);
	    break;
	case L'\\':
	    *escaped = 1;
	    kiss_cread_char(in, KISS_NIL, KISS_NIL);
	    kiss_read_single_escape(in);
	    break;
	default:
	    kiss_cread_char(in, KISS_NIL, KISS_NIL);
	    c->c = tolower(c->c);
	    kiss_push_lexeme_char(c);
	    break;
	}
    }
}

static kiss_obj* kiss_read_lexeme_chars(kiss_obj* in) {
    kiss_environment_t* env = Kiss_Get_Environment();
    int escaped = 0;
    kiss_string_t* str;
    long int i;
    float f;
    wchar_t* tail;
    kiss_collect_lexeme_chars(in, &escaped);
    str = kiss_chars_to_str(kiss_reverse(env->lexeme_chars));
    if (escaped) { return kiss_intern((kiss_obj*)str); }
    errno = 0;
    i = wcstol(str->str, &tail, 10);
    if (tail == str->str + wcslen(str->str)) {
      return (kiss_obj*)kiss_make_integer(i);
    }
    f = wcstof(str->str, &tail);
    if (tail == str->str + wcslen(str->str)) {
      return (kiss_obj*)kiss_make_float(f);
    }

    if (wcscmp(str->str, L".") == 0)   { return KISS_DOT; }
    return kiss_intern((kiss_obj*)str);
}

static kiss_obj* kiss_read_sharp_reader_macro_char(kiss_obj* in) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* p = kiss_cread_char(in, KISS_NIL, KISS_EOS);
    kiss_string_t *char_name, *downcased_char_name;
    if (p == KISS_EOS) {
	Kiss_Err(L"missing character after #\\ macro reader");
    }
    kiss_push(p, &env->lexeme_chars);

    p = kiss_cpreview_char(in, KISS_NIL, KISS_EOS);
    while (p != KISS_EOS && !kiss_is_delimiter(((kiss_character_t*)p)->c)) {
	kiss_cread_char(in, KISS_NIL, KISS_EOS);
	kiss_push(p, &env->lexeme_chars);
	p = kiss_cpreview_char(in, KISS_NIL, KISS_EOS);
    }

    /* given a single character */
    if (kiss_clength(env->lexeme_chars) == 1) {
	return KISS_CAR(env->lexeme_chars);
    }

    /* given a character name */
    char_name = kiss_chars_to_str(kiss_reverse(env->lexeme_chars)); /* used in err msg*/
    for (p = env->lexeme_chars; KISS_IS_CONS(p); p = KISS_CDR(p)) {/* downcase chars*/
	kiss_character_t* c = (kiss_character_t*)KISS_CAR(p);
	c->c = tolower(c->c);
    }
    downcased_char_name = kiss_chars_to_str(kiss_reverse(env->lexeme_chars));
    
    if (wcscmp(downcased_char_name->str, L"newline") == 0) {
	return (kiss_obj*)kiss_make_character(L'\n');
    }
    if (wcscmp(downcased_char_name->str, L"space") == 0) {
	return (kiss_obj*)kiss_make_character(L' ');
    }
    Kiss_Err(L"Invalid character name ~S", char_name);
}


static kiss_obj* kiss_read_sharp_reader_macro(kiss_obj* in) {
    kiss_obj* p = kiss_cread_char(in, KISS_NIL, KISS_EOS);
    kiss_character_t* c;
    if (p == KISS_EOS) { Kiss_Err(L"missing # macro reader character"); }
    c = (kiss_character_t*)p;
    switch (c->c) {
    case L'\'': /* #'f */
	return kiss_clist(2, KISS_SFUNCTION, kiss_cread(in, KISS_T, KISS_NIL));
    case L'\\': /* #\c */
	return kiss_read_sharp_reader_macro_char(in);
    case L'(': /* #() */{
	return kiss_vector(kiss_read_list(in));
    }
    case L'0': case L'1': case L'2': case L'3': case L'4': 
    case L'5': case L'6': case L'7': case L'8': case L'9': {
	 
	 //return kiss_array(kiss_read_list(in));
    }
    default:
	Kiss_Err(L"Illegal # macro reader character ~S", c);
    }
}

static kiss_obj* kiss_read_comma_at(kiss_obj* in) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* p;
    if (env->dynamic_env.backquote_nest == 0) Kiss_Err(L"Out of place ,@");
    env->dynamic_env.backquote_nest--;
    p = kiss_cread(in, KISS_NIL, KISS_EOS);
    if (p == KISS_EOS) { Kiss_Err(L"Missing form after comma-at ,@"); }
    env->dynamic_env.backquote_nest++;
    return kiss_clist(2, KISS_COMMA_AT, p);
}

static kiss_obj* kiss_read_comma(kiss_obj* in) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* p;
    if (env->dynamic_env.backquote_nest == 0) Kiss_Err(L"Out of place ,");
    env->dynamic_env.backquote_nest--;
    p = kiss_cread(in, KISS_NIL, KISS_EOS);
    if (p == KISS_EOS) { Kiss_Err(L"Missing form after comma ,"); }
    env->dynamic_env.backquote_nest++;
    return kiss_clist(2, KISS_COMMA, p);
}

/*
  `ATOM = 'ATOM
  `,FORM = FORM
  `,@FORM => error
  `(FORM1 ,FORM2 ,@FORM3) = (append (list `FORM1) (list FORM2) FORM3)
  `(FORM1 . FORM2)  = (append (list `FORM1) `FORM2)
  `(FORM1 . ,FORM2) = (append (list `FORM1) form)
  `(FORM1 . ,@FORM2) => error
 */
static kiss_obj* kiss_expand_backquote(kiss_obj* p) {
    kiss_obj* stack = KISS_NIL;
    if (!KISS_IS_CONS(p))        /* `ATOM = 'ATOM */
	return kiss_clist(2, KISS_QUOTE, p);
    if (KISS_CAR(p) == KISS_COMMA)    /* `,FORM = FORM */
	return kiss_cadr(p);
    if (KISS_CAR(p) == KISS_COMMA_AT) /* `,@FORM => error */
	Kiss_Err(L"Unquote-splicing(,@) out of list");
    kiss_push(KISS_SAPPEND_S, &stack);
    while (KISS_IS_CONS(p)) { /* `(FORM ...) */
	kiss_obj* x = KISS_CAR(p);
	if (KISS_IS_CONS(x)) {
	    if (KISS_CAR(x) == KISS_COMMA)
		kiss_push(kiss_clist(2, KISS_SLIST, kiss_cadr(x)), &stack);
	    else if (KISS_CAR(x) == KISS_COMMA_AT)
		kiss_push(kiss_cadr(x), &stack);
	    else kiss_push(kiss_clist(2, ((kiss_obj*)(&KISS_Slist)),
				      kiss_expand_backquote(x)),
			   &stack);
	} else {
	    if (x == KISS_COMMA) {
		/* syntax list (a b COMMA c) denotes (a b . ,c) */
		p = kiss_cdr(p);
		kiss_push(kiss_car(p), &stack);
	    } else if (x == KISS_COMMA_AT) {
		/* syntax list (a b COMMA_AT c) denotes (a b . ,@c) */
		Kiss_Err(L"Invalid unquote-splicing(,@)");
	    } else {
		kiss_push(kiss_clist(2, ((kiss_obj*)(&KISS_Slist)),
				     kiss_clist(2, KISS_QUOTE, x)),
			  &stack);
	    }

	}
	p = KISS_CDR(p);
    }
    if (p != KISS_NIL) { kiss_push(kiss_expand_backquote(p), &stack); }
    return kiss_nreverse(stack);
}

static kiss_obj* kiss_read_backquote(kiss_obj* in) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* p;
    env->dynamic_env.backquote_nest++;
    p = kiss_cread(in, KISS_NIL, KISS_EOS);
    if (p == KISS_EOS) { Kiss_Err(L"Missing form after backquote `"); }
    env->dynamic_env.backquote_nest--;
    return kiss_expand_backquote(p);
}

static kiss_obj* kiss_read_lexeme(kiss_obj* in) {
    kiss_environment_t* env = Kiss_Get_Environment();
    env->lexeme_chars = KISS_NIL;
    while(1) {
	kiss_obj* p = kiss_cpreview_char(in, KISS_NIL, KISS_NIL);
	kiss_character_t* c;
	if (p == KISS_NIL) { return NULL; }
	c = (kiss_character_t*)p;
	if (iswspace(c->c)) {
	    kiss_cread_char(in, KISS_NIL, KISS_NIL);
	    continue;
	}

	switch (c->c) {
	case L'(': { kiss_cread_char(in, KISS_NIL, KISS_NIL);
		return kiss_read_list(in); }
	case L')': { kiss_cread_char(in, KISS_NIL, KISS_NIL);
		return KISS_RPAREN; }
	case L'`': { kiss_cread_char(in, KISS_NIL, KISS_NIL);
		return kiss_read_backquote(in); }
	case L',':
	    kiss_cread_char(in, KISS_NIL, KISS_NIL);
	    p = kiss_cpreview_char(in, KISS_NIL, KISS_NIL);
	    if (p == KISS_NIL) Kiss_Err(L"Stray unquote ,");
	    c = (kiss_character_t*)p;
	    if (c->c == L'@') {
		kiss_cread_char(in, KISS_NIL, KISS_NIL);
		return kiss_read_comma_at(in);
	    } else {
		return kiss_read_comma(in);
	    }
	case L'\'': {
	    kiss_obj* obj;
	    kiss_cread_char(in, KISS_NIL, KISS_NIL);
	    obj = kiss_cread(in, KISS_NIL, KISS_EOS);
	    if (obj == KISS_EOS) Kiss_Err(L"Stray quote '");
	    return kiss_clist(2, KISS_QUOTE, obj);
	}
	case L';':
	    do {
		p = kiss_cread_char(in, KISS_NIL, KISS_NIL);
	    } while (p != KISS_NIL && ((kiss_character_t*)p)->c != L'\n');
	    if (p == KISS_NIL) return NULL;
	    break;
	case L'"':
	    kiss_cread_char(in, KISS_NIL, KISS_NIL);
	    return kiss_read_string(in);
	case L'#': {
	    kiss_cread_char(in, KISS_NIL, KISS_NIL);
	    return kiss_read_sharp_reader_macro(in);
	}
	default:
	    return kiss_read_lexeme_chars(in);
	}
    }
}

kiss_obj* kiss_cread(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val) {
    kiss_obj* p = kiss_read_lexeme(in);
    if (p == NULL) {
	if (eos_err_p != KISS_NIL) {
	    Kiss_End_Of_Stream_Error(in);
	} else {
	    return eos_val;
	}
    }
    else if (p == KISS_RPAREN)  { Kiss_Err(L"Illegal right parenthesis"); }
    else if (p == KISS_DOT)     { Kiss_Err(L"Illegal consing dot"); }
    else if (KISS_IS_SYMBOL(p)) { assert(kiss_is_interned((kiss_symbol_t*)p)); }
    return p;
}

/* function: (read [input-stream [eos-error-p [eos-value]]]) -> <object> */
kiss_obj* kiss_read(kiss_obj* args) {
    kiss_obj* in = kiss_standard_input();
    kiss_obj* eos_err_p = KISS_T;
    kiss_obj* eos_val = KISS_NIL;
    if (KISS_IS_CONS(args)) {
	in = KISS_CAR(args);
	args = KISS_CDR(args);
	if (KISS_IS_CONS(args)) { /* process args */
	    eos_err_p = KISS_CAR(args);
	    args = KISS_CDR(args);
	    if (KISS_IS_CONS(args)) {
		eos_val = KISS_CAR(args);
	    }
	}
    }
    return kiss_cread(in, eos_err_p, eos_val);
}
