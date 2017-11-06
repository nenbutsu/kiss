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
kiss_symbol_t KISS_Udot, KISS_Urparen, KISS_Ucomma, KISS_Ucomma_at;
#define KISS_DOT       ((kiss_obj*)(&KISS_Udot))
#define KISS_RPAREN    ((kiss_obj*)(&KISS_Urparen))
#define KISS_COMMA     ((kiss_obj*)(&KISS_Ucomma))
#define KISS_COMMA_AT  ((kiss_obj*)(&KISS_Ucomma_at))

static kiss_obj* kiss_read_lexeme(const kiss_obj* const in);

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


static kiss_obj* kiss_read_list(const kiss_obj* const in) {
     kiss_cons_t head;
     kiss_init_cons(&head, KISS_NIL, KISS_NIL);
     kiss_cons_t* tail = &head;
     while(1) {
          kiss_obj* x = kiss_read_lexeme(in);
          if (x == NULL) Kiss_Err(L"Missing closing parenthesis");
          if (x == KISS_RPAREN) { break; }
          if (x == KISS_DOT) {
               if (tail == &head) { Kiss_Err(L"Illegal consing dot"); }
               kiss_obj* const rest = kiss_read_lexeme(in);
               if (rest == NULL || rest == KISS_RPAREN || rest == KISS_DOT) {
                    Kiss_Err(L"Illegal consing dot");
               }
               if (kiss_read_lexeme(in) != KISS_RPAREN) {
                    Kiss_Err(L"Closing parenthesis is needed");
               }
               tail->cdr = rest;
               break;
          }
          tail->cdr = kiss_cons(x, KISS_NIL);
          tail = (kiss_cons_t*)tail->cdr;
     }
     return head.cdr;
}

static kiss_obj* kiss_read_string(const kiss_obj* const in) {
     kiss_cons_t head;
     kiss_init_cons(&head, KISS_NIL, KISS_NIL);
     kiss_cons_t* tail = &head;
     while (1) {
          kiss_obj* x = kiss_c_read_char(in, KISS_NIL, KISS_EOS);
          if (x == KISS_EOS) { Kiss_Err(L"Missing closing double quotation"); }
          else {
               wchar_t c = kiss_wchar(x);
               switch (c) {
               case L'"': goto end;
               case L'\\':
                    x = kiss_c_read_char(in, KISS_NIL, KISS_EOS);
                    if (x == KISS_EOS) {
                         Kiss_Err(L"Missing character after backslash in a string");
                    }
                    tail->cdr = kiss_cons(x, KISS_NIL);
                    break;
               default:
                    tail->cdr = kiss_cons(x, KISS_NIL);
                    break;
               }
               tail = (kiss_cons_t*)tail->cdr;
          }
     }
end:
     return (kiss_obj*)kiss_chars_to_str(head.cdr);
}

static void kiss_push_lexeme_char(const kiss_obj* const x) {
    kiss_environment_t* env = Kiss_Get_Environment();
    env->lexeme_chars = kiss_cons(x, env->lexeme_chars);
}

static void kiss_read_single_escaped_lexeme_char(const kiss_obj* const in) {
    kiss_obj* x = kiss_c_read_char(in, KISS_NIL, KISS_EOS);
    if (x == KISS_EOS) Kiss_Err(L"Missing single-escaped character");
    kiss_push_lexeme_char(x);
}

static void kiss_read_multiple_escaped_lexeme_chars(const kiss_obj* const in) {
     while(1) {
          kiss_obj* x = kiss_c_read_char(in, KISS_NIL, KISS_EOS);
          if (x == KISS_EOS) Kiss_Err(L"Missing closing multiple-escape");
          wchar_t c = kiss_wchar(x);
          switch (c) {
          case L'|': return;
          case L'\\': kiss_read_single_escaped_lexeme_char(in); continue;
          default: kiss_push_lexeme_char(x); break;
          }
     }
}

static void kiss_collect_lexeme_chars(const kiss_obj* const in, int* const escaped) {
    *escaped = 0;
    while (1) {
	kiss_obj* x = kiss_c_preview_char(in, KISS_NIL, KISS_EOS);
	if (x == KISS_EOS) { return; }
	wchar_t c = kiss_wchar(x);
	if (kiss_is_delimiter(c)) { return; }
	switch (c) {
	case L'|':
	    *escaped = 1;
	    kiss_read_multiple_escaped_lexeme_chars(in);
	    break;
	case L'\\':
	    kiss_c_read_char(in, KISS_NIL, KISS_NIL);
	    kiss_read_single_escaped_lexeme_char(in);
	    break;
	default:
	    kiss_c_read_char(in, KISS_NIL, KISS_NIL);
            if (!*escaped) {
                 c = towlower(c);
            }
	    kiss_push_lexeme_char(kiss_make_character(c));
	    break;
	}
    }
}

static kiss_obj* kiss_read_lexeme_chars(const kiss_obj* const in) {
    kiss_environment_t* const env = Kiss_Get_Environment();
    int escaped = 0;
    kiss_collect_lexeme_chars(in, &escaped);
    kiss_string_t* const str = kiss_chars_to_str(kiss_reverse(env->lexeme_chars));
    if (escaped) { return kiss_intern((kiss_obj*)str); }

    if (wcscmp(str->str, L".") == 0) {
         return KISS_DOT;
    }

    if (wcscmp(str->str, L"+") == 0 || wcscmp(str->str, L"-") == 0) {
         return kiss_intern((kiss_obj*)str);
    }


    kiss_obj* p = kiss_c_parse_number((kiss_obj*)str);

    if (p != NULL) {
         return p;
    }
    return kiss_intern((kiss_obj*)str);
}

static kiss_obj* kiss_read_sharp_reader_macro_char(const kiss_obj* const in) {
    kiss_environment_t* env = Kiss_Get_Environment();
    kiss_obj* p = kiss_c_read_char(in, KISS_NIL, KISS_EOS);
    if (p == KISS_EOS) {
	Kiss_Err(L"missing character after #\\ macro reader");
    }
    kiss_push(p, &env->lexeme_chars);

    p = kiss_c_preview_char(in, KISS_NIL, KISS_EOS);
    while (p != KISS_EOS && !kiss_is_delimiter(kiss_wchar(p))) {
	kiss_c_read_char(in, KISS_NIL, KISS_EOS);
	kiss_push(p, &env->lexeme_chars);
	p = kiss_c_preview_char(in, KISS_NIL, KISS_EOS);
    }

    /* given a single character */
    if (kiss_c_length(env->lexeme_chars) == 1) {
	return KISS_CAR(env->lexeme_chars);
    }

    /* given a character name */
    
    kiss_string_t* char_name = kiss_chars_to_str(kiss_reverse(env->lexeme_chars)); // used in err msg*/
    for (p = env->lexeme_chars; KISS_IS_CONS(p); p = KISS_CDR(p)) {/* downcase chars*/
         wchar_t c = kiss_wchar(KISS_CAR(p));
         kiss_set_car(kiss_make_character(towlower(c)), p);
    }
    kiss_string_t* downcased_char_name = kiss_chars_to_str(kiss_reverse(env->lexeme_chars));
    
    if (wcscmp(downcased_char_name->str, L"newline") == 0) {
	return kiss_make_character(L'\n');
    }
    if (wcscmp(downcased_char_name->str, L"space") == 0) {
	return kiss_make_character(L' ');
    }
    Kiss_Err(L"Invalid character name ~S", char_name);
}

static kiss_obj* kiss_list_to_array_dimensions(const size_t rank, const kiss_obj* list) {
     kiss_obj* p = KISS_NIL;
     for (size_t i = 0; i < rank; i++) {
	  kiss_push(kiss_length(list), &p);
	  list = kiss_car(list);
     }
     return kiss_nreverse(p);
}

static void kiss_fill_array(const size_t rank, const kiss_obj* list, const kiss_general_vector_t* const vector) {
     if (rank == 1) {
	  for (size_t i = 0; i < vector->n; i++) {
	       vector->v[i] = kiss_car(list);
	       list = kiss_cdr(list);
	  }
     } else {
	  for (size_t i = 0; i < vector->n; i++) {
	       kiss_fill_array(rank - 1, kiss_car(list), Kiss_General_Vector(vector->v[i]));
	       list = kiss_cdr(list);
	  }
     }
}

static kiss_obj* kiss_read_array(const kiss_obj* const in) {
     wchar_t wcs[100];
     wchar_t i = 0;
     kiss_obj* p = kiss_c_read_char(in, KISS_NIL, KISS_EOS);
     wchar_t c = Kiss_Character(p);
     while (iswdigit(c)) {
	  wcs[i++] = c;
	  c = Kiss_Character(kiss_c_read_char(in, KISS_NIL, KISS_EOS));
     }
     wcs[i] = L'\0';

     if (c != L'a' && c !=L'A') {
	  Kiss_Err(L"Invalid array designator");
     }
     c = Kiss_Character(kiss_c_read_char(in, KISS_NIL, KISS_EOS));
     if (c != L'(') {
	  Kiss_Err(L"Invalid array designator");
     }
     size_t rank = wcstol(wcs, NULL, 10);
     kiss_obj* list = kiss_read_list(in);

     if (rank == 1) {
	  return kiss_vector(list);
     }
     kiss_obj* dimensions = kiss_list_to_array_dimensions(rank, list);
     kiss_obj* array = kiss_create_array(dimensions, KISS_NIL);

     kiss_fill_array(rank, list, Kiss_General_Vector(Kiss_General_Array_S(array)->vector));
     return array;
}

static kiss_obj* kiss_skip_comment(const kiss_obj* const in) {
     while (1) {
          kiss_obj* p = kiss_c_read_char(in, KISS_NIL, KISS_EOS);
          if (p == KISS_EOS) { Kiss_Err(L"missing |# comment closer"); }
          wchar_t c = kiss_wchar(p);
          switch (c) {
          case L'#': {
               p = kiss_c_read_char(in, KISS_NIL, KISS_EOS);
               if (p == KISS_EOS) { Kiss_Err(L"missing |# comment closer"); }
               c = kiss_wchar(p);
               if (c == L'|') {
                    kiss_skip_comment(in); // comments can nest
               }
               break;
          }
          case L'|':
               p = kiss_c_read_char(in, KISS_NIL, KISS_EOS);
               if (p == KISS_EOS) { Kiss_Err(L"missing |# comment closer"); }
               c = kiss_wchar(p);
               if (c == L'#') {
                    return NULL;
               }
               break;
          }
     }
     return KISS_NIL;
}

static kiss_obj* kiss_read_sharp_reader_macro(const kiss_obj* const in) {
     kiss_obj* p = kiss_c_preview_char(in, KISS_NIL, KISS_EOS);
     if (p == KISS_EOS) { Kiss_Err(L"missing # macro reader character"); }
     wchar_t c = kiss_wchar(p);
     switch (c) {
     case L'\'': /* #'f */
	  kiss_c_read_char(in, KISS_NIL, KISS_EOS);
	  return kiss_c_list(2, (kiss_obj*)&KISS_Sfunction, kiss_c_read(in, KISS_T, KISS_NIL));
     case L'\\': /* #\c */
	  kiss_c_read_char(in, KISS_NIL, KISS_EOS);
	  return kiss_read_sharp_reader_macro_char(in);
     case L'(': /* #() */{
	  kiss_c_read_char(in, KISS_NIL, KISS_EOS);
	  return kiss_vector(kiss_read_list(in));
     }
     case L'|': {
	  kiss_c_read_char(in, KISS_NIL, KISS_EOS);
          kiss_skip_comment(in);
          return NULL;
     }
     case L'1': case L'2': case L'3': case L'4': 
     case L'5': case L'6': case L'7': case L'8': case L'9':
	  return kiss_read_array(in);
     case L'b': case L'B': case L'o': case L'O': case L'x': case L'X': {
          kiss_push_lexeme_char(kiss_make_character(L'#'));
          kiss_push_lexeme_char(kiss_c_read_char(in, KISS_NIL, KISS_EOS));
          int escaped;
          kiss_collect_lexeme_chars(in, &escaped);
          kiss_environment_t* env = Kiss_Get_Environment();
          kiss_string_t* const str = kiss_chars_to_str(kiss_reverse(env->lexeme_chars));
          env->lexeme_chars = KISS_NIL;
          if (escaped) { Kiss_Cannot_Parse_Number_Error((kiss_obj*)str); }
          return kiss_parse_number((kiss_obj*) str);
     }
     default:
	  Kiss_Err(L"Illegal # macro reader character ~S", p);
     }
}

static kiss_obj* kiss_read_comma_at(const kiss_obj* const in) {
     kiss_environment_t* const env = Kiss_Get_Environment();
     if (env->dynamic_env.backquote_nest == 0) Kiss_Err(L"Out of place ,@");
     env->dynamic_env.backquote_nest--;
     kiss_obj* p = kiss_c_read(in, KISS_NIL, KISS_EOS);
     if (p == KISS_EOS) { Kiss_Err(L"Missing form after comma-at ,@"); }
     env->dynamic_env.backquote_nest++;
     return kiss_c_list(2, KISS_COMMA_AT, p);
}

static kiss_obj* kiss_read_comma(const kiss_obj* const in) {
     kiss_environment_t* const env = Kiss_Get_Environment();
     if (env->dynamic_env.backquote_nest == 0) Kiss_Err(L"Out of place ,");
     env->dynamic_env.backquote_nest--;
     kiss_obj* p = kiss_c_read(in, KISS_NIL, KISS_EOS);
     if (p == KISS_EOS) { Kiss_Err(L"Missing form after comma ,"); }
     env->dynamic_env.backquote_nest++;
     return kiss_c_list(2, KISS_COMMA, p);
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
    if (!KISS_IS_CONS(p))             /* `ATOM = 'ATOM */
         return kiss_c_list(2, (kiss_obj*)&KISS_Squote, p);
    if (KISS_CAR(p) == KISS_COMMA)    /* `,FORM = FORM */
	return kiss_cadr(p);
    if (KISS_CAR(p) == KISS_COMMA_AT) /* `,@FORM => error */
	Kiss_Err(L"Unquote-splicing(,@) out of list");
    kiss_push((kiss_obj*)&KISS_Sappend_s, &stack);
    while (KISS_IS_CONS(p)) { /* `(FORM ...) */
	kiss_obj* x = KISS_CAR(p);
	if (KISS_IS_CONS(x)) {
	    if (KISS_CAR(x) == KISS_COMMA)
                 kiss_push(kiss_c_list(2, (kiss_obj*)&KISS_Slist, kiss_cadr(x)), &stack);
	    else if (KISS_CAR(x) == KISS_COMMA_AT)
		kiss_push(kiss_cadr(x), &stack);
	    else kiss_push(kiss_c_list(2, ((kiss_obj*)(&KISS_Slist)),
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
                 kiss_push(kiss_c_list(2, ((kiss_obj*)(&KISS_Slist)), kiss_c_list(2, (kiss_obj*)&KISS_Squote, x)),
			  &stack);
	    }

	}
	p = KISS_CDR(p);
    }
    if (p != KISS_NIL) { kiss_push(kiss_expand_backquote(p), &stack); }
    return kiss_nreverse(stack);
}

static kiss_obj* kiss_read_backquote(const kiss_obj* const in) {
    kiss_environment_t* env = Kiss_Get_Environment();
    env->dynamic_env.backquote_nest++;
    kiss_obj* p = kiss_c_read(in, KISS_NIL, KISS_EOS);
    if (p == KISS_EOS) { Kiss_Err(L"Missing form after backquote `"); }
    env->dynamic_env.backquote_nest--;
    return kiss_expand_backquote(p);
}

static kiss_obj* kiss_read_lexeme(const kiss_obj* const in) {
     kiss_environment_t* const env = Kiss_Get_Environment();
     env->lexeme_chars = KISS_NIL;
     while(1) {
          kiss_obj* p = kiss_c_preview_char(in, KISS_NIL, KISS_NIL);
          if (p == KISS_NIL) { return NULL; }
          wchar_t c = kiss_wchar(p);
          if (iswspace(c) || iswcntrl(c)) {
               kiss_c_read_char(in, KISS_NIL, KISS_NIL);
               continue;
          }

          switch (c) {
          case L'(': {
               kiss_c_read_char(in, KISS_NIL, KISS_NIL);
               return kiss_read_list(in);
          }
          case L')': {
               kiss_c_read_char(in, KISS_NIL, KISS_NIL);
               return KISS_RPAREN;
          }
          case L'`': {
               kiss_c_read_char(in, KISS_NIL, KISS_NIL);
               return kiss_read_backquote(in);
          }
          case L',': {
               kiss_c_read_char(in, KISS_NIL, KISS_NIL);
               p = kiss_c_preview_char(in, KISS_NIL, KISS_NIL);
               if (p == KISS_NIL) { Kiss_Err(L"Stray unquote ,"); }
               c = kiss_wchar(p);
               if (c == L'@') {
                    kiss_c_read_char(in, KISS_NIL, KISS_NIL);
                    return kiss_read_comma_at(in);
               } else {
                    return kiss_read_comma(in);
               }
          }
          case L'\'': {
               kiss_c_read_char(in, KISS_NIL, KISS_NIL);
               p = kiss_c_read(in, KISS_NIL, KISS_EOS);
               if (p == KISS_EOS) { Kiss_Err(L"Stray quote '"); }
               return kiss_c_list(2, (kiss_obj*)&KISS_Squote, p);
          }
          case L';': {
               do {
                    p = kiss_c_read_char(in, KISS_NIL, KISS_NIL);
               } while (p != KISS_NIL && kiss_wchar(p) != L'\n');
               if (p == KISS_NIL) return NULL;
               break;
          }
          case L'"': {
               kiss_c_read_char(in, KISS_NIL, KISS_NIL);
               return kiss_read_string(in);
          }
          case L'#': {
               kiss_c_read_char(in, KISS_NIL, KISS_NIL);
               kiss_obj* x = kiss_read_sharp_reader_macro(in);
               if (x == NULL) {
                    break; // skipped comments
               } else {
                    return x;
               }
          }
          default:
               return kiss_read_lexeme_chars(in);
          }
     }
}

kiss_obj* kiss_c_read(const kiss_obj* const in, const kiss_obj* const eos_err_p, const kiss_obj* const eos_val) {
     kiss_obj* x = kiss_read_lexeme(in);
     if (x == NULL) { // end of stream
          if (eos_err_p != KISS_NIL) {
               Kiss_End_Of_Stream_Error(in); // _Noreturn
          } else {
               return (kiss_obj*)eos_val;
          }
     } else if (x == KISS_RPAREN) {
          Kiss_Err(L"Read error. Illegal right parenthesis: ~S", in); // _Noreturn
     } else if (x == KISS_DOT) {
          Kiss_Err(L"Read error. Illegal consing dot: ~S", in); // _Noreturn
     } else {
          if (KISS_IS_SYMBOL(x)) { assert(kiss_is_interned((kiss_symbol_t*)x)); }
          return x;
     }
}

// function: (read [input-stream [eos-error-p [eos-value]]]) -> <object> 
// https://nenbutsu.github.io/ISLispHyperDraft/islisp-v23.html#argument_conventions
// https://nenbutsu.github.io/ISLispHyperDraft/islisp-v23.html#f_read  
kiss_obj* kiss_read(const kiss_obj* args) {
     kiss_obj* in = kiss_standard_input();
     kiss_obj* eos_err_p = KISS_T;
     kiss_obj* eos_val = KISS_NIL;
     if (KISS_IS_CONS(args)) {
          in = (kiss_obj*)Kiss_Input_Char_Stream(KISS_CAR(args));
          args = KISS_CDR(args);
          if (KISS_IS_CONS(args)) {
               eos_err_p = KISS_CAR(args);
               args = KISS_CDR(args);
               if (KISS_IS_CONS(args)) {
                    eos_val = KISS_CAR(args);
               }
          }
     }
     return kiss_c_read(in, eos_err_p, eos_val);
}
