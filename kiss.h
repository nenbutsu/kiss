/*
  kiss.h --- declarations of ISLisp processor KISS.

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
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>

typedef enum {
     KISS_CONS,
     KISS_SYMBOL,
     KISS_CHARACTER,
     KISS_INTEGER,
     KISS_FLOAT,
     KISS_STRING,
     KISS_GENERAL_VECTOR,
     KISS_GENERAL_ARRAY,
     KISS_STREAM,

     KISS_FUNCTION,
     KISS_MACRO,
     KISS_CFUNCTION,
     KISS_CMACRO,

     KISS_CATCHER,
     KISS_CLEANUP,
     KISS_BLOCK,
     KISS_TAGBODY,

     KISS_OO_OBJ,
} kiss_type;

typedef struct {
     kiss_type type;
     void* pointer[];
} kiss_obj;

struct kiss_gc_obj {
     kiss_type type;
     int gc_flag;
     struct kiss_gc_obj* gc_next;
};

typedef struct kiss_gc_obj kiss_gc_obj;

typedef kiss_obj* (*kiss_cf0_t)(void);
typedef kiss_obj* (*kiss_cf1_t)(kiss_obj*);
typedef kiss_obj* (*kiss_cf2_t)(kiss_obj*, kiss_obj*);
typedef kiss_obj* (*kiss_cf3_t)(kiss_obj*, kiss_obj*, kiss_obj*);
typedef kiss_obj* (*kiss_cf4_t)(kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*);
typedef kiss_obj* (*kiss_cf5_t)(kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*, 
				kiss_obj*);
typedef kiss_obj* (*kiss_cf6_t)(kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*,
				kiss_obj*, kiss_obj*);
typedef kiss_obj* (*kiss_cf7_t)(kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*,
				kiss_obj*, kiss_obj*, kiss_obj*);
typedef kiss_obj* (*kiss_cf8_t)(kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*,
				kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*);
typedef kiss_obj* (*kiss_cf9_t)(kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*,
				kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*,
				kiss_obj*);
typedef kiss_obj* (*kiss_cf10_t)(kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*,
				 kiss_obj*, kiss_obj*, kiss_obj*, kiss_obj*,
				 kiss_obj*, kiss_obj*);

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_obj* car;
     kiss_obj* cdr;
} kiss_cons_t;

typedef enum {
    KISS_CONSTANT_VAR = 1,
    KISS_CONSTANT_FUN = 2,
} kiss_symbol_flags;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     wchar_t* name;
     kiss_symbol_flags flags;
     kiss_obj* var;
     kiss_obj* fun;
     kiss_obj* plist;
} kiss_symbol_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     wchar_t c;
} kiss_character_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     long int i;
} kiss_integer_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     float f;
} kiss_float_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     wchar_t* str;
     size_t n;
} kiss_string_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_obj** v;
     size_t n;
} kiss_general_vector_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_obj* vector;
     size_t rank;
} kiss_general_array_t;

typedef struct {
     kiss_type type;
     kiss_symbol_t* name;
     void* fun;
     int min_args;
     int max_args;
} kiss_cfunction_t;

typedef struct {
     kiss_obj* vars;
     kiss_obj* funs;
     kiss_obj* jumpers;
} kiss_lexical_environment_t;

typedef struct {
     kiss_obj* vars;
     kiss_obj* jumpers;
     size_t backquote_nest;
} kiss_dynamic_environment_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_symbol_t* name;
     kiss_obj* lambda;
     kiss_lexical_environment_t lexical_env;
} kiss_function_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_obj* tag;
     void*     jmp;
     kiss_dynamic_environment_t dynamic_env;
} kiss_catcher_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_symbol_t* name;
     void*          jmp;
     kiss_dynamic_environment_t dynamic_env;
} kiss_block_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_obj* body;
     kiss_lexical_environment_t lexical_env;
     kiss_dynamic_environment_t dynamic_env;
} kiss_cleanup_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_symbol_t* tag;
     void* jmp;
     kiss_dynamic_environment_t dynamic_env;
     kiss_obj* body;
} kiss_tagbody_t;


typedef enum {
    KISS_INPUT_STREAM     = 1,
    KISS_OUTPUT_STREAM    = 2,
    KISS_CHARACTER_STREAM = 4,
    KISS_BYTE_STREAM      = 8,
    KISS_STRING_STREAM    = 16,
    KISS_FILE_STREAM      = 32,
} kiss_stream_flags;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_stream_flags flags;
     size_t column;
} kiss_stream_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_stream_flags flags;
     size_t column;
     FILE* file_ptr;
} kiss_file_stream_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_stream_flags flags;
     size_t column;
     kiss_obj* list;
} kiss_string_stream_t;

typedef struct {
     kiss_type type;
     int gc_flag;
     kiss_gc_obj* gc_next;
     kiss_obj* plist;
} kiss_oo_obj_t;


typedef struct {
     kiss_lexical_environment_t lexical_env;
     kiss_dynamic_environment_t dynamic_env;
     size_t gensym_number;
     kiss_obj* lexeme_chars;
     kiss_obj* throw_result;
     kiss_obj* block_result;
     size_t heap_index;
     int gc_flag;
     kiss_tagbody_t* current_tagbody;
     void* top_level;
     kiss_obj* global_dynamic_vars;
     kiss_obj* features;
} kiss_environment_t;

kiss_symbol_t KISS_St, KISS_Snil, KISS_Squote, KISS_Slambda,
    KISS_Skw_rest, KISS_Samp_rest, KISS_Ueos;
#define KISS_T        ((kiss_obj*)(&KISS_St))
#define KISS_NIL      ((kiss_obj*)(&KISS_Snil))
#define KISS_QUOTE    ((kiss_obj*)(&KISS_Squote))
#define KISS_LAMBDA   ((kiss_obj*)(&KISS_Slambda))
#define KISS_AMP_REST ((kiss_obj*)(&KISS_Samp_rest))
#define KISS_KW_REST  ((kiss_obj*)(&KISS_Skw_rest))
#define KISS_EOS      ((kiss_obj*)(&KISS_Ueos))

#define KISS_CAR(x) ((void*)(((kiss_cons_t*)x)->car))
#define KISS_CDR(x) ((void*)(((kiss_cons_t*)x)->cdr))
#define KISS_CDDR(x)   KISS_CDR(KISS_CDR(x))
#define KISS_CADR(x)   KISS_CAR(KISS_CDR(x))
#define KISS_CADDR(x)  KISS_CAR(KISS_CDR(KISS_CDR(x)))

#define KISS_OBJ_TYPE(x) (((kiss_obj*)x)->type)

#define KISS_IS_CONS(x)              (KISS_OBJ_TYPE(x) == KISS_CONS)
#define KISS_IS_LIST(x)              (KISS_IS_CONS(x) || (((kiss_obj*)x) == KISS_NIL))
#define KISS_IS_SYMBOL(x)            (KISS_OBJ_TYPE(x) == KISS_SYMBOL)
#define KISS_IS_INTEGER(x)           (KISS_OBJ_TYPE(x) == KISS_INTEGER)
#define KISS_IS_FLOAT(x)             (KISS_OBJ_TYPE(x) == KISS_FLOAT)
#define KISS_IS_CHARACTER(x)         (KISS_OBJ_TYPE(x) == KISS_CHARACTER)
#define KISS_IS_STRING(x)            (KISS_OBJ_TYPE(x) == KISS_STRING)
#define KISS_IS_GENERAL_VECTOR(x)    (KISS_OBJ_TYPE(x) == KISS_GENERAL_VECTOR)
#define KISS_IS_GENERAL_ARRAY(x)     (KISS_OBJ_TYPE(x) == KISS_GENERAL_ARRAY)
#define KISS_IS_SEQUENCE(x)          (KISS_IS_LIST(x) || KISS_IS_STRING(x) || KISS_IS_GENERAL_VECTOR(x))
#define KISS_IS_FUNCTION(x)          (KISS_OBJ_TYPE(x) == KISS_FUNCTION)
#define KISS_IS_MACRO(x)             (KISS_OBJ_TYPE(x) == KISS_MACRO)
#define KISS_IS_CFUNCTION(x)         (KISS_OBJ_TYPE(x) == KISS_CFUNCTION)
#define KISS_IS_CMACRO(x)            (KISS_OBJ_TYPE(x) == KISS_CMACRO)
#define KISS_IS_CATCHER(x)           (KISS_OBJ_TYPE(x) == KISS_CATCHER)
#define KISS_IS_CLEANUP(x)           (KISS_OBJ_TYPE(x) == KISS_CLEANUP)
#define KISS_IS_BLOCK(x)             (KISS_OBJ_TYPE(x) == KISS_BLOCK)
#define KISS_IS_TAGBODY(x)           (KISS_OBJ_TYPE(x) == KISS_TAGBODY)
#define KISS_IS_OBJECT(x)            (KISS_OBJ_TYPE(x) == KISS_OO_OBJ)
#define KISS_IS_STREAM(x)            (KISS_OBJ_TYPE(x) == KISS_STREAM)
#define KISS_IS_INPUT_STREAM(x)     (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_INPUT_STREAM))
#define KISS_IS_OUTPUT_STREAM(x)    (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_OUTPUT_STREAM))
#define KISS_IS_CHARACTER_STREAM(x) (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_CHARACTER_STREAM))
#define KISS_IS_FILE_STREAM(x)      (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_FILE_STREAM))
#define KISS_IS_STRING_STREAM(x)    (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_STRING_STREAM))

#define KISS_IS_GC_OBJ(x)           (KISS_IS_CONS(x) || KISS_IS_SYMBOL(x) || KISS_IS_INTEGER(x) || KISS_IS_FLOAT(x) || KISS_IS_CHARACTER(x) || KISS_IS_STRING(x) || KISS_IS_GENERAL_VECTOR(x) || KISS_IS_FUNCTION(x) || KISS_IS_MACRO(x) || KISS_IS_CATCHER(x) || KISS_IS_CLEANUP(x) || KISS_IS_BLOCK(x) || KISS_IS_TAGBODY(x) || KISS_IS_OBJECT(x) || KISS_IS_STREAM(x))


/* character.c */
kiss_character_t* kiss_make_character(wchar_t c);
kiss_obj* kiss_characterp (kiss_obj* obj);
kiss_obj* kiss_char_eq(kiss_obj* character1, kiss_obj* character2);
kiss_obj* kiss_char_lessthan(kiss_obj* character1, kiss_obj* character2);

/* cinvoke.c */
kiss_obj* kiss_cinvoke(kiss_cfunction_t* cfun, kiss_obj* args);

/* cons.c */
kiss_obj* kiss_cons(kiss_obj* car, kiss_obj* cdr);
kiss_obj* kiss_consp(kiss_obj* obj);
kiss_obj* kiss_car(kiss_obj* p);
kiss_obj* kiss_cdr(kiss_obj* p);
kiss_obj* kiss_cadr(kiss_obj* p);
kiss_obj* kiss_cddr(kiss_obj* p);
kiss_obj* kiss_caddr(kiss_obj* p);
kiss_obj* kiss_set_car(kiss_obj* obj, kiss_obj* cons);
kiss_obj* kiss_set_cdr(kiss_obj* obj, kiss_obj* cons);
kiss_obj* kiss_list(kiss_obj* p);
kiss_obj* kiss_c_list(int nargs, ...);
kiss_obj* kiss_append(kiss_obj* p);
kiss_obj* kiss_append_s(kiss_obj* p);
kiss_obj* kiss_cappend(int nargs, ...);
kiss_obj* kiss_reverse(kiss_obj* p);
kiss_obj* kiss_nreverse(kiss_obj* p);
kiss_obj* kiss_member(kiss_obj* obj, kiss_obj* list);
kiss_obj* kiss_assoc(kiss_obj* obj, kiss_obj* alist);
kiss_obj* kiss_cmapcar(kiss_cf1_t f, kiss_obj* list);
kiss_obj* kiss_cmapc(kiss_cf1_t f, kiss_obj* list);
kiss_obj* kiss_plist_member (kiss_obj* plist, kiss_obj* property);
kiss_obj* kiss_plist_remove(kiss_obj* plist, kiss_obj* property);
kiss_obj* kiss_plist_get (kiss_obj* plist, kiss_obj* property);
kiss_obj* kiss_plist_put (kiss_obj* plist, kiss_obj* property, kiss_obj* value);
kiss_obj* kiss_copy_list(kiss_obj* p);
void kiss_push(kiss_obj* elm, kiss_obj** list);

/* control.c */
kiss_obj* kiss_quote(kiss_obj* obj);
kiss_obj* kiss_if(kiss_obj* test_form, kiss_obj* then_form, kiss_obj* rest);
kiss_obj* kiss_eq(kiss_obj* obj1, kiss_obj* obj2);
kiss_obj* kiss_eql(kiss_obj* obj1, kiss_obj* obj2);
kiss_obj* kiss_progn(kiss_obj* body);
kiss_catcher_t* kiss_make_catcher(kiss_obj* tag, jmp_buf jmp);
kiss_obj* kiss_catch(kiss_obj* tag_form, kiss_obj* body);
kiss_obj* kiss_throw(kiss_obj* tag_form, kiss_obj* result_form);
kiss_obj* kiss_unwind_protect(kiss_obj* protected_form, kiss_obj* cleanup_body);
kiss_obj* kiss_block(kiss_obj* name, kiss_obj* body);
kiss_obj* kiss_return_from(kiss_obj* name, kiss_obj* result_form);
kiss_obj* kiss_tagbody(kiss_obj* args);
kiss_obj* kiss_go(kiss_obj* tag);

/* error.c */
void Kiss_System_Error (void);
void Kiss_Err(wchar_t* str, ...);
kiss_obj* kiss_error(kiss_obj* error_string, kiss_obj* rest);
void Kiss_Cannot_Chage_Constant_Error(kiss_obj* obj);
void Kiss_End_Of_Stream_Error(kiss_obj* stream);
void Kiss_Cannot_Parse_Number_Error(kiss_obj* str);
kiss_stream_t* Kiss_Input_Char_Stream(kiss_obj* obj);
kiss_stream_t* Kiss_Output_Char_Stream(kiss_obj* obj);
void Kiss_Unbound_Variable_Error(kiss_obj* obj);
void Kiss_Catcher_Not_Found_Error(kiss_obj* tag);
void Kiss_Block_Not_Found_Error(kiss_obj* name);
void Kiss_Tagbody_Not_Found_Error(kiss_obj* name);
void Kiss_Index_Out_Of_Range_Error(kiss_obj* sequence, kiss_obj* index);
kiss_obj* Kiss_Valid_Sequence_Index(kiss_obj* sequence, kiss_obj* index);
kiss_cons_t* Kiss_Cons(kiss_obj* obj);
kiss_obj* Kiss_List(kiss_obj* obj);
kiss_stream_t* Kiss_Stream(kiss_obj* obj);
kiss_obj* Kiss_Proper_List(kiss_obj* obj);
kiss_cons_t* Kiss_Proper_List_2(kiss_obj* obj);
kiss_integer_t* Kiss_Integer(kiss_obj* obj);
kiss_integer_t* Kiss_Non_Negative_Integer(kiss_obj* obj);
kiss_integer_t* Kiss_Non_Zero_Integer(kiss_obj* obj);
kiss_float_t* Kiss_Float(kiss_obj* obj);
kiss_obj* Kiss_Number(kiss_obj* obj);
kiss_character_t* Kiss_Character(kiss_obj* obj);
kiss_symbol_t* Kiss_Symbol(kiss_obj* obj);
kiss_string_t* Kiss_String(kiss_obj* obj);
kiss_general_vector_t* Kiss_General_Vector(kiss_obj* obj);
kiss_general_array_t* Kiss_General_Array_S(kiss_obj* obj);
kiss_obj* Kiss_General_Array(kiss_obj* obj);
kiss_obj* Kiss_Basic_Array(kiss_obj* obj);
kiss_obj* Kiss_Sequence(kiss_obj* obj);
kiss_function_t* Kiss_Function(kiss_obj* obj);
kiss_function_t* Kiss_Macro(kiss_obj* obj);
kiss_cfunction_t* Kiss_CFunction(kiss_obj* obj);
kiss_cfunction_t* Kiss_CMacro(kiss_obj* obj);
kiss_oo_obj_t* Kiss_Object(kiss_obj* obj);
kiss_obj* Kiss_Lambda_List(kiss_obj* list);
kiss_obj* Kiss_Lambda_Expression(kiss_obj* p);
kiss_string_stream_t* Kiss_String_Output_Stream(kiss_obj* obj);

/* eval.c */
kiss_obj* kiss_eval(kiss_obj* form);
kiss_obj* kiss_load(kiss_obj* filename);
kiss_obj* kiss_eval_body(kiss_obj* body);

/* format.c */
kiss_obj* kiss_format(kiss_obj* out, kiss_obj* format, kiss_obj* args);
kiss_obj* kiss_format_integer(kiss_obj* out, kiss_obj* obj, kiss_obj* radix);
kiss_obj* kiss_format_float(kiss_obj* out, kiss_obj* obj) ;
kiss_obj* kiss_format_pointer(kiss_obj* out, kiss_obj* obj);
kiss_obj* kiss_format_object(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep);
kiss_obj* kiss_print(kiss_obj* obj);

/* function.c */
kiss_function_t* kiss_make_function(kiss_symbol_t* name, kiss_obj* lambda);
kiss_obj* kiss_simple_function_p(kiss_obj* obj);
kiss_obj* kiss_linvoke(kiss_function_t* fun, kiss_obj* args);
kiss_obj* kiss_lambda(kiss_obj* params, kiss_obj* body);
kiss_obj* kiss_defun(kiss_obj* name, kiss_obj* params, kiss_obj* body);
kiss_obj* kiss_defmacro(kiss_obj* name, kiss_obj* params, kiss_obj* body);
kiss_obj* kiss_fun_ref(kiss_symbol_t* name);
kiss_obj* kiss_function(kiss_obj* name);
kiss_obj* kiss_funcall(kiss_obj* f, kiss_obj* args);
kiss_obj* kiss_cfuncall(wchar_t* function_name, kiss_obj* args);
kiss_obj* kiss_apply(kiss_obj* f, kiss_obj* obj, kiss_obj* rest);
kiss_obj* kiss_flet(kiss_obj* fspecs, kiss_obj* body);
kiss_obj* kiss_labels(kiss_obj* fspecs, kiss_obj* body);
void kiss_bind_funargs(kiss_obj* params, kiss_obj* args);

/* vector.c */
kiss_general_vector_t* kiss_make_general_vector(size_t n, kiss_obj* obj);
kiss_obj* kiss_create_general_vector(kiss_obj* i, kiss_obj* rest);
kiss_obj* kiss_vector(kiss_obj* objs);
kiss_obj* kiss_general_vector_p(kiss_obj* obj);
kiss_obj* kiss_gvref(kiss_obj* general_vector, kiss_obj* index);
kiss_obj* kiss_set_gvref(kiss_obj* obj, kiss_obj* general_vector, kiss_obj* index);

/* array.c */
kiss_obj* kiss_create_array(kiss_obj* dimensions, kiss_obj* rest);
kiss_obj* kiss_aref(kiss_obj* array, kiss_obj* rest);
kiss_obj* kiss_set_aref(kiss_obj* obj, kiss_obj* array, kiss_obj* rest);
kiss_obj* kiss_garef(kiss_obj* array, kiss_obj* rest);
kiss_obj* kiss_set_garef(kiss_obj* obj, kiss_obj* array, kiss_obj* rest);
kiss_obj* kiss_general_array_s_to_list (kiss_obj* obj);
kiss_obj* kiss_array_dimensions(kiss_obj* array);
kiss_obj* kiss_general_array_s_to_list (kiss_obj* obj);
kiss_obj* kiss_basic_array_p (kiss_obj* obj);
kiss_obj* kiss_basic_array_s_p (kiss_obj* obj);
kiss_obj* kiss_general_array_s_p (kiss_obj* obj);

/* environment.c */
kiss_environment_t* Kiss_Get_Environment(void);

/* init.c */
void kiss_initialize(void);

/* number.c */
kiss_integer_t* kiss_make_integer(long int i);
kiss_obj* kiss_integerp(kiss_obj* obj);
kiss_obj* kiss_Lplus(kiss_obj* p);
kiss_obj* kiss_Lmultiply(kiss_obj* p);
kiss_obj* kiss_Lminus(kiss_obj* number, kiss_obj* rest);
kiss_obj* kiss_Lnum_eq(kiss_obj* x, kiss_obj* y);
kiss_obj* kiss_Lnum_lessthan(kiss_obj* x, kiss_obj* y);

kiss_float_t* kiss_make_float(float f);
kiss_obj* kiss_floatp(kiss_obj* obj);
kiss_obj* kiss_parse_number(kiss_obj* str);
kiss_obj* kiss_float(kiss_obj* x);

kiss_obj* kiss_abs(kiss_obj* x);
kiss_obj* kiss_exp(kiss_obj* x);
kiss_obj* kiss_log(kiss_obj* x);
kiss_obj* kiss_floor(kiss_obj* x);
kiss_obj* kiss_ceiling(kiss_obj* x);
kiss_obj* kiss_truncate(kiss_obj* x);
kiss_obj* kiss_round(kiss_obj* x);

kiss_obj* kiss_div(kiss_obj* z1, kiss_obj* z2);
kiss_obj* kiss_mod(kiss_obj* z1, kiss_obj* z2);
kiss_obj* kiss_gcd(kiss_obj* z1, kiss_obj* z2);
kiss_obj* kiss_lcm(kiss_obj* z1, kiss_obj* z2);

kiss_obj* kiss_sin(kiss_obj* x);
kiss_obj* kiss_cos(kiss_obj* x);
kiss_obj* kiss_tan(kiss_obj* x);

/* malloc.c */
void* Kiss_Malloc(size_t size);
void* Kiss_Malloc_Atomic(size_t size);

/* read.c */
kiss_obj* kiss_cread(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val);
kiss_obj* kiss_read(kiss_obj* args);

/* repl.c */
int kiss_read_eval_print_loop(void);

/* sequence.c */
size_t kiss_clength(kiss_obj* p);
kiss_obj* kiss_length(kiss_obj* sequence);
kiss_obj* kiss_elt(kiss_obj* sequence, kiss_obj* z);
kiss_obj* kiss_set_elt(kiss_obj* obj, kiss_obj* sequence, kiss_obj* z);
kiss_obj* kiss_subseq(kiss_obj* sequence, kiss_obj* z1, kiss_obj* z2);
kiss_obj* kiss_map_into(kiss_obj* destination, kiss_obj* function, kiss_obj* rest);

/* stream.c */
void kiss_init_streams(void);
kiss_obj* kiss_streamp(kiss_obj* obj);
kiss_obj* kiss_open_stream_p(kiss_obj* obj);
kiss_obj* kiss_create_string_input_stream(kiss_obj* string);
kiss_obj* kiss_create_string_output_stream(void);
kiss_obj* kiss_get_output_stream_string(kiss_obj* stream);
kiss_obj* kiss_standard_input(void);
kiss_obj* kiss_standard_output(void);
kiss_obj* kiss_error_output(void);
kiss_obj* kiss_input_stream_p(kiss_obj* p);
kiss_obj* kiss_output_stream_p(kiss_obj* p);
kiss_obj* kiss_char_stream_p(kiss_obj* p);
kiss_obj* kiss_input_char_stream_p(kiss_obj* p);
kiss_obj* kiss_output_char_stream_p(kiss_obj* p);
kiss_obj* kiss_c_read_char(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val);
kiss_obj* kiss_c_preview_char(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val);
kiss_obj* kiss_read_char(kiss_obj* args);
kiss_obj* kiss_preview_char(kiss_obj* args);
kiss_obj* kiss_c_read_line(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val);
kiss_obj* kiss_read_line(kiss_obj* args);
kiss_obj* kiss_format_char(kiss_obj* out, kiss_obj* obj);
kiss_obj* kiss_open_input_file(kiss_obj* filename, kiss_obj* rest);
kiss_obj* kiss_open_output_file(kiss_obj* filename, kiss_obj* rest);
kiss_obj* kiss_open_io_file(kiss_obj* filename, kiss_obj* rest);
kiss_obj* kiss_close(kiss_obj* obj);
kiss_obj* kiss_finish_output (kiss_obj* obj);

/* string.c */
kiss_string_t* kiss_make_string(wchar_t* s);
kiss_obj* kiss_create_string(kiss_obj* i, kiss_obj* rest);
kiss_obj* kiss_stringp(kiss_obj* obj);
kiss_string_t* kiss_chars_to_str(kiss_obj* chars);
kiss_obj* kiss_str_to_chars(kiss_string_t* str);
kiss_obj* kiss_string_append(kiss_obj* rest);

/* symbols.c */
extern size_t Kiss_Symbol_Number;
extern kiss_symbol_t* Kiss_Symbols[];
void kiss_init_symbols(void);
kiss_obj* kiss_symbolp(kiss_obj* obj);
kiss_obj* kiss_gensym(void);
kiss_obj* kiss_symbol_function (kiss_obj* obj);
kiss_obj* kiss_set_symbol_function (kiss_obj* sym, kiss_obj* definition);
kiss_obj* kiss_fboundp (kiss_obj* obj);
kiss_obj* kiss_fmakunbound (kiss_obj* obj);
int kiss_is_interned(kiss_symbol_t* p);
kiss_obj* kiss_symbol(wchar_t* name);
kiss_obj* kiss_intern(kiss_obj* name);
kiss_obj* kiss_property(kiss_obj* symbol, kiss_obj* property, kiss_obj* rest);
kiss_obj* kiss_set_property(kiss_obj* obj, kiss_obj* symbol, kiss_obj* property);
kiss_obj* kiss_remove_property(kiss_obj* symbol, kiss_obj* property);
extern kiss_symbol_t KISS_Sblock;
extern kiss_symbol_t KISS_Serror;

/* variable.c */
kiss_obj* kiss_var_ref(kiss_symbol_t* name);
kiss_obj* kiss_setq(kiss_obj* name, kiss_obj* form);
kiss_obj* kiss_defglobal(kiss_obj* name, kiss_obj* form);
kiss_obj* kiss_defconstant(kiss_obj* name, kiss_obj* form);
kiss_obj* kiss_let(kiss_obj* vspecs, kiss_obj* body);
kiss_obj* kiss_let_s(kiss_obj* vspecs, kiss_obj* body);
kiss_obj* kiss_defdynamic(kiss_obj* name, kiss_obj* form);
kiss_obj* kiss_dynamic(kiss_obj* name);
kiss_obj* kiss_dynamic_let(kiss_obj* vspecs, kiss_obj* body);
kiss_obj* kiss_set_dynamic(kiss_obj* form, kiss_obj* var);

/* eval.c */
kiss_obj* kiss_eval(kiss_obj* form);

/* object.c */
kiss_obj* kiss_object_p(kiss_obj* obj);
kiss_obj* kiss_make_object(kiss_obj* info);
kiss_obj* kiss_object_plist(kiss_obj* obj);
kiss_obj* kiss_set_object_plist(kiss_obj* plist, kiss_obj* oo_obj);
kiss_obj* kiss_object_plist_get(kiss_obj* obj, kiss_obj* property);
kiss_obj* kiss_object_plist_put(kiss_obj* obj, kiss_obj* property,
				kiss_obj* value);

/* gf_invoke.c */
kiss_obj* kiss_method_invoke(kiss_obj* m);

/* class.c */
kiss_obj* kiss_type_to_class_name(kiss_type t);

/* environment.c */
void kiss_init_environment(void);

/* feature.c */
kiss_obj* kiss_featurep(kiss_obj* feature);
kiss_obj* kiss_provide(kiss_obj* feature);

/* gc.c */
void* Kiss_Malloc(size_t size);
void* Kiss_GC_Malloc(size_t size);

