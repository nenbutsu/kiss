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
#include <time.h>
#include <unistd.h>
#include <wchar.h>
#include <wctype.h>

#include <gmp.h>

typedef long int kiss_ptr_int;
#define KISS_PTR_INT_MAX (LONG_MAX>>2)
#define KISS_PTR_INT_MIN (LONG_MIN>>2)
_Static_assert (sizeof(kiss_ptr_int) == sizeof(void*), "We need a C int with the same width as void*");

#define kiss_ptr_int(x)        (((kiss_ptr_int)x)>>2)
#define kiss_wchar(x)          kiss_ptr_int(x)

#define kiss_make_char(x)      (kiss_obj*)((((kiss_ptr_int)x)<<2) | 2)
#define kiss_make_fixnum(x)    (kiss_obj*)((((kiss_ptr_int)x)<<2) | 1)

typedef enum {
     KISS_FIXNUM = 1,
     KISS_CHARACTER = 2,
     KISS_CONS,
     KISS_SYMBOL,
     KISS_BIGNUM,
     KISS_FLOAT,
     KISS_STRING,
     KISS_GENERAL_VECTOR,
     KISS_GENERAL_ARRAY_S,
     KISS_STREAM,
     KISS_HASH_TABLE,

     KISS_LFUNCTION,
     KISS_LMACRO,
     KISS_CFUNCTION,
     KISS_CMACRO,

     KISS_CATCHER,
     KISS_CLEANUP,
     KISS_BLOCK,
     KISS_TAGBODY,

     KISS_ILOS_OBJ,
} kiss_type;

typedef struct {
     kiss_type type;
     void* pointer1;
     void* pointer2;
     void* pointer3;
     void* pointer4;
     void* pointer5;
} kiss_obj;

struct kiss_gc_obj {
     kiss_type type;
     void* gc_ptr;
     void* pointer2;
     void* pointer3;
     void* pointer4;
     void* pointer5;
};
typedef struct kiss_gc_obj kiss_gc_obj;


#define KISS_HEAP_STACK_SIZE (1024 * 1024 * 2)
extern size_t Kiss_Heap_Top;
extern kiss_gc_obj* Kiss_Heap_Stack[];

extern kiss_ptr_int Kiss_GC_Flag;
extern size_t Kiss_GC_Amount;
extern void* Kiss_GC_Objects;

/* gc.c */
kiss_obj* kiss_gc_info(void);
kiss_obj* kiss_gc(void);

_Noreturn
void Kiss_System_Error (void);

#define kiss_gc_ptr(x)   ((void*)((kiss_ptr_int)x & (~0<<1)))
/* An error shall be signaled if the requested memory cannot be allocated
   (error-id. <storage-exhausted>). */
inline
void* Kiss_Malloc(size_t const size) {
    void* p = malloc(size);
    if (p == NULL) { Kiss_System_Error(); }
    return p;
}

inline
void* Kiss_GC_Malloc(size_t const size) {
    void* p = Kiss_Malloc(size);

    Kiss_GC_Amount += size;
    if (Kiss_GC_Amount > 1024 * 1024 * 4) {
         //fwprintf(stderr, L"\ngc...\n");
	 kiss_gc();
	 Kiss_GC_Amount = 0;
    }

    Kiss_Heap_Stack[Kiss_Heap_Top++] = p;
    assert(Kiss_Heap_Top < KISS_HEAP_STACK_SIZE);
    ((kiss_gc_obj*)p)->gc_ptr = (void*)((kiss_ptr_int)kiss_gc_ptr(Kiss_GC_Objects) | Kiss_GC_Flag);
    Kiss_GC_Objects = p;
    return p;
}

typedef struct {
     kiss_type type;
     void* gc_ptr;
     kiss_obj* car;
     kiss_obj* cdr;
} kiss_cons_t;

typedef enum {
    KISS_SYSTEM_CONSTANT_VAR = 1,
    KISS_USER_CONSTANT_VAR = 2,
    KISS_CONSTANT_FSLOT = 4,
    KISS_SPECIAL_OPERATOR = 8,
    KISS_DEFINING_OPERATOR = 16,
} kiss_symbol_flags;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     wchar_t* name;
     kiss_symbol_flags flags;
     kiss_obj* var;
     kiss_obj* fun;
     kiss_obj* plist;
} kiss_symbol_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     mpz_t mpz;
} kiss_bignum_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     mpf_t mpf;
} kiss_float_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     wchar_t* str;
     size_t n;
} kiss_string_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     kiss_obj** v;
     size_t n;
} kiss_general_vector_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     kiss_obj* vector;
     size_t rank;
} kiss_general_array_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     size_t n;
     kiss_general_vector_t* vector;
     kiss_obj* test;
     kiss_obj* weakness;
     kiss_obj* rehash_size;
     kiss_obj* rehash_threshold;
} kiss_hash_table_t;

extern kiss_hash_table_t* Kiss_Symbol_Hash_Table;



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

typedef union {
     kiss_cf0_t f0;
     kiss_cf1_t f1;
     kiss_cf2_t f2;
     kiss_cf3_t f3;
     kiss_cf4_t f4;
     kiss_cf5_t f5;
     kiss_cf6_t f6;
     kiss_cf7_t f7;
     kiss_cf8_t f8;
     kiss_cf9_t f9;
     kiss_cf10_t f10;
} kiss_cf_t;

typedef struct {
     kiss_type type;
     kiss_symbol_t* name;
     kiss_cf_t* fun;
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
     void* gc_ptr;
     kiss_symbol_t* name;
     kiss_obj* lambda;
     kiss_lexical_environment_t lexical_env;
} kiss_function_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     kiss_obj* tag;
     void*     jmp;
     kiss_dynamic_environment_t dynamic_env;
} kiss_catcher_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     kiss_symbol_t* name;
     void*          jmp;
     kiss_dynamic_environment_t dynamic_env;
} kiss_block_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     kiss_obj* body;
     kiss_lexical_environment_t lexical_env;
     kiss_dynamic_environment_t dynamic_env;
} kiss_cleanup_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
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
     void* gc_ptr;
     kiss_stream_flags flags;
     size_t column;
} kiss_stream_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     kiss_stream_flags flags;
     size_t column;
     kiss_obj* list;
} kiss_string_stream_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     kiss_stream_flags flags;
     size_t column;
     FILE* file_ptr;
     size_t pos;
} kiss_file_stream_t;

typedef struct {
     kiss_type type;
     void* gc_ptr;
     kiss_obj* plist;
} kiss_ilos_obj_t;


typedef struct {
     kiss_lexical_environment_t lexical_env;
     kiss_dynamic_environment_t dynamic_env;
     kiss_obj* lexeme_chars;
     kiss_obj* throw_result;
     kiss_obj* block_result;
     kiss_tagbody_t* current_tagbody;
     void* top_level;
     kiss_obj* global_dynamic_vars;
     kiss_obj* call_stack;
     kiss_obj* error_call_stack;
} kiss_environment_t;

extern kiss_obj* Kiss_Features;

kiss_symbol_t KISS_St, KISS_Snil, KISS_Squote, KISS_Slambda;
#define KISS_T        ((kiss_obj*)(&KISS_St))
#define KISS_NIL      ((kiss_obj*)(&KISS_Snil))

kiss_symbol_t KISS_Skw_rest, KISS_Samp_rest, KISS_Skw_size, KISS_Skw_test, KISS_Skw_weakness, KISS_Skw_rehash_size, KISS_Skw_rehash_threshold, KISS_Skw_name, KISS_Skw_class;

kiss_symbol_t KISS_Seql;

kiss_symbol_t KISS_Ueos, KISS_Udummy;
#define KISS_DUMMY    ((kiss_obj*)(&KISS_Udummy))
#define KISS_EOS      ((kiss_obj*)(&KISS_Ueos))


#define KISS_CAR(x) ((void*)(((kiss_cons_t*)x)->car))
#define KISS_CDR(x) ((void*)(((kiss_cons_t*)x)->cdr))
#define KISS_CDDR(x)   KISS_CDR(KISS_CDR(x))
#define KISS_CADR(x)   KISS_CAR(KISS_CDR(x))
#define KISS_CADDR(x)  KISS_CAR(KISS_CDR(KISS_CDR(x)))

#define KISS_OBJ_TYPE(x) (int)(((kiss_ptr_int)x & 3) ? ((kiss_ptr_int)x & 3) : (((kiss_obj*)x)->type))

#define KISS_IS_FIXNUM(x)            ((kiss_ptr_int)x & 1)
#define KISS_IS_FIXCHAR(x)           ((kiss_ptr_int)x & 2)
#define KISS_IS_BIGNUM(x)            (KISS_OBJ_TYPE(x) == KISS_BIGNUM)
#define KISS_IS_INTEGER(x)           (KISS_IS_FIXNUM(x) || KISS_IS_BIGNUM(x))
#define KISS_IS_CHARACTER(x)         (KISS_IS_FIXCHAR(x))
#define KISS_IS_CONS(x)              (KISS_OBJ_TYPE(x) == KISS_CONS)
#define KISS_IS_LIST(x)              (KISS_IS_CONS(x) || (((void*)x) == KISS_NIL))
#define KISS_IS_SYMBOL(x)            (KISS_OBJ_TYPE(x) == KISS_SYMBOL)
#define KISS_IS_FLOAT(x)             (KISS_OBJ_TYPE(x) == KISS_FLOAT)
#define KISS_IS_STRING(x)            (KISS_OBJ_TYPE(x) == KISS_STRING)
#define KISS_IS_GENERAL_VECTOR(x)    (KISS_OBJ_TYPE(x) == KISS_GENERAL_VECTOR)
#define KISS_IS_GENERAL_ARRAY_S(x)   (KISS_OBJ_TYPE(x) == KISS_GENERAL_ARRAY_S)
#define KISS_IS_TASH_TABLE(x)        (KISS_OBJ_TYPE(x) == KISS_HASH_TABLE)
#define KISS_IS_SEQUENCE(x)          (KISS_IS_LIST(x) || KISS_IS_STRING(x) || KISS_IS_GENERAL_VECTOR(x))
#define KISS_IS_LFUNCTION(x)         (KISS_OBJ_TYPE(x) == KISS_LFUNCTION)
#define KISS_IS_LMACRO(x)            (KISS_OBJ_TYPE(x) == KISS_LMACRO)
#define KISS_IS_CFUNCTION(x)         (KISS_OBJ_TYPE(x) == KISS_CFUNCTION)
#define KISS_IS_CMACRO(x)            (KISS_OBJ_TYPE(x) == KISS_CMACRO)
#define KISS_IS_CATCHER(x)           (KISS_OBJ_TYPE(x) == KISS_CATCHER)
#define KISS_IS_CLEANUP(x)           (KISS_OBJ_TYPE(x) == KISS_CLEANUP)
#define KISS_IS_BLOCK(x)             (KISS_OBJ_TYPE(x) == KISS_BLOCK)
#define KISS_IS_TAGBODY(x)           (KISS_OBJ_TYPE(x) == KISS_TAGBODY)
#define KISS_IS_ILOS_OBJ(x)          (KISS_OBJ_TYPE(x) == KISS_ILOS_OBJ)
#define KISS_IS_STREAM(x)            (KISS_OBJ_TYPE(x) == KISS_STREAM)
#define KISS_IS_INPUT_STREAM(x)      (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_INPUT_STREAM))
#define KISS_IS_OUTPUT_STREAM(x)     (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_OUTPUT_STREAM))
#define KISS_IS_CHARACTER_STREAM(x)  (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_CHARACTER_STREAM))
#define KISS_IS_BYTE_STREAM(x)       (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_BYTE_STREAM))
#define KISS_IS_FILE_STREAM(x)       (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_FILE_STREAM))
#define KISS_IS_STRING_STREAM(x)     (KISS_IS_STREAM(x) && ((((kiss_stream_t*)x)->flags) & KISS_STRING_STREAM))

#define KISS_IS_GC_OBJ(x)            !(KISS_IS_FIXNUM(x) || KISS_IS_FIXCHAR(x) || KISS_IS_CFUNCTION(x) || KISS_IS_CMACRO(x))


/* character.c */
kiss_obj* kiss_characterp (const kiss_obj* const obj);
kiss_obj* kiss_char_eq(const kiss_obj* const character1, const kiss_obj* const character2);
kiss_obj* kiss_char_lessthan(const kiss_obj* const character1, const kiss_obj* const character2);

/* cf_invoke.c */
kiss_obj* kiss_cf_invoke(const kiss_cfunction_t* const cfun, kiss_obj* args);

/* cons.c */


/* control.c */
kiss_obj* kiss_quote(kiss_obj* obj);
kiss_obj* kiss_and(const kiss_obj* const forms);
kiss_obj* kiss_or(const kiss_obj* const forms);
kiss_obj* kiss_equal(const kiss_obj* const obj1, const kiss_obj* const obj2);
kiss_obj* kiss_cond(const kiss_obj* const clauses);
kiss_obj* kiss_case_using(const kiss_obj* const predform, const kiss_obj* const keyform, const kiss_obj* const clauses);
kiss_obj* kiss_case(const kiss_obj* const keyform, const kiss_obj* const clauses);
kiss_obj* kiss_if(kiss_obj* test_form, kiss_obj* then_form, kiss_obj* rest);
kiss_obj* kiss_progn(kiss_obj* body);
kiss_obj* kiss_prog1(const kiss_obj* const form1, const kiss_obj* const forms);
kiss_obj* kiss_while(const kiss_obj* const test_form, const kiss_obj* const body);
kiss_catcher_t* kiss_make_catcher(kiss_obj* tag, jmp_buf jmp);
kiss_obj* kiss_catch(kiss_obj* tag_form, kiss_obj* body);
kiss_obj* kiss_throw(kiss_obj* tag_form, kiss_obj* result_form);
kiss_obj* kiss_unwind_protect(kiss_obj* protected_form, kiss_obj* cleanup_body);
kiss_obj* kiss_block(kiss_obj* name, kiss_obj* body);
kiss_obj* kiss_return_from(kiss_obj* name, kiss_obj* result_form);
kiss_obj* kiss_tagbody(kiss_obj* args);
kiss_obj* kiss_go(kiss_obj* tag);

/* error.c */
_Noreturn
void Kiss_Err(const wchar_t* const str, ...);
_Noreturn
void Kiss_Domain_Error(const kiss_obj* const obj, const wchar_t* const domain);
kiss_symbol_t* Kiss_Variable_Name(const kiss_obj* const name);
kiss_obj* Kiss_Valid_Sequence_Index(const kiss_obj* const sequence, const kiss_obj* const index);
kiss_stream_t* Kiss_Input_Char_Stream(const kiss_obj* const obj);
kiss_stream_t* Kiss_Output_Char_Stream(const kiss_obj* const obj);
kiss_stream_t* Kiss_Input_Byte_Stream(const kiss_obj* const obj);
kiss_stream_t* Kiss_Output_Byte_Stream(const kiss_obj* const obj);
kiss_file_stream_t* Kiss_Open_File_Stream(const kiss_obj* const obj);
kiss_string_stream_t* Kiss_String_Output_Stream(const kiss_obj* const obj);
kiss_obj* Kiss_Lambda_List(const kiss_obj* const list);
kiss_obj* Kiss_Lambda_Expression(const kiss_obj* const p);
_Noreturn
void Kiss_Cannot_Parse_Number_Error(const kiss_obj* const str);
_Noreturn
void Kiss_Division_By_Zero_Error(const kiss_obj* const i);
_Noreturn
void Kiss_End_Of_Stream_Error(const kiss_obj* const stream);

_Noreturn
void Kiss_Unbound_Variable_Error(const kiss_obj* const obj);
_Noreturn
void Kiss_Catcher_Not_Found_Error(const kiss_obj* const tag);
_Noreturn
void Kiss_Block_Not_Found_Error(const kiss_obj* const name);
_Noreturn
void Kiss_Tagbody_Not_Found_Error(const kiss_obj* const name);
_Noreturn
void Kiss_Arity_Error(const kiss_obj* const name, const kiss_obj* const message);

/* eval.c */
kiss_obj* kiss_invoke(const kiss_obj* const f, kiss_obj* const args);

/* format.c */
kiss_obj* kiss_format(kiss_obj* out, kiss_obj* format, kiss_obj* args);
kiss_obj* kiss_format_fresh_line(kiss_obj* output);
kiss_obj* kiss_format_integer(kiss_obj* out, kiss_obj* obj, kiss_obj* radix);
kiss_obj* kiss_format_fixnum(kiss_obj* out, kiss_obj* obj, kiss_obj* radix);
kiss_obj* kiss_format_bignum(kiss_obj* out, kiss_obj* obj, kiss_obj* radix);
kiss_obj* kiss_format_float(kiss_obj* out, kiss_obj* obj) ;
kiss_obj* kiss_format_pointer(kiss_obj* out, kiss_obj* obj);
kiss_obj* kiss_format_object(kiss_obj* out, kiss_obj* obj, kiss_obj* escapep);
kiss_obj* kiss_print(kiss_obj* obj);

/* function.c */
kiss_function_t* kiss_make_function(kiss_symbol_t* name, kiss_obj* lambda);
kiss_obj* kiss_simple_function_p(kiss_obj* obj);
kiss_obj* kiss_lf_invoke(kiss_function_t* fun, kiss_obj* args);
kiss_obj* kiss_lambda(kiss_obj* params, kiss_obj* body);
kiss_obj* kiss_defun(kiss_obj* name, kiss_obj* params, kiss_obj* body);
kiss_obj* kiss_defmacro(kiss_obj* name, kiss_obj* params, kiss_obj* body);
kiss_obj* kiss_fun_ref(kiss_symbol_t* name);
kiss_obj* kiss_function(kiss_obj* name);
kiss_obj* kiss_funcall(const kiss_obj* const f, const kiss_obj* const args);
kiss_obj* kiss_c_funcall(wchar_t* function_name, kiss_obj* args);
kiss_obj* kiss_apply(kiss_obj* f, kiss_obj* obj, kiss_obj* rest);
kiss_obj* kiss_flet(kiss_obj* fspecs, kiss_obj* body);
kiss_obj* kiss_labels(kiss_obj* fspecs, kiss_obj* body);
void kiss_bind_funargs(kiss_obj* name, kiss_obj* params, kiss_obj* args);

/* vector.c */
kiss_general_vector_t* kiss_make_general_vector(const size_t n, const kiss_obj* const obj);
kiss_obj* kiss_create_general_vector(const kiss_obj* const i, const kiss_obj* const rest);
kiss_obj* kiss_vector(const kiss_obj* objs);
kiss_obj* kiss_basic_vector_p(const kiss_obj* const obj);
kiss_obj* kiss_general_vector_p(const kiss_obj* const obj);
kiss_obj* kiss_gvref(const kiss_obj* const general_vector, const kiss_obj* const index);
kiss_obj* kiss_set_gvref(const kiss_obj* const obj, kiss_obj* general_vector, const kiss_obj* const index);

/* array.c */
kiss_obj* kiss_create_array(const kiss_obj* const dimensions, const kiss_obj* const rest);
kiss_obj* kiss_aref(const kiss_obj* const array, const kiss_obj* const rest);
kiss_obj* kiss_set_aref(const kiss_obj* const obj, kiss_obj* const array, const kiss_obj* const rest);
kiss_obj* kiss_garef(const kiss_obj* const array, const kiss_obj* const rest);
kiss_obj* kiss_set_garef(const kiss_obj* const obj, kiss_obj* const array, const kiss_obj* const rest);
kiss_obj* kiss_general_array_s_to_list (const kiss_obj* const garray);
kiss_obj* kiss_array_dimensions(const kiss_obj* const array);
kiss_obj* kiss_basic_array_p (const kiss_obj* const obj);
kiss_obj* kiss_basic_array_s_p (const kiss_obj* const obj);
kiss_obj* kiss_general_array_s_p (const kiss_obj* const obj);

/* hash_table.c */
kiss_obj* kiss_make_hash_table(kiss_obj* size, kiss_obj* test, kiss_obj* weakness, kiss_obj* rehash_size, kiss_obj* rehash_threshold);
kiss_obj* kiss_create_hash_table(kiss_obj* args);
kiss_obj* kiss_c_gethash(const kiss_obj* const key, const kiss_hash_table_t* const hash_table, const kiss_obj* const default_value);
kiss_obj* kiss_gethash(const kiss_obj* const key, const kiss_obj* const table, const kiss_obj* const rest);
kiss_obj* kiss_puthash(const kiss_obj* const key, const kiss_obj* const value, kiss_obj* const table);

/* environment.c */
kiss_environment_t* Kiss_Get_Environment(void);

/* init.c */
void kiss_initialize(void);

/* number.c */
kiss_obj* kiss_fixnum_if_possible(const kiss_obj* const obj);
kiss_bignum_t* kiss_make_bignum(kiss_ptr_int i);
kiss_obj* kiss_integerp(kiss_obj* obj);
kiss_obj* kiss_plus(kiss_obj* p);
kiss_obj* kiss_plus2(kiss_obj* a, kiss_obj* b);
kiss_obj* kiss_multiply(kiss_obj* p);
kiss_obj* kiss_minus(kiss_obj* number, kiss_obj* rest);
kiss_obj* kiss_num_eq(const kiss_obj* const x, const kiss_obj* const y);
kiss_obj* kiss_num_lessthan(kiss_obj* x, kiss_obj* y);

kiss_float_t* kiss_make_float(double d);
kiss_obj* kiss_floatp(kiss_obj* obj);
kiss_obj* kiss_c_parse_number(kiss_obj* obj);
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

/* read.c */
kiss_obj* kiss_c_read(const kiss_obj* const in, const kiss_obj* const eos_err_p, const kiss_obj* const eos_val);
kiss_obj* kiss_read(const kiss_obj* args);

/* repl.c */
int kiss_read_eval_print_loop(void);

/* sequence.c */
kiss_obj* kiss_length(const kiss_obj* const sequence);
kiss_obj* kiss_elt(const kiss_obj* const sequence, const kiss_obj* const z);
kiss_obj* kiss_set_elt(const kiss_obj* const obj, kiss_obj* const sequence, const kiss_obj* const z);
kiss_obj* kiss_subseq(const kiss_obj* const sequence, const kiss_obj* const z1, const kiss_obj* const z2);
kiss_obj* kiss_map_into(kiss_obj* const destination, const kiss_obj* const function, const kiss_obj* const rest);

/* wcs.c */
char* kiss_wctombs(const wchar_t c);
wchar_t* kiss_mbstowcs(const char* const src);
char* kiss_wcstombs(const wchar_t* const src);

/* stream.c */
void kiss_init_streams(void);
kiss_obj* kiss_open_stream_p(kiss_obj* obj);
kiss_obj* kiss_create_string_input_stream(kiss_obj* string);
kiss_obj* kiss_create_string_output_stream(void);
kiss_obj* kiss_get_output_stream_string(kiss_obj* stream);
kiss_obj* kiss_char_stream_p(kiss_obj* p);
kiss_obj* kiss_input_char_stream_p(kiss_obj* p);
kiss_obj* kiss_output_char_stream_p(kiss_obj* p);
kiss_obj* kiss_c_read_char(const kiss_obj* const in, const kiss_obj* const eos_err_p, const kiss_obj* const eos_val);
kiss_obj* kiss_c_preview_char(const kiss_obj* const in, const kiss_obj* const eos_err_p, const kiss_obj* const eos_val);
kiss_obj* kiss_read_char(const kiss_obj* args);
kiss_obj* kiss_c_read_byte(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val);
kiss_obj* kiss_read_byte(kiss_obj* input_stream, kiss_obj* args);
kiss_obj* kiss_write_byte(kiss_obj* z, kiss_obj* output);
kiss_obj* kiss_preview_char(kiss_obj* args);
kiss_obj* kiss_c_read_line(kiss_obj* in, kiss_obj* eos_err_p, kiss_obj* eos_val);
kiss_obj* kiss_read_line(kiss_obj* args);
kiss_obj* kiss_format_char(kiss_obj* out, kiss_obj* obj);
kiss_obj* kiss_open_input_file(const kiss_obj* const filename, const kiss_obj* const rest);
kiss_obj* kiss_open_output_file(kiss_obj* filename, kiss_obj* rest);
kiss_obj* kiss_open_io_file(kiss_obj* filename, kiss_obj* rest);
kiss_obj* kiss_close(kiss_obj* obj);
kiss_obj* kiss_finish_output (kiss_obj* obj);
kiss_obj* kiss_stream_ready_p(kiss_obj* obj);

kiss_obj* kiss_load(const kiss_obj* const filename);

/* string.c */
kiss_obj* kiss_create_string(const kiss_obj* const i, const kiss_obj* const rest);
kiss_obj* kiss_stringp(const kiss_obj* const obj);
kiss_obj* kiss_string_eq(const kiss_obj* const str1, const kiss_obj* const str2);
kiss_obj* kiss_string_neq(const kiss_obj* const str1, const kiss_obj* const str2);
kiss_string_t* kiss_chars_to_str(const kiss_obj* const chars);
kiss_obj* kiss_str_to_chars(const kiss_string_t* const str);
kiss_obj* kiss_string_append(const kiss_obj* const rest);

/* misc.c */
kiss_obj* kiss_identity(kiss_obj* obj);
kiss_obj* kiss_get_universal_time(void);
kiss_obj* kiss_get_internal_run_time(void);
kiss_obj* kiss_get_internal_real_time(void);
kiss_obj* kiss_internal_time_units_per_second(void);
kiss_obj* kiss_time(kiss_obj* form);

/* symbol.c */
extern size_t Kiss_Symbol_Number;
extern kiss_symbol_t* Kiss_Symbols[];
void kiss_init_symbols(void);
kiss_obj* kiss_symbolp(const kiss_obj* const obj);
kiss_obj* kiss_gensym(void);
kiss_obj* kiss_symbol_function (const kiss_obj* const obj);
kiss_obj* kiss_set_symbol_function (const kiss_obj* const definition, kiss_obj* const sym);
kiss_obj* kiss_fboundp (const kiss_obj* const obj);
kiss_obj* kiss_fmakunbound (kiss_obj* const obj);
int kiss_is_interned(const kiss_symbol_t* const p);
kiss_obj* kiss_intern(const kiss_obj* const name);
kiss_obj* kiss_property(const kiss_obj* const symbol, const kiss_obj* const property, const kiss_obj* const rest);
kiss_obj* kiss_set_property(const kiss_obj* const obj, kiss_obj* const symbol, const kiss_obj* const property);
kiss_obj* kiss_remove_property(kiss_obj* const symbol, const kiss_obj* const property);
inline
void kiss_init_string(kiss_string_t* str, wchar_t* name, size_t n) {
     str->type = KISS_STRING;
     str->str = name;
     str->n = n;
}
inline
kiss_string_t* kiss_make_string(const wchar_t* const s) {
     kiss_string_t* const p = Kiss_GC_Malloc(sizeof(kiss_string_t));
     p->type = KISS_STRING;
     p->str = NULL;
     p->n = 0;
     size_t n = wcslen(s) ;
     wchar_t* wcs = wcscpy(Kiss_Malloc(sizeof(wchar_t) * (n + 1)), s);
     kiss_init_string(p, wcs, n);
     return p;
}
inline
kiss_obj* kiss_symbol(const wchar_t* const name) {
     return kiss_intern((kiss_obj*)kiss_make_string(name));
}

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

/* ilos.c */
kiss_obj* kiss_object_p(kiss_obj* obj);
kiss_obj* kiss_make_ilos_obj(kiss_obj* info);
kiss_obj* kiss_ilos_obj_plist(const kiss_obj* const obj);
kiss_obj* kiss_set_ilos_obj_plist(const kiss_obj* const plist, kiss_obj* const obj);
kiss_obj* kiss_oref(const kiss_obj* const obj, const kiss_obj* const property);
kiss_obj* kiss_set_oref(const kiss_obj* const value, kiss_obj* const obj, const kiss_obj* const property);
kiss_obj* kiss_k_class(const kiss_obj* const name);
kiss_obj* kiss_class_of(const kiss_obj* const obj);
// predefined class names
kiss_symbol_t KISS_Sc_object;
kiss_symbol_t KISS_Sc_built_in_class;
kiss_symbol_t KISS_Sc_standard_class;
kiss_symbol_t KISS_Sc_null;
kiss_symbol_t KISS_Sc_cons;
kiss_symbol_t KISS_Sc_symbol;
kiss_symbol_t KISS_Sc_character;
kiss_symbol_t KISS_Sc_integer;
kiss_symbol_t KISS_Sc_float;
kiss_symbol_t KISS_Sc_string;
kiss_symbol_t KISS_Sc_general_vector;
kiss_symbol_t KISS_Sc_general_array_s;
kiss_symbol_t KISS_Sc_stream;
kiss_symbol_t KISS_Sc_function;
kiss_symbol_t KISS_Sc_hash_table;

/* gf_invoke.c */
kiss_obj* kiss_method_invoke(const kiss_obj* const m);

/* class.c */
//kiss_obj* kiss_type_to_class_name(kiss_type t);

/* environment.c */
void kiss_init_environment(void);

/* feature.c */
kiss_obj* kiss_featurep(kiss_obj* feature);
kiss_obj* kiss_provide(kiss_obj* feature);

inline
kiss_obj* kiss_make_integer(kiss_ptr_int i) {
     return (i > KISS_PTR_INT_MAX) ? (kiss_obj*)kiss_make_bignum(i) : kiss_make_fixnum(i);
}

inline
kiss_cons_t* Kiss_Cons(const kiss_obj* const obj) {
     if (KISS_IS_CONS(obj)) { return (kiss_cons_t*)obj; }
     Kiss_Domain_Error(obj, L"<cons>");
}

inline
kiss_hash_table_t* Kiss_Hash_Table(const kiss_obj* const obj) {
     if (KISS_IS_TASH_TABLE(obj)) { return (kiss_hash_table_t*)obj; }
     Kiss_Domain_Error(obj, L"hash-table");
}

inline
kiss_ptr_int Kiss_Fixnum(const kiss_obj* obj) {
     if (KISS_IS_FIXNUM(obj)) { return kiss_ptr_int(obj); }
     obj = kiss_fixnum_if_possible(obj);
     if (KISS_IS_FIXNUM(obj)) { return kiss_ptr_int(obj); }
     Kiss_Domain_Error(obj, L"fixnum");
}

inline
kiss_bignum_t* Kiss_Bignum(const kiss_obj* const obj) {
     if (KISS_IS_BIGNUM(obj)) { return (kiss_bignum_t*)obj; }
     Kiss_Domain_Error(obj, L"bignum");
}

inline
kiss_obj* Kiss_Integer(const kiss_obj* const obj) {
     if (KISS_IS_FIXNUM(obj) || KISS_IS_BIGNUM(obj)) { return (kiss_obj*)obj; }
     Kiss_Domain_Error(obj, L"<integer>");
}

inline
kiss_obj* Kiss_Number(const kiss_obj* const obj) {
     if (KISS_IS_INTEGER(obj) || KISS_IS_FLOAT(obj)) { return (kiss_obj*)obj; }
     Kiss_Domain_Error(obj, L"<number>");
}

inline
kiss_ptr_int Kiss_Non_Negative_Fixnum(const kiss_obj* const obj) {
     const kiss_ptr_int i = Kiss_Fixnum(obj);
     if (i >= 0) { return i; }
     Kiss_Domain_Error(obj, L"non negative fixnum");
}

inline
kiss_ptr_int Kiss_Non_Zero_Fixnum(const kiss_obj* const obj) {
     const kiss_ptr_int i = Kiss_Fixnum(obj);
     if (i != 0) { return i; }
     Kiss_Domain_Error(obj, L"non zero fixnum");
}

inline
kiss_obj* Kiss_General_Array(const kiss_obj* const obj) {
     if (KISS_IS_GENERAL_VECTOR(obj) || KISS_IS_GENERAL_ARRAY_S(obj)) {
          return (kiss_obj*)obj;
     }
     Kiss_Domain_Error(obj, L"general array (<general-vector> or <general-array*>)");
}

inline
kiss_obj* Kiss_Sequence(const kiss_obj* const obj) {
     if (KISS_IS_SEQUENCE(obj)) { return (kiss_obj*)obj; }
     Kiss_Domain_Error(obj, L"sequence");
}

inline
kiss_ilos_obj_t* Kiss_ILOS_Obj(const kiss_obj* const obj) {
     if (KISS_IS_ILOS_OBJ(obj)) { return (kiss_ilos_obj_t*)obj; }
     Kiss_Domain_Error(obj, L"ILOS object");
}

inline
kiss_obj* Kiss_Basic_Array(const kiss_obj* const obj) {
     if (KISS_IS_GENERAL_VECTOR(obj) || KISS_IS_GENERAL_ARRAY_S(obj) || KISS_IS_STRING(obj)) {
          return (kiss_obj*)obj;
     }
     Kiss_Domain_Error(obj, L"<basic array>");
}


inline
kiss_obj* Kiss_List(const kiss_obj* const obj) {
     if (obj == KISS_NIL || KISS_IS_CONS(obj)) { return (kiss_obj*)obj; }
     Kiss_Domain_Error(obj, L"<list>");
}

/* Proper list is a list terminated by the empty list. (The empty list is a proper list.) */
inline
kiss_obj* Kiss_Proper_List(const kiss_obj* const obj) {
     const kiss_obj* p = obj;
     while (KISS_IS_CONS(p)) { p = KISS_CDR(p); }
     if (p == KISS_NIL) { return (kiss_obj*)obj; }
     Kiss_Domain_Error(obj, L"proper list");
}

inline
kiss_cons_t* Kiss_Proper_List_2(const kiss_obj* const obj) {
     const kiss_obj* p = obj;
     size_t i = 0;
     while (KISS_IS_CONS(p) && i < 4) { i++; p = KISS_CDR(p); }
     if (p == KISS_NIL && i == 2) { return (kiss_cons_t*)obj; }
     Kiss_Domain_Error(obj, L"proper list of length two");
}

inline
kiss_float_t* Kiss_Float(const kiss_obj* const obj) {
     if (KISS_IS_FLOAT(obj)) { return (kiss_float_t*)obj; }
     Kiss_Domain_Error(obj, L"<float>");
}

inline
wchar_t Kiss_Character(const kiss_obj* const obj) {
     if (KISS_IS_FIXCHAR(obj)) { return kiss_wchar(obj); }
     Kiss_Domain_Error(obj, L"fixchar");
}

inline
kiss_symbol_t* Kiss_Symbol(const kiss_obj* const obj) {
     if (KISS_IS_SYMBOL(obj)) { return (kiss_symbol_t*)obj; }
     Kiss_Domain_Error(obj, L"<symbol>");
}

inline
kiss_string_t* Kiss_String(const kiss_obj* const obj) {
     if (KISS_IS_STRING(obj)) { return (kiss_string_t*)obj; }
     Kiss_Domain_Error(obj, L"<string>");
}

static inline
kiss_stream_t* Kiss_Stream(const kiss_obj* const obj) {
     if (KISS_IS_STREAM(obj)) { return (kiss_stream_t*)obj; }
     Kiss_Domain_Error(obj, L"<stream>");
}

inline
kiss_general_vector_t* Kiss_General_Vector(const kiss_obj* const obj) {
     if (KISS_IS_GENERAL_VECTOR(obj)) { return (kiss_general_vector_t*)obj; }
     Kiss_Domain_Error(obj, L"<general-vector>");
}

inline
kiss_general_array_t* Kiss_General_Array_S(const kiss_obj* const obj) {
     if (KISS_IS_GENERAL_ARRAY_S(obj)) { return (kiss_general_array_t*)obj; }
     Kiss_Domain_Error(obj, L"<general-array*>");
}

inline
kiss_function_t* Kiss_LFunction(const kiss_obj* const obj) {
     if (KISS_IS_LFUNCTION(obj)) { return (kiss_function_t*)obj; }
     Kiss_Domain_Error(obj, L"lisp function");
}

inline
kiss_function_t* Kiss_LMacro(const kiss_obj* const obj) {
     if (KISS_IS_LMACRO(obj)) { return (kiss_function_t*)obj; }
     Kiss_Domain_Error(obj, L"lisp macro");
}

inline
kiss_cfunction_t* Kiss_CFunction(const kiss_obj* const obj) {
     if (KISS_IS_CFUNCTION(obj)) { return (kiss_cfunction_t*)obj; }
     Kiss_Domain_Error(obj, L"c function");
}

inline
kiss_cfunction_t* Kiss_CMacro(const kiss_obj* const obj) {
     if (KISS_IS_CMACRO(obj)) { return (kiss_cfunction_t*)obj; }
     Kiss_Domain_Error(obj, L"c macro");
}

/* function: (not obj) -> boolean
   This predicate is the logical `not'. It returns t
   if obj is nil and nil otherwise. obj may be any ISLISP object. */
inline
kiss_obj* kiss_not(const kiss_obj* const obj) {
     return obj == KISS_NIL ? KISS_T : KISS_NIL;
}

/* function: (eq obj1 obj2) -> boolean
   the consequences are implementation defined if both obj1 and obj2 are
   numbers or both are characters.*/
inline
kiss_obj* kiss_eq(const kiss_obj* const obj1, const kiss_obj* const obj2) {
     return obj1 == obj2 ? KISS_T : KISS_NIL;
}


/* function: (eql obj1 obj2) -> boolean
   the meaning for numbers and characters is defined as follows:
   • If obj1 and obj2 are numbers, eql tests whether they are direct
   instances of the same class and have the same value.  If an
   implementation supports positive and negative zeros as distinct
   values, then (eql 0.0 -0.0) returns nil. When the syntax -0.0 is read
   and it is interpreted as the value 0.0 then (eql 0.0 -0.0) returns t.
   • If obj1 and obj2 are characters, eql tests whether they are the same
   character (see char=). */
inline
kiss_obj* kiss_eql(const kiss_obj* const obj1, const kiss_obj* const obj2) {
     if (KISS_IS_INTEGER(obj1) && KISS_IS_INTEGER(obj2)) {
	  return kiss_num_eq(obj1, obj2);
     }
     if (KISS_IS_FLOAT(obj1) && KISS_IS_FLOAT(obj2)) {
	  return kiss_num_eq(obj1, obj2);
     }
     if (KISS_IS_CHARACTER(obj1) && KISS_IS_CHARACTER(obj2)) {
	  return obj1 == obj2 ? KISS_T : KISS_NIL;
     }
     return kiss_eq(obj1, obj2);
}

/* function: (streamp obj ) -> boolean
   Returns t if OBJ is a stream (instance of class <stream>); otherwise,
   returns nil. OBJ may be any ISLISP object. streamp is unaffected by
   whether its argument, if an instance of the class <stream>, is open or
   closed.
   Example: (streamp (standard-input)) => t
            (streamp '()) => nil */
inline
kiss_obj* kiss_streamp(kiss_obj* obj) {
     return KISS_IS_STREAM(obj) ? KISS_T : KISS_NIL;
}

/* function: (input-stream-p obj) -> boolean
   Returns t if OBJ is a stream that can handle input operations;
   otherwise, returns nil. */
inline
kiss_obj* kiss_input_stream_p(kiss_obj* p) {
     return KISS_IS_INPUT_STREAM(p) ? KISS_T : KISS_NIL;
}

/* function: (output-stream-p obj) -> boolean
   Returns t if OBJ is a stream that can handle output operations;
   otherwise, returns nil. */
inline
kiss_obj* kiss_output_stream_p(kiss_obj* p) {
     return KISS_IS_OUTPUT_STREAM(p) ? KISS_T : KISS_NIL;
}

/* function: (standard-input) -> <stream> */
inline
kiss_obj* kiss_standard_input(void)  {
     return kiss_dynamic(kiss_symbol(L"*kiss::standard-input*"));
}

/* function: (standard-output) -> <stream> */
inline
kiss_obj* kiss_standard_output(void) {
     return kiss_dynamic(kiss_symbol(L"*kiss::standard-output*"));
}

/* function: (error-output) -> <stream> */
inline
kiss_obj* kiss_error_output(void)    {
     return kiss_dynamic(kiss_symbol(L"*kiss::error-output*"));
}

/* function: (consp obj) -> boolean
   Returns t if OBJ is a cons (instance of class <cons>); otherwise, returns nil.
   OBJ may be any ISLISP object. */
inline
kiss_obj* kiss_consp(const kiss_obj* const obj) { return KISS_IS_CONS(obj) ? KISS_T : KISS_NIL; }

/* function: (car cons) -> <object>
   Returns the left component of the CONS.
   An error shall be signaled if CONS is not a cons (error-id. domain-error). */
inline
kiss_obj* kiss_car(const kiss_obj* const p) { return KISS_CAR(Kiss_Cons(p)); }

/* function: (cdr cons) -> <object>
   Returns the right component of the CONS.
   An error shall be signaled if CONS is not a cons (error-id. domain-error). */
inline
kiss_obj* kiss_cdr(const kiss_obj* const p) { return KISS_CDR(Kiss_Cons(p)); }

inline
kiss_obj* kiss_cadr(const kiss_obj* const p)  { return kiss_car(kiss_cdr(p)); }
inline
kiss_obj* kiss_cddr(const kiss_obj* const p)  { return kiss_cdr(kiss_cdr(p)); }
inline
kiss_obj* kiss_caddr(const kiss_obj* const p) { return kiss_car(kiss_cddr(p)); }

/* function: (null obj) -> boolean
   Returns t if OBJ is nil; otherwise, returns nil.
   OBJ may be any ISLISP object. */
inline
kiss_obj* kiss_null(const kiss_obj* const p) { return (p == KISS_NIL ? KISS_T : KISS_NIL); }

/* function: (listp obj) -> boolean
   Returns t if OBJ is a list (instance of class <list>);
   otherwise, returns nil. OBJ may be any ISLISP object. */
inline
kiss_obj* kiss_listp(const kiss_obj* const p) {
     return (p == KISS_NIL || KISS_IS_CONS(p) ? KISS_T : KISS_NIL);
}


/* function (nreverse list) -> <list>
   Return a list whose elements are those of the given LIST, but in reverse
   order.  An error shall be signaled if LIST is not a list (error-id. domain-error ).
   the conses which make up the top level of the given list are permitted,
   but not required, to be side-effected in order to produce this new list.
   nreverse should never be called on a literal object. */
inline
kiss_obj* kiss_nreverse(kiss_obj* p) {
     p = Kiss_List(p);
     if (p == KISS_NIL) {
          return p;
     } else {
          /*
             +---+    +---+    
             |   |--->|   |--->nil
             +---+    +---+    
          */
          kiss_obj* p2 = KISS_CDR(p);
          ((kiss_cons_t*)p)->cdr = KISS_NIL;
          while (KISS_IS_CONS(p2)) {
               kiss_obj* p3 = KISS_CDR(p2);
               /*
                 +---+           +---+    p3
                 |p  |--->nil    |p2 |--->nil
                 +---+           +---+    
               */
               ((kiss_cons_t*)p2)->cdr = p;
               p = p2;
               p2 = p3;
               /*                        p2
                        +---+    +---+   p3
                 nil<---|   |<---|p  |   nil
                        +---+    +---+
               */
          }
          return p;
     }
}

/* function: (assoc obj association-list) -> <list>
   If ASSOCATION-LIST contains at least one cons whose car is OBJ (as
   determined by eql), the first such cons is returned. Otherwise, nil is
   returned. An error shall be signaled if ASSOCIATION-LIST is not a list
   of conses (error-id. domain-error ).

   Example:
   (assoc 'a '((a . 1) (b . 2))) => (a . 1) */
inline
kiss_obj* kiss_assoc(const kiss_obj* const obj, kiss_obj* const alist) {
    for (const kiss_obj* p = Kiss_List(alist); KISS_IS_CONS(p); p = KISS_CDR(p)) {
        kiss_cons_t* x = Kiss_Cons(KISS_CAR(p));
        if (kiss_eql(obj, x->car) == KISS_T) { return (kiss_obj*)x; }
    }
    return KISS_NIL;
}

inline
kiss_cons_t* kiss_init_cons(kiss_cons_t* const p, const kiss_obj* const left, const kiss_obj* const right)
{
    p->type = KISS_CONS;
    p->car = (kiss_obj*)left;
    p->cdr = (kiss_obj*)right;
    return p;
}

inline
void kiss_copy_list_to_consarray(const kiss_obj* const list, kiss_cons_t* const pointer) {
     kiss_cons_t* p = pointer;
     for(const kiss_obj* q = list; KISS_IS_CONS(q); q = KISS_CDR(q)) {
          kiss_init_cons(p, KISS_CAR(q), (kiss_obj*)((kiss_cons_t*)p + 1));
          p++;
     }
     if (p != pointer) {
          p--;
          p->cdr = KISS_NIL;
     }
}

/* function: (cons obj1 obj2) -> <cons>
   Builds a cons from two objects, with OBJ1 as its car (or `left') part and
   with OBJ2 as its cdr (or `right') part.
   An error shall be signaled if the requested cons cannot be allocated 
   (error-id. cannot-create-cons). 
   Both OBJ1 and OBJ2 may be any ISLISP object. */
inline
kiss_obj* kiss_cons(const kiss_obj* const car, const kiss_obj* const cdr) {
     return (kiss_obj*)kiss_init_cons(Kiss_GC_Malloc(sizeof(kiss_cons_t)), car, cdr);
}

inline
void kiss_push(const kiss_obj* const elm, kiss_obj** const list) {
    *list = kiss_cons(elm, *list);
}

inline
size_t kiss_c_length(const kiss_obj* const p) {
     Kiss_Sequence(p);
     switch (KISS_OBJ_TYPE(p)) {
     case KISS_SYMBOL:
	  assert(p == KISS_NIL);
	  return 0;
     case KISS_CONS: {
	  size_t n = 0;
	  for (const kiss_obj* q = p; KISS_IS_CONS(q); q = KISS_CDR(q)) { n++; }
	  return n;
     }
     case KISS_STRING: return ((kiss_string_t*)p)->n;
     case KISS_GENERAL_VECTOR: return ((kiss_general_vector_t*)p)->n;
     default:
	  fwprintf(stderr, L"kiss_c_length: unknown primitive type %d", KISS_OBJ_TYPE(p));
	  exit(EXIT_FAILURE);
     }
}

/* function: (set-car obj cons) -> <object>
   Updates the left component of CONS with OBJ.
   The returned value is OBJ.
   An error shall be signaled if CONS is not a cons (error-id. domain-error).
   OBJ may be any ISLISP object */
inline
kiss_obj* kiss_set_car(const kiss_obj* const obj, kiss_obj* const cons) {
    kiss_cons_t* const p = Kiss_Cons(cons);
    p->car = (kiss_obj*)obj;
    return (kiss_obj*)obj;
}

/* function: (set-cdr obj cons) -> <object>
   Updates the right component of CONS with OBJ. The returned value is OBJ.
   An error shall be signaled if CONS is not a cons (error-id. domain-error).
   OBJ may be any ISLISP object. */
inline
kiss_obj* kiss_set_cdr(const kiss_obj* const obj, kiss_obj* const cons) {
    kiss_cons_t* const p = Kiss_Cons(cons);
    p->cdr = (kiss_obj*)obj;
    return (kiss_obj*)obj;
}

/* function: (create-list i [initial-element]) -> <list>
   Returns a list of length I. If INITIAL-ELEMENT is given,
   the elements of the new list are initialized with this object;
   otherwise, the initialization is implementation defined.
   An error shall be signaled if the requested list cannot be allocated
   (error-id. cannot-create-list).
   An error shall be signaled if I is not a non-negative integer (error-id. domain-error).
   INITIAL-ELEMENT may be any ISLISP object. */
inline
kiss_obj* kiss_create_list(const kiss_obj* const i, const kiss_obj* const rest) {
     kiss_ptr_int n = Kiss_Non_Negative_Fixnum(i);
     kiss_obj* init = rest == KISS_NIL ? KISS_NIL : KISS_CAR(rest);
     kiss_obj* p = KISS_NIL;
     for (; n > 0; n--) {
          kiss_push(init, &p);
     }
     return p;
}


/* CL function: (copy-list list) -> copy
   Returns a copy of LIST. If LIST is a dotted list, the resulting list will
   also be a dotted list. Only the list structure of LIST is copied;
   the elements of the resulting list are the same as the corresponding
   elements of the given LIST. */
inline
kiss_obj* kiss_copy_list(const kiss_obj* p) {
     kiss_cons_t head;
     kiss_init_cons(&head, KISS_NIL, KISS_NIL);
     kiss_obj* const result = (kiss_obj*)&head;
     kiss_obj* here = result;
     for (p = Kiss_List((kiss_obj*)p); KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_set_cdr(kiss_cons(KISS_CAR(p), KISS_NIL), here);
          here= KISS_CDR(here);
     }
     kiss_set_cdr(p, here);
     return KISS_CDR(result);
}

/* function: (list obj*)-> <list>
   Returns a new list whose length is the number of arguments and whose
   elements are the arguments in the same order as in the list-form.
   An error shall be signaled if the requested list cannot be allocated
   (error-id. cannot-create-list). Each OBJ may be any ISLisp object. */
inline
kiss_obj* kiss_list(kiss_obj* const p) { return kiss_copy_list(p); }

inline
kiss_obj* kiss_c_list(int nargs, ...) {
     kiss_cons_t head;
     kiss_init_cons(&head, KISS_NIL, KISS_NIL);
     kiss_obj* const result = (kiss_obj*)&head;
     kiss_obj* here = result;
     va_list args;
     va_start(args, nargs);
     while (nargs-- > 0) {
          kiss_set_cdr(kiss_cons(va_arg(args, kiss_obj*), KISS_NIL), here);
          here= KISS_CDR(here);
     }
     va_end(args);
     return KISS_CDR(result);
}

/* kiss_c_mapcar(function, list) -> new_list */
inline
kiss_obj* kiss_c_mapcar1(const kiss_cf1_t f, const kiss_obj* const list) {
     kiss_cons_t result;
     kiss_init_cons(&result, KISS_NIL, KISS_NIL);
     kiss_obj* p = (kiss_obj*)&result;
     for (kiss_obj* q = Kiss_List(list); KISS_IS_CONS(q); q = KISS_CDR(q)) {
          kiss_set_cdr(kiss_cons(f(KISS_CAR(q)), KISS_NIL), p);
          p = KISS_CDR(p);
     }
     return KISS_CDR(&result);
}

inline
kiss_obj* kiss_mapcar1(const kiss_obj* const f, const kiss_obj* const list) {
     kiss_cons_t result;
     kiss_init_cons(&result, KISS_NIL, KISS_NIL);
     kiss_obj* p = (kiss_obj*)&result;
     kiss_cons_t args;
     kiss_init_cons(&args, KISS_NIL, KISS_NIL);
     for (const kiss_obj* q = Kiss_List(list); KISS_IS_CONS(q); q = KISS_CDR(q)) {
          kiss_set_car(KISS_CAR(q), (kiss_obj*)&args);
          kiss_set_cdr(kiss_cons(kiss_funcall(f, (kiss_obj*)&args), KISS_NIL), p);
          p = KISS_CDR(p);
     }
     return KISS_CDR(&result);
}


/* kiss_c_mapc(function, list) -> list */
inline
kiss_obj* kiss_c_mapc1(const kiss_cf1_t f, const kiss_obj* const list) {
    for (kiss_obj* p = Kiss_List(list); KISS_IS_CONS(p); p = KISS_CDR(p)) {
        f(KISS_CAR(p));
    }
    return (kiss_obj*)list;
}

/* kiss function: (append* [list* last]) -> <list>
   LAST doesn't have to be a list. This behaviour is the same as the Common Lisp's append */
inline
kiss_obj* kiss_append_s(kiss_obj* p) {
     if (p == KISS_NIL) { return KISS_NIL; } /* (append*) -> nil       */
     else if (!KISS_IS_CONS(KISS_CDR(p))) {  /* (append* last) -> last */
          return KISS_CAR(p);
     } else {                                /* (append* '(1 2) '(3 4) 'a) => (1 2 3 4 . a) */
          kiss_cons_t head;
          kiss_init_cons(&head, KISS_NIL, KISS_NIL);
          kiss_cons_t* tail = &head;
          kiss_obj *list1, *list2, *rest;
          do {
               list1 = Kiss_List(KISS_CAR(p));
               list2 = KISS_CADR(p);
               rest  = KISS_CDDR(p);
               while (KISS_IS_CONS(list1)) {
                    tail->cdr = kiss_cons(KISS_CAR(list1), KISS_NIL);
                    tail = (kiss_cons_t*)tail->cdr;
                    list1 = KISS_CDR(list1);
               }
               tail->cdr = list2;
               p = KISS_CDR(p);
          } while (KISS_IS_CONS(rest));
          return head.cdr;
     }
}

/* function: (append list*) -> <list>
   Returns the result of appending all of the LISTS, or () if given no lists.
   An error shall be signaled if any LIST is not a list (error-id. domain-error).
   This function does not modify its arguments.
   It is implementation defined whether and when the result shares structure with its
   LIST arguments.
   An error shall be signaled if the list cannot be allocated (error-id. cannot-create-list). */
inline
kiss_obj* kiss_append(kiss_obj* const p) {
     kiss_c_mapc1((kiss_cf1_t)Kiss_List, p);
     return kiss_append_s(p);
}

inline
kiss_obj* kiss_c_append(int nargs, ...) {
    va_list args;
    kiss_obj* stack = KISS_NIL;
    va_start(args, nargs);  
    while (nargs-- > 0) { kiss_push(va_arg(args, kiss_obj*), &stack); }
    va_end(args);
    return kiss_append(kiss_nreverse(stack));
}

/* function: (reverse list) -> <list>
   Return a list whose elements are those of the given LIST, but in reverse
   order. An error shall be signaled if LIST is not a list (error-id. domain-error).
   no side-effect to the given LIST occurs. The resulting list is permitted
   but not required to share structure with the input LIST.*/
inline
kiss_obj* kiss_reverse(kiss_obj* p) {
    kiss_obj* stack = KISS_NIL;
    for (p = Kiss_List(p); KISS_IS_CONS(p); p = KISS_CDR(p)) {
        kiss_push(KISS_CAR(p), &stack);
    }
    return stack;
}


/* function: (member obj list) -> <list>
   If LIST contains at least one occurrence of OBJ (as determined by eql),
   the first sublist of LIST whose car is OBJ is returned.
   Otherwise, nil is returned.
   An error shall be signaled if LIST is not a list (error-id. domain-error ).

   Example: (member 'c '(a b c d e f)) => (c d e f) */
inline
kiss_obj* kiss_member(kiss_obj* const obj, kiss_obj* const list) {
     for (const kiss_obj* p = Kiss_List(list); KISS_IS_CONS(p); p = KISS_CDR(p)) {
          if (kiss_eql(KISS_CAR(p), obj) == KISS_T) { return (kiss_obj*)p; }
     }
     return KISS_NIL;
}

/* function: (member-using predicate obj list) -> <list>
   If LIST contains at least one occurrence of OBJ (as determined by PREDICATE),
   the first sublist of LIST whose car is OBJ is returned.
   Otherwise, nil is returned.
   An error shall be signaled if LIST is not a list (error-id. domain-error ).

   Example: (member-using #'eq 'c '(a b c d e f)) => (c d e f) */
inline
kiss_obj* kiss_member_using(const kiss_obj* const predicate, kiss_obj* const obj, kiss_obj* const list)
{
     for (const kiss_obj* p = Kiss_List(list); KISS_IS_CONS(p); p = KISS_CDR(p)) {
          if (kiss_funcall(predicate, kiss_c_list(2, KISS_CAR(p), obj)) != KISS_NIL) { return (kiss_obj*)p; }
     }
     return KISS_NIL;
}


/* Emacs Lisp function: (plist-member plist property) -> <tail of plist>
   This returns non-`nil' if PLIST contains the given PROPERTY.
   Unlike `plist-get', this allows you to distinguish between a
   missing property and a property with the value `nil'.  The value
   is actually the tail of PLIST whose `car' is PROPERTY. */
inline
kiss_obj* kiss_plist_member (kiss_obj* plist, const kiss_obj* const property) {
     for (plist = Kiss_List(plist); KISS_IS_CONS(plist); plist = KISS_CDR(plist)) {
          if (KISS_CAR(plist) == property) {
               return plist;
          }
          plist = KISS_CDR(plist);
          if (!KISS_IS_CONS(plist)) { return KISS_NIL; }
     }
     return KISS_NIL;
}

inline
kiss_obj* kiss_plist_remove(kiss_obj* plist, const kiss_obj* const property) {
     kiss_obj* prev = KISS_NIL;
     kiss_obj* p = Kiss_List(plist);
     while (KISS_IS_CONS(p)) {
          if (KISS_CAR(p) == property) {
               if (prev == KISS_NIL) {
                    p = kiss_cddr(p);
                    plist = p;
               } else {
                    p = kiss_cddr(p);
                    kiss_set_cdr(p, prev);
               }
          } else {
               p = KISS_CDR(p);
               prev = p;
               p = kiss_cdr(p);
          }
     }
     return plist;
}

/* Emacs Lisp function: (plist-get plist property) -> value
   This returns the value of the PROPERTY property stored in the
   property list PLIST.  It accepts a malformed PLIST argument.  If
   PROPERTY is not found in the PLIST, it returns `nil'.

   Example:
   (plist-get '(foo 4) 'foo)     => 4
   (plist-get '(foo 4 bad) 'foo) => 4
   (plist-get '(foo 4 bad) 'bad) => nil
   (plist-get '(foo 4 bad) 'bar) => nil */
inline
kiss_obj* kiss_plist_get (kiss_obj* plist, const kiss_obj* const property) {
    kiss_obj* here = kiss_plist_member(plist, property);
    if (here == KISS_NIL) {
        return KISS_NIL;
    } else {
        here = KISS_CDR(here);
        if (KISS_IS_CONS(here)) {
            return KISS_CAR(here);
        } else {
            return KISS_NIL;
        }
    }
}

/* Emacs Lisp function: (plist-put plist property value) -> modified-plist
   This stores VALUE as the value of the PROPERTY property in the
   property list PLIST.  It may modify PLIST destructively, or it may
   construct a new list structure without altering the old.  The
   function returns the modified property list, so you can store that
   back in the place where you got PLIST.  For example,

   Example:
   (setq my-plist '(bar t foo 4)) => (bar t foo 4)
   (setq my-plist (plist-put my-plist 'foo 69)) => (bar t foo 69)
   (setq my-plist (plist-put my-plist 'quux '(a))) => (bar t foo 69 quux (a)) */
inline
kiss_obj* kiss_plist_put(kiss_obj* plist, const kiss_obj* const property, const kiss_obj* const value)
{
    kiss_obj* here = kiss_plist_member(plist, property);
    if (here == KISS_NIL) {
        return kiss_c_append(2, plist, kiss_c_list(2, property, value));
    } else {
        kiss_set_car(value, kiss_cdr(here));
        return plist;
    }
}

inline
kiss_obj* kiss_plist_mapc(const kiss_obj* const function, const kiss_obj* const plist) {
     for (const kiss_obj* p = Kiss_List(plist); KISS_IS_CONS(p); p = KISS_CDDR(p)) {
          kiss_funcall(function, kiss_c_list(2, KISS_CAR(p), kiss_cadr(p)));
     }
     return (kiss_obj*)plist;
}


inline
kiss_obj* kiss_assoc_using(const kiss_obj* test, const kiss_obj* const obj, kiss_obj* const alist) {
    for (const kiss_obj* p = Kiss_List(alist); KISS_IS_CONS(p); p = KISS_CDR(p)) {
        kiss_cons_t* x = Kiss_Cons(KISS_CAR(p));
        if (kiss_funcall(test, kiss_c_list(2, obj, x->car)) != KISS_NIL) { return (kiss_obj*)x; }
    }
    return KISS_NIL;
}

/* Common Lisp fucntion: last list &optional n => tail
   Arguments and Values:
   list---a list, which might be a dotted list but must not be a circular list.
   n---a non-negative integer. The default is 1.
   tail---an object.
  
   Description:
   last returns the last n conses (not the last n elements) of list).
   If list is (), last returns ().
   If n is zero, the atom that terminates list is returned. If n is greater
   than or equal to the number of cons cells in list, the result is list. */
inline
kiss_obj* kiss_last(const kiss_obj* const list, const kiss_obj* const rest) {
     kiss_ptr_int n = rest == KISS_NIL ? 1 : Kiss_Non_Negative_Fixnum(kiss_car(rest));
     const kiss_obj* p = Kiss_List(list);
     size_t len = kiss_c_length(list);
     for (kiss_ptr_int i = len - n; i > 0; i--) p = kiss_cdr(p);
     return (kiss_obj*)p;
}

/* Common Lisp function: nconc &rest lists => concatenated-list
   Arguments and Values:
   list---each but the last must be a list (which might be a dotted list
   but must not be a circular list); the last list may be any object.
   concatenated-list---a list. */
inline
kiss_obj* kiss_nconc(kiss_obj* const lists) {
     kiss_cons_t head;
     kiss_init_cons(&head, KISS_NIL, KISS_NIL);
     kiss_obj* q = (kiss_obj*)&head;
     for (kiss_obj* p = lists; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_obj* l = Kiss_List(KISS_CAR(p));
          kiss_set_cdr(l, q);
          q = kiss_last(q, KISS_NIL);
     }
     return head.cdr;
}


/*  function: (mapcar function list+) -> <list>
    Operates on successive elements of the LISTS. FUNCTION is applied to
    the first element of each LIST, then to the second element of each LIST,
    and so on. The iteration terminates when the shortest LIST runs out,
    and excess elements in other LISTS are ignored.
    The value returned by mapcar is a list of the results of successive calls
    to function. */
inline
kiss_obj* kiss_mapcar(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest)
{
     size_t n = kiss_c_length(rest);
     if (n == 0) { return kiss_mapcar1(function, list1); }
     kiss_cons_t stack_rest[n];
     kiss_copy_list_to_consarray(rest, stack_rest);
     kiss_cons_t args;
     kiss_init_cons(&args, list1, (kiss_obj*)stack_rest);
     for (kiss_obj* x = (kiss_obj*)&args; KISS_IS_CONS(x); x = KISS_CDR(x))
          Kiss_List(KISS_CAR(x));
     kiss_cons_t result;
     kiss_init_cons(&result, KISS_NIL, KISS_NIL);
     kiss_obj* p = (kiss_obj*)&result;
     if (kiss_member(KISS_NIL, (kiss_obj*)&args) != KISS_NIL) { return KISS_NIL; }
     while(1) {
          kiss_set_cdr(kiss_cons(kiss_funcall(function,
                                              kiss_c_mapcar1((kiss_cf1_t)kiss_car,
                                                             (kiss_obj*)&args)),
                                 KISS_NIL),
                       p);
          p = KISS_CDR(p);
          for (kiss_obj* q = (kiss_obj*)&args; KISS_IS_CONS(q); q = KISS_CDR(q)) {
               kiss_obj* obj = KISS_CDR(KISS_CAR(q));
               if (!KISS_IS_CONS(obj)) {
                    goto end;
               }
               kiss_set_car(obj, q);
          }
     }
end:
     return result.cdr;
}

/* function: (mapcan function list+) -> <list>
   mapcan is like mapcar, except that the results of applying function are combined
   into a list by the use of an operation that performs a destructive form of
   append rather than list. */
inline
kiss_obj* kiss_mapcan(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest)
{
     return kiss_nconc(kiss_mapcar(function, list1, rest));
}

inline
kiss_obj* kiss_mapc1(const kiss_obj* const function, const kiss_obj* const list) {
     for (const kiss_obj* p = list; KISS_IS_CONS(p); p = KISS_CDR(p)) {
          kiss_funcall(function, kiss_c_list(1, KISS_CAR(p)));
     }
     return (kiss_obj*)list;
}

inline
kiss_obj* kiss_mapc(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest)
{
     size_t n = kiss_c_length(rest);
     if (n == 0) { return kiss_mapc1(function, list1); }
     kiss_cons_t stack_rest[n];
     kiss_copy_list_to_consarray(rest, stack_rest);
     kiss_cons_t args;
     kiss_init_cons(&args, list1, (kiss_obj*)stack_rest);
     for (kiss_obj* x = (kiss_obj*)&args; KISS_IS_CONS(x); x = KISS_CDR(x))
          Kiss_List(KISS_CAR(x));
     if (kiss_member(KISS_NIL, (kiss_obj*)&args) != KISS_NIL) {
          return (kiss_obj*)list1;
     }
     while(1) {
          kiss_funcall(function, kiss_c_mapcar1((kiss_cf1_t)kiss_car, (kiss_obj*)&args));
          for (kiss_obj* q = (kiss_obj*)&args; KISS_IS_CONS(q); q = KISS_CDR(q)) {
               kiss_obj* obj = KISS_CDR(KISS_CAR(q));
               if (!KISS_IS_CONS(obj)) {
                    goto end;
               }
               kiss_set_car(obj, q);
          }
     }
end:
     return (kiss_obj*)list1;
}

inline
kiss_obj* kiss_maplist1(const kiss_obj* const function, const kiss_obj* const list) {
     kiss_cons_t result;
     kiss_init_cons(&result, KISS_NIL, KISS_NIL);
     kiss_obj* p = (kiss_obj*)&result;
     for (const kiss_obj* q = list; KISS_IS_CONS(q); q = KISS_CDR(q)) {
          kiss_set_cdr(kiss_cons(kiss_funcall(function, kiss_cons(q, KISS_NIL)), KISS_NIL), p);
          p = KISS_CDR(p);
     }
     return result.cdr;
}

/* function: (maplist function list+) -> <list>
   maplist is like mapcar except that function is applied to successive sublists of
   the lists. function is first applied to the lists themselves, and then to the cdr of
   each list, and then to the cdr of the cdr of each list, and so on. */
inline
kiss_obj* kiss_maplist(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest)
{
     size_t n = kiss_c_length(rest);
     if (n == 0) { return kiss_maplist1(function, list1); }
     kiss_cons_t stack_rest[n];
     kiss_copy_list_to_consarray(rest, stack_rest);
     kiss_cons_t args;
     kiss_init_cons(&args, list1, (kiss_obj*)stack_rest);
     for (kiss_obj* x = (kiss_obj*)&args; KISS_IS_CONS(x); x = KISS_CDR(x))
          Kiss_List(KISS_CAR(x));
     kiss_cons_t result;
     kiss_init_cons(&result, KISS_NIL, KISS_NIL);
     kiss_obj* p = (kiss_obj*)&result;
     if (kiss_member(KISS_NIL, (kiss_obj*)&args) != KISS_NIL) { return KISS_NIL; }
     while(1) {
          kiss_set_cdr(kiss_cons(kiss_funcall(function, (kiss_obj*)&args), KISS_NIL), p);
          p = KISS_CDR(p);
          for (kiss_obj* q = (kiss_obj*)&args; KISS_IS_CONS(q); q = KISS_CDR(q)) {
               kiss_obj* obj = KISS_CDR(KISS_CAR(q));
               if (!KISS_IS_CONS(obj)) {
                    goto end;
               }
               kiss_set_car(obj, q);
          }
     }
end:
     return result.cdr;
}

/* function: (mapcon function list+) -> <list>
   mapcon  are like maplist, except that the results of applying function are combined
   into a list by the use of an operation that performs a destructive form of append
   rather than list. */
inline
kiss_obj* kiss_mapcon(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest)
{
     return kiss_nconc(kiss_maplist(function, list1, rest));
}

inline
kiss_obj* kiss_mapl1(const kiss_obj* const function, const kiss_obj* const list) {
     for (const kiss_obj* q = list; KISS_IS_CONS(q); q = KISS_CDR(q)) {
          kiss_funcall(function, kiss_cons(q, KISS_NIL));
     }
     return (kiss_obj*)list;
}

inline
kiss_obj* kiss_mapl(const kiss_obj* const function, const kiss_obj* const list1, const kiss_obj* const rest)
{
     size_t n = kiss_c_length(rest);
     if (n == 0) { return kiss_mapl1(function, list1); }
     kiss_cons_t stack_rest[n];
     kiss_copy_list_to_consarray(rest, stack_rest);
     kiss_cons_t args;
     kiss_init_cons(&args, list1, (kiss_obj*)stack_rest);
     for (kiss_obj* x = (kiss_obj*)&args; KISS_IS_CONS(x); x = KISS_CDR(x))
          Kiss_List(KISS_CAR(x));
     if (kiss_member(KISS_NIL, (kiss_obj*)&args) != KISS_NIL) { return (kiss_obj*)list1; }
     while(1) {
          kiss_funcall(function, (kiss_obj*)&args);
          for (kiss_obj* q = (kiss_obj*)&args; KISS_IS_CONS(q); q = KISS_CDR(q)) {
               kiss_obj* obj = KISS_CDR(KISS_CAR(q));
               if (!KISS_IS_CONS(obj)) {
                    goto end;
               }
               kiss_set_car(obj, q);
          }
     }
end:
     return (kiss_obj*)list1;
}

kiss_symbol_t KISS_Ssignal_condition;

inline
kiss_obj* kiss_eval_compound_form(kiss_cons_t* p) {
     kiss_obj* op = p->car;
     switch (KISS_OBJ_TYPE(op)) {
     case KISS_SYMBOL: {
          if (op == (kiss_obj*)&KISS_Ssignal_condition) {
               kiss_environment_t* env = Kiss_Get_Environment();
               env->error_call_stack = env->call_stack;
          }
	  return kiss_invoke(kiss_fun_ref((kiss_symbol_t*)op), p->cdr);
     }
     case KISS_CONS:
	  return kiss_invoke((kiss_obj*)kiss_make_function(NULL, op), p->cdr);
     default: Kiss_Err(L"eval: Invalid compound expression ~S", p);
     }
}

inline
kiss_obj* kiss_eval(const kiss_obj* const form) {
     switch (KISS_OBJ_TYPE(form)) {
     case KISS_CONS:
          return kiss_eval_compound_form((kiss_cons_t*)Kiss_Proper_List(form));
     case KISS_SYMBOL:
          return kiss_var_ref((kiss_symbol_t*)form);
     default: /* self-evaluating object. */
          return (kiss_obj*)form;
     }
}

inline
kiss_obj* kiss_eval_body(const kiss_obj* const body) {
     kiss_obj* result = KISS_NIL;
     for (const kiss_obj* p = Kiss_Proper_List(body); KISS_IS_CONS(p); p = KISS_CDR(p)) {
	  result = kiss_eval(KISS_CAR(p));
     }
     return result;
}



