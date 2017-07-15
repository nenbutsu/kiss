/*  -*- coding: utf-8 -*-
  number.c --- defines the number handling mechanism of ISLisp processor KISS.

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

kiss_integer_t* kiss_make_integer(long int i) {
    kiss_integer_t* p = Kiss_GC_Malloc(sizeof(kiss_integer_t));
    p->type = KISS_INTEGER;
    p->i = i;
    return p;
}

kiss_float_t* kiss_make_float(float f) {
    kiss_float_t* p = Kiss_GC_Malloc(sizeof(kiss_float_t));
    p->type = KISS_FLOAT;
    p->f = f;
    return p;
}

/* function: (integeerp obj) -> boolean
   Returns t if obj is an integer otherwise, returns nil.*/
kiss_obj* kiss_integerp(kiss_obj* obj) {
    if (KISS_IS_INTEGER(obj)) { return KISS_T; }
    else                      { return KISS_NIL; }
}

/* function: (floatp obj) -> boolean
   Returns t if obj is an float otherwise, returns nil.*/
kiss_obj* kiss_floatp(kiss_obj* obj) {
    if (KISS_IS_FLOAT(obj)) { return KISS_T; }
    else                    { return KISS_NIL; }
}

/* function: (float x) -> <float>
   Returns x itself if it is an instance of the class <float> and returns 
   a floating-point approximation of x otherwise. An error shall be signaled if
   x is not a number (error-id. domain-error).
 */
kiss_obj* kiss_float(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_float(Kiss_Integer(x)->i);
     } else {
	  return x;
     }
}

static int kiss_all_int(kiss_obj* p) {
     while (KISS_IS_CONS(p)) {
	  if (!KISS_IS_INTEGER(KISS_CAR(p))) { return 0; }
	  p = KISS_CDR(p);
     }
     return 1;
}



/* function: (parse-number string) -> <number>
   The characters belonging to string are scanned (as if by read) and if the resulting
   lexeme is the textual representation of a number, the number it represents is returned.
   An error shall be signaled if string is not a string (error-id. domain-error).
   An error shall be signaled if string is not the textual representation of a number
   (error-id. cannot-parse-number)
*/
kiss_obj* kiss_parse_number(kiss_obj* p) {
     kiss_string_t* str = Kiss_String(p);
     wchar_t* tailptr;
     long i = wcstol(str->str, &tailptr, 10);
     if (tailptr == str->str + str->n) { return (kiss_obj*)kiss_make_integer(i); }
     else {
	  float f = wcstof(str->str, &tailptr);
	  if (tailptr == str->str + str->n) { return (kiss_obj*)kiss_make_float(f); }
	  else { Kiss_Cannot_Parse_Number_Error(p); }
     }
}


/* function: (+ x*) -> <number> 
   The function + returns the sum of its arguments. 
   If all arguments are integers, the result is an integer.
   If any argument is a float, the result is a float.
   When given no arguments, + returns 0. 
   An error shall be signaled if any x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_Lplus(kiss_obj* p) {
     if (kiss_all_int(p)) {
	  long int sum = 0;
	  while (KISS_IS_CONS(p)) {
	       kiss_integer_t* x = Kiss_Integer(KISS_CAR(p));
	       sum += x->i;
	       p = KISS_CDR(p);
	  }
	  return (kiss_obj*)kiss_make_integer(sum);
     } else {
	  float sum = 0.0;
	  while (KISS_IS_CONS(p)) {
	       kiss_obj* obj = Kiss_Number(KISS_CAR(p));
	       if (KISS_IS_INTEGER(obj)) {
		    sum += ((kiss_integer_t*)obj)->i;
	       } else {
		    sum += ((kiss_float_t*)obj)->f;
	       }
	       p = KISS_CDR(p);
	  }
	  return (kiss_obj*)kiss_make_float(sum);
     }
}

/* function: (* x*) -> <number> 
   The function * returns the product of its arguments. 
   If all arguments are integers, the result is an integer.
   If any argument is a float, the result is a float.
   When given no arguments, * returns 1. 
   An error shall be signaled if any x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_Lmultiply(kiss_obj* p) {
     if (kiss_all_int(p)) {
	  long int product = 1;
	  while (KISS_IS_CONS(p)) {
	       product *= Kiss_Integer(KISS_CAR(p))->i;
	       p = KISS_CDR(p);
	  }
	  return (kiss_obj*)kiss_make_integer(product);
     } else {
	  float product = 1.0;
	  while (KISS_IS_CONS(p)) {
	       kiss_obj* obj = Kiss_Number(KISS_CAR(p));
	       if (KISS_IS_INTEGER(obj)) {
		    product *= ((kiss_integer_t*)obj)->i;
	       } else {
		    product *= ((kiss_float_t*)obj)->f;
	       }
	       p = KISS_CDR(p);
	  }
	  return (kiss_obj*)kiss_make_float(product);
     }
}

/* function: (- x+) -> <number> 
   Given one argument, x, this function returns its additive inverse.
   An error shall be signaled if x is not a number (error-id. domain-error).
   If an implementation supports a -0.0 that is distinct from 0.0 ,then (- 0.0)
   returns -0.0 ;in implementations where -0.0 and 0.0 are not distinct, (- 0.0)
   returns 0.0.
*/
kiss_obj* kiss_Lminus(kiss_obj* number, kiss_obj* rest) {
     if (kiss_all_int(kiss_cons(number, rest))) {
	  if (rest == KISS_NIL) {
	       /* (- 10) => -10 */
	       return (kiss_obj*)kiss_make_integer( - Kiss_Integer(number)->i);
	  } else {
	       /* (- 10 1 1) => 8 */
	       long int remainder = Kiss_Integer(number)->i;
	       while (KISS_IS_CONS(rest)) {
		    long int i = Kiss_Integer(KISS_CAR(rest))->i;
		    remainder -= i;
		    rest = KISS_CDR(rest);
	       }
	       return (kiss_obj*)kiss_make_integer(remainder);
	  }
     } else {
	  if (rest == KISS_NIL) {
	       return (kiss_obj*)kiss_make_float( - Kiss_Float(number)->f);
	  } else {
	       float remainder = Kiss_Float(number)->f;
	       while (KISS_IS_CONS(rest)) {
		    float f = Kiss_Float(KISS_CAR(rest))->f;
		    remainder -= f;
		    rest = KISS_CDR(rest);
	       }
	       return (kiss_obj*)kiss_make_float(remainder);
	  }
     }
}

/* function: (= x y) -> boolean 
   Returns t if x1 has the same mathematical value as x2 ;otherwise,returns nil.
   An error shall be signaled if either x1 or x2 is not a number (error-id. domain-error).
   Note: = differs from eql because = compares only the mathematical values of its arguments,
   whereas eql also compares the representations. 
*/
kiss_obj* kiss_Lnum_eq(kiss_obj* x, kiss_obj* y) {
     Kiss_Number(x);
     Kiss_Number(y);
     if (KISS_IS_INTEGER(x)) {
	  if (KISS_IS_INTEGER(y)) {
	       return Kiss_Integer(x)->i == Kiss_Integer(y)->i ? KISS_T : KISS_NIL;
	  } else {
	       return Kiss_Integer(x)->i == Kiss_Float(y)->f ? KISS_T : KISS_NIL;
	  }
     } else if (KISS_IS_INTEGER(y)) {
	  return Kiss_Float(x)->f == Kiss_Integer(y)->i ? KISS_T : KISS_NIL;
     } else {
	  return Kiss_Float(x)->f == Kiss_Float(y)->f ? KISS_T : KISS_NIL;
     }
}

/* function: (< x1 x2) -> boolean 
   < returns t if x1 is less than x2.
   The mathematical values of the arguments are compared. 
   An error shall be signaled if either x1 or x2 is not a number (error-id. domain-error).
*/
kiss_obj* kiss_Lnum_lessthan(kiss_obj* x, kiss_obj* y) {
     Kiss_Number(x);
     Kiss_Number(y);
     if (KISS_IS_INTEGER(x)) {
	  if (KISS_IS_INTEGER(y)) {
	       return Kiss_Integer(x)->i < Kiss_Integer(y)->i ? KISS_T : KISS_NIL;
	  } else {
	       return Kiss_Integer(x)->i < Kiss_Float(y)->f ? KISS_T : KISS_NIL;
	  }
     } else if (KISS_IS_INTEGER(y)) {
	  return Kiss_Float(x)->f < Kiss_Integer(y)->i ? KISS_T : KISS_NIL;
     } else {
	  return Kiss_Float(x)->f < Kiss_Float(y)->f ? KISS_T : KISS_NIL;
     }
}

/* function: (div z1 z2) -> <integer>
   div returns the greatest integer less than or equal to the quotient of z1 
   and z2.
   An error shall be signaled if z2 is zero (error-id. division-by-zero).
*/
kiss_obj* kiss_div(kiss_obj* z1, kiss_obj* z2) {
     float f1 = Kiss_Integer(z1)->i;
     float f2 = Kiss_Non_Zero_Integer(z2)->i;
     
     return (kiss_obj*)kiss_make_integer(floorf(f1 / f2));
}

/* function: (mod z1 z2) -> <integer>
   mod returns the remainder of the integer division of z1 by z2.
   The sign of the result is the sign of z2.
   The result lies between 0 (inclusive) and z2 (exclusive),
   and the difference of z1 and this result is divisible by z2 without remainder.
*/
kiss_obj* kiss_mod(kiss_obj* z1, kiss_obj* z2) {
     float f1 = Kiss_Integer(z1)->i;
     float f2 = Kiss_Non_Zero_Integer(z2)->i;
     
     return (kiss_obj*)kiss_make_integer(remainderf(f1, f2));
}

/* function: (gcd z1 z2) -> <integer>
   gcd returns the greatest common divisor of its integer arguments. 
   The result is a non-negative integer. 
   For nonzero arguments the greatest common divisor is the largest integer z 
   such that z1 and z2 are integral multiples of z.
   An error shall be signaled if either z1 or z2 is not an integer 
   (error-id. domain-error).
*/
kiss_obj* kiss_gcd(kiss_obj* z1, kiss_obj* z2) {
     long i1 = Kiss_Integer(z1)->i;
     long i2 = Kiss_Integer(z2)->i;
     long remainder;
     long tmp;

     if (i1 < 0) { i1 = -i1; }
     if (i2 < 0) { i2 = -i2; }

     if (i1 < i2) {
	  tmp = i1;
	  i1 = i2;
	  i2 = tmp;
     }

     assert(i1 >= i2);
     /* Euclidean Algorithm */
     remainder = i2;
     while (remainder) {
	  remainder = i1 % i2;
	  i1 = i2;
	  i2 = remainder;
     }
     return (kiss_obj*)kiss_make_integer(i1);
}

/* function: (lcm z1 z2) -> <integer>
   lcm returns the least common multiple of its integer arguments.
   gcd and lcm satisfies: (= (* (gcd m n) (lcm m n)) (abs (* m n)))
   That is, the evaluation of the above form always return t.
   An error shall be signaled if either z1 or z2 is not an integer
   (error-id. domain-error).
*/
kiss_obj* kiss_lcm(kiss_obj* z1, kiss_obj* z2) {
     long gcd = ((kiss_integer_t*)kiss_gcd(z1, z2))->i;
     long i1 = Kiss_Integer(z1)->i;
     long i2 = Kiss_Integer(z2)->i;
     if (i1 < 0) { i1 = -i1; }
     if (i2 < 0) { i2 = -i2; }
     if (gcd == 0) {
	  return (kiss_obj*)kiss_make_integer(0);
     } else {
	  return (kiss_obj*)kiss_make_integer(gcd * (i1 / gcd) * (i2 / gcd));
     }
}


/* function: (abs x) -> <number>
   The function abs returns the absolute value of its argument.
   An error shall be signaled if x is not a number (error-id. domain-error)
*/
kiss_obj* kiss_abs(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_integer(abs(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_float(fabs(Kiss_Float(x)->f));
     }
}

/* function: (exp x) -> <number>
   Returns e raised to the power x, where e is the base of the natural logarithm.
   An error shall be signaled if x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_exp(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_float(expf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_float(expf(Kiss_Float(x)->f));
     }
}

/* function: (floor x) -> <integer> 
   Returns the greatest integer less than or equal to x.
   Thatis, x is truncated towards negative infinity.
   An error shall be signaled if x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_floor(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_integer(floorf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_integer(floorf(Kiss_Float(x)->f));
     }
}

/* function: (ceiling x) -> <integer> 
   Returns the smallest integer that is not smaller than x. 
   Thatis, x is truncated towards positive infinity.
   An error shall be signaled if x is not a number (error-id. domain-error).   
*/
kiss_obj* kiss_ceiling(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_integer(ceilf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_integer(ceilf(Kiss_Float(x)->f));
     }
}

/* function: (truncate x) -> <integer> 
   Returns the integer between 0 and x(inclusive) that is nearest to x.
   Thatis, x is truncated towards zero.
   An error shall be signaled if x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_truncate(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_integer(truncf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_integer(truncf(Kiss_Float(x)->f));
     }
}

/* function: (round x) -> <integer> 
   Returns the integer nearest to x. If x is exactly halfway between two integers,
   the even one is chosen. 
   An error shall be signaled if x is not a number (error-id. domain-error).
*/
kiss_obj* kiss_round(kiss_obj* x) {
     Kiss_Err(L"c library function 'roundevenf' is not supported as of this writing.");
     /*
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_integer(roundevenf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_integer(roundevenf(Kiss_Float(x)->f));
     }
     */
}



/* function: (log x) -> <number>
   Returns the natural logarithm of x.
   An error shall be signaled if x is not a positive number (error-id. domain-error).
*/
kiss_obj* kiss_log(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_float(logf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_float(logf(Kiss_Float(x)->f));
     }
}

/* function: (sin x) -> <number>
   The function sin returns the sine of x. 
   x must be given in radians.
*/
kiss_obj* kiss_sin(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_float(sinf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_float(sinf(Kiss_Float(x)->f));
     }
}

/* function: (cos x) -> <number>
   The function cos returns the cosine of x. 
   x must be given in radians.
*/
kiss_obj* kiss_cos(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_float(cosf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_float(cosf(Kiss_Float(x)->f));
     }
}

/* function: (tan x) -> <number>
   The function tan returns the tangent of x. 
   x must be given in radians.
*/
kiss_obj* kiss_tan(kiss_obj* x) {
     Kiss_Number(x);
     if (KISS_IS_INTEGER(x)) {
	  return (kiss_obj*)kiss_make_float(tanf(Kiss_Integer(x)->i));
     } else {
	  return (kiss_obj*)kiss_make_float(tanf(Kiss_Float(x)->f));
     }
}
