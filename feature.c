/*  -*- coding: utf-8 -*-
  feature.c --- defines the feature mechanism of ISLisp processor KISS.

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

kiss_obj* kiss_featurep(kiss_obj* feature) {
     kiss_environment_t* env = Kiss_Get_Environment();
     if (kiss_member(feature, env->features) == KISS_NIL) {
	  return KISS_NIL;
     } else {
	  return KISS_T;
     }
}

kiss_obj* kiss_provide(kiss_obj* feature) {
     kiss_environment_t* env = Kiss_Get_Environment();
     if (kiss_featurep(feature) == KISS_NIL) {
	  env->features = kiss_cons(feature, env->features);
     }
     return feature;
}
