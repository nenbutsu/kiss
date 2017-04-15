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

static kiss_obj* Kiss_Features = KISS_NIL;

kiss_obj* kiss_featurep(kiss_obj* feature) {
    if (kiss_member(feature, Kiss_Features) == KISS_NIL) {
	return KISS_NIL;
    } else {
	return KISS_T;
    }
}

kiss_obj* kiss_provide(kiss_obj* feature) {
    if (kiss_featurep(feature) == KISS_NIL) {
	Kiss_Features = kiss_cons(feature, Kiss_Features);
    }
    return feature;
}
