/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 */
/**
 * A header file forming the interface for the CBA test cases.
 */

#pragma once

#include <assert.h>
#include <string>

// -- standard analysis --

// alias tests
void cba_expect_is_alias(void* a, void* b)  { assert(a==b); };
void cba_expect_not_alias(void* a, void* b) { assert(a!=b); };
void cba_expect_may_alias(void* a, void* b) {};

// integer tests
void cba_expect_undefined_int(int a)     {};                  // = is universe
void cba_expect_defined_int(int a)       {};                  // = is not empty and not universe
void cba_expect_single_int(int a)        {};                  // = is a single value
void cba_expect_eq_int(int a, int b)     { assert(a==b); };
void cba_expect_ne_int(int a, int b)     { assert(a!=b); };
void cba_expect_may_eq_int(int a, int b) {};

// -- allscale specific analysis --

// assertions on the data requirements
void cba_expect_data_requirements(const char* region)  { /* nothing */ };

// debugging
void cba_print_code() {};
void cba_print_int(int a) {};
void cba_dump_json() {};
void cba_dump_statistic() {};
void cba_dump_solution() {};

// debugging specifically for data requirements
void cba_print_scope() {};
void cba_dump_scope_json() {};
void cba_dump_scope_solution() {};


#define cba_debug() cba_print_code(); cba_dump_json(); cba_dump_solution();

#define cba_debug_requirements() cba_print_scope(); cba_dump_scope_json(); cba_dump_scope_solution();
