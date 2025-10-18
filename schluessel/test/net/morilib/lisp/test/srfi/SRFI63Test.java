/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.lisp.test.srfi;

import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/29
 */
public class SRFI63Test extends TCSubr {

	public void testListToArray() {
		Scheme l = Scheme.newInstance();

		equal(l,"(equal? (list->array 2 '#() '((1 2) (3 4)))" +
				" '#2A((1 2) (3 4)))", T);
		equal(l,"(equal? (list->array 0 '#() 3) #0A 3)", T);
		equal(l,"(equal? (list->array 2 (a:fixz64b) '((1 2) (3 4)))" +
				" '#2A:fixZ64b((1 2) (3 4)))", T);
		lperr(l,"(list->array 2 '#() '(1 2))");
		lperr(l,"(list->array 2 '() '(1 2))");
		lperr(l,"(list->array 2 '#() '((1 2) (2 3 4)))");
		lperr(l,"(list->array 2 '#() '((1 2) 3))");
		lperr(l,"(list->array 2 (a:fixz8b) '((1 2) (3 128)))");
	}

	public void testArrayToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(array->list '#2A((1 2) (3 4)))",
				list(list(1, 2), list(3, 4)));
		equal(l,"(array->list '#2A:fixZ32b((1 2) (3 4)))",
				list(list(1, 2), list(3, 4)));
	}

	public void testVectorToArray() {
		Scheme l = Scheme.newInstance();

		equal(l,"(equal? (vector->array '#(1 2 3 4) '#() 2 2)" +
				" '#2A((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:flor128b) 2 2)" +
				" '#2A:floR64b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:flor64b) 2 2)" +
				" '#2A:floR64b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:flor32b) 2 2)" +
				" '#2A:floR32b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:flor16b) 2 2)" +
				" '#2A:floR32b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:floc128b) 2 2)" +
				" '#2A:floC64b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:floc64b) 2 2)" +
				" '#2A:floC64b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:floc32b) 2 2)" +
				" '#2A:floC32b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:floc16b) 2 2)" +
				" '#2A:floC32b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:floq128d) 2 2)" +
				" '#2A((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:floq64d) 2 2)" +
				" '#2A((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:floq32d) 2 2)" +
				" '#2A((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:fixz64b) 2 2)" +
				" '#2A:fixZ64b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:fixz32b) 2 2)" +
				" '#2A:fixZ32b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:fixz16b) 2 2)" +
				" '#2A:fixZ16b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:fixz8b) 2 2)" +
				" '#2A:fixZ8b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:fixn64b) 2 2)" +
				" '#2A:fixN64b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:fixn32b) 2 2)" +
				" '#2A:fixN32b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:fixn16b) 2 2)" +
				" '#2A:fixN16b((1 2) (3 4)))", T);
		equal(l,"(equal? (vector->array '#(1 2 3 4) (a:fixn8b) 2 2)" +
				" '#2A:fixN8b((1 2) (3 4)))", T);
		System.out.println(LispUtils.print(l.exec("(vector->array '#(#t #f #t #f) (a:bool) 2 2)")));
		equal(l,"(equal? (vector->array '#(#t #f #t #f) (a:bool) 2 2)" +
				" '#2A:bool((#t #f) (#t #f)))", T);
		equal(l,"(equal? (vector->array '#(#\\1 #\\2 #\\3 #\\4) \"\" 2 2)" +
				" '#2A:char((#\\1 #\\2) (#\\3 #\\4)))", T);
		lperr(l,"(vector->array '#(a 2) (a:floc128b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:floc64b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:floc32b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:floc16b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:flor128b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:flor64b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:flor32b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:flor16b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:fixz64b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:fixz32b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:fixz16b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:fixz8b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:fixn64b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:fixn32b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:fixn16b) 2 1)");
		lperr(l,"(vector->array '#(a 2) (a:fixn8b) 2 1)");
		lperr(l,"(vector->array '#(9223372036854775808 2) (a:fixz64b) 2 1)");
		lperr(l,"(vector->array '#(2147483648 2) (a:fixz32b) 2 1)");
		lperr(l,"(vector->array '#(32768 2) (a:fixz16b) 2 1)");
		lperr(l,"(vector->array '#(128 2) (a:fixz8b) 2 1)");
		lperr(l,"(vector->array '#(#x10000000000000000 2) (a:fixn64b) 2 1)");
		lperr(l,"(vector->array '#(#x100000000 2) (a:fixn32b) 2 1)");
		lperr(l,"(vector->array '#(#x10000 2) (a:fixn16b) 2 1)");
		lperr(l,"(vector->array '#(#x100 2) (a:fixn8b) 2 1)");
		lperr(l,"(vector->array '#(-9223372036854775809 2) (a:fixz64b) 2 1)");
		lperr(l,"(vector->array '#(-2147483649 2) (a:fixz32b) 2 1)");
		lperr(l,"(vector->array '#(-32769 2) (a:fixz16b) 2 1)");
		lperr(l,"(vector->array '#(-129 2) (a:fixz8b) 2 1)");
		lperr(l,"(vector->array '#(-1 2) (a:fixn64b) 2 1)");
		lperr(l,"(vector->array '#(-1 2) (a:fixn32b) 2 1)");
		lperr(l,"(vector->array '#(-1 2) (a:fixn16b) 2 1)");
		lperr(l,"(vector->array '#(-1 2) (a:fixn8b) 2 1)");
	}

	public void testArrayToVector() {
		Scheme l = Scheme.newInstance();

		equal(l,"(array->vector '#2A((1 2) (3 4)))",
				vec(list(1, 2), list(3, 4)));
		equal(l,"(array->vector '#2A:fixZ32b((1 2) (3 4)))",
				vec(list(1, 2), list(3, 4)));
	}

}
