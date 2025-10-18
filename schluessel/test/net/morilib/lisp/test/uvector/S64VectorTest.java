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
package net.morilib.lisp.test.uvector;

import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.LispS64Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class S64VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testS64Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(s64vector? #s64(1 2))", T);
		eq(l, "(s64vector? #f)", F);
		eq(l, "(s64vector? 'a)", F);
		eq(l, "(s64vector? #\\a)", F);
		eq(l, "(s64vector? #(1 2))", F);
		eq(l, "(s64vector? vector?)", F);
		eq(l, "(s64vector? '(1 2))", F);
		eq(l, "(s64vector? 1)", F);
		eq(l, "(s64vector? \"aaa\")", F);
		eq(l, "(s64vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(s64vector 1 2 3)",
				new LispS64Vector((long)1, (long)2, (long)3));
		equal(l, "(s64vector)", new LispS64Vector());
		lperr(l, "(s64vector -9223372036854775809)");
		lperr(l, "(s64vector 9223372036854775808)");
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-s64vector 4 1)",
				new LispS64Vector((long)1, (long)1, (long)1, (long)1));
		equal(l, "(make-s64vector 0 1)", new LispS64Vector());
		equal(l, "(make-s64vector 0)", new LispS64Vector());
		lperr(l, "(make-s64vector 3 9223372036854775808)");
		lperr(l, "(make-s64vector 4.0)");
		lperr(l, "(make-s64vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(s64vector-length '#s64(1 2 3 4))", 4);
		eqi(l, "(s64vector-length #s64())", 0);
		lperr(l, "(s64vector-length '(1 2 3 4))");
		lperr(l, "(s64vector-length '())");
		lperr(l, "(s64vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(s64vector-ref '#s64(1 2 3 4) 2)", 3);
		eqi(l, "(s64vector-ref '#s64(1 2 3 4) 0)", 1);
		eqi(l, "(s64vector-ref '#s64(1 2 3 4) 3)", 4);
		lperr(l, "(s64vector-ref '#s64(1 2 3 4) 4)");
		lperr(l, "(s64vector-ref '#s64(1 2 3 4) -1)");
		lperr(l, "(s64vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(s64vector-ref '#s64(1 2 3 4) 1.0)");
		lperr(l, "(s64vector-ref '#s64(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#s64(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(s64vector-set! v 1 5)");
		equal(l.get("v"), new LispS64Vector(
				(long)1, (long)5, (long)3, (long)4));
		lperr(l, "(s64vector-set! v 1 -9223372036854775809)");
		lperr(l, "(s64vector-set! v 1 9223372036854775808)");
		lperr(l, "(s64vector-set! v -1 5)");
		lperr(l, "(s64vector-set! v 4 5)");
		lperr(l, "(s64vector-set! v 1.0 5)");
		lperr(l, "(s64vector-set! l 1 5)");
		lperr(l, "(s64vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(s64vector->list '#s64(1 2 3 4))", list(1, 2, 3, 4));
		equal(l, "(s64vector->list '#s64())", Nil.NIL);
		lperr(l, "(s64vector->list 1)");
		lperr(l, "(s64vector->list '(1 2 3 4))");
		lperr(l, "(s64vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->s64vector '(1 2 3 4))",new LispS64Vector(
				(long)1, (long)2, (long)3, (long)4));
		equal(l, "(list->s64vector '())", new LispS64Vector());
		lperr(l, "(list->s64vector '(-9223372036854775809))");
		lperr(l, "(list->s64vector '(9223372036854775808))");
		lperr(l, "(list->s64vector 1)");
		lperr(l, "(list->s64vector '#(1 2 3 4))");
		lperr(l, "(list->s64vector '(1 2 3 4 . 5))");
		lperr(l, "(list->s64vector)");
	}

}
