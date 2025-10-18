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
import net.morilib.lisp.uvector.LispS32Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class S32VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testS32Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(s32vector? #s32(1 2))", T);
		eq(l, "(s32vector? #f)", F);
		eq(l, "(s32vector? 'a)", F);
		eq(l, "(s32vector? #\\a)", F);
		eq(l, "(s32vector? #(1 2))", F);
		eq(l, "(s32vector? vector?)", F);
		eq(l, "(s32vector? '(1 2))", F);
		eq(l, "(s32vector? 1)", F);
		eq(l, "(s32vector? \"aaa\")", F);
		eq(l, "(s32vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(s32vector 1 2 3)",
				new LispS32Vector((int)1, (int)2, (int)3));
		equal(l, "(s32vector)", new LispS32Vector());
		lperr(l, "(s32vector −2147483649)");
		lperr(l, "(s32vector 2147483648)");
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-s32vector 4 1)",
				new LispS32Vector((int)1, (int)1, (int)1, (int)1));
		equal(l, "(make-s32vector 0 1)", new LispS32Vector());
		equal(l, "(make-s32vector 0)", new LispS32Vector());
		lperr(l, "(make-s32vector 3 2147483648)");
		lperr(l, "(make-s32vector 4.0)");
		lperr(l, "(make-s32vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(s32vector-length '#s32(1 2 3 4))", 4);
		eqi(l, "(s32vector-length #s32())", 0);
		lperr(l, "(s32vector-length '(1 2 3 4))");
		lperr(l, "(s32vector-length '())");
		lperr(l, "(s32vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(s32vector-ref '#s32(1 2 3 4) 2)", 3);
		eqi(l, "(s32vector-ref '#s32(1 2 3 4) 0)", 1);
		eqi(l, "(s32vector-ref '#s32(1 2 3 4) 3)", 4);
		lperr(l, "(s32vector-ref '#s32(1 2 3 4) 4)");
		lperr(l, "(s32vector-ref '#s32(1 2 3 4) -1)");
		lperr(l, "(s32vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(s32vector-ref '#s32(1 2 3 4) 1.0)");
		lperr(l, "(s32vector-ref '#s32(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#s32(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(s32vector-set! v 1 5)");
		equal(l.get("v"), new LispS32Vector(
				(int)1, (int)5, (int)3, (int)4));
		lperr(l, "(s32vector-set! v 1 −2147483649)");
		lperr(l, "(s32vector-set! v 1 2147483648)");
		lperr(l, "(s32vector-set! v -1 5)");
		lperr(l, "(s32vector-set! v 4 5)");
		lperr(l, "(s32vector-set! v 1.0 5)");
		lperr(l, "(s32vector-set! l 1 5)");
		lperr(l, "(s32vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(s32vector->list '#s32(1 2 3 4))", list(1, 2, 3, 4));
		equal(l, "(s32vector->list '#s32())", Nil.NIL);
		lperr(l, "(s32vector->list 1)");
		lperr(l, "(s32vector->list '(1 2 3 4))");
		lperr(l, "(s32vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->s32vector '(1 2 3 4))",new LispS32Vector(
				(int)1, (int)2, (int)3, (int)4));
		equal(l, "(list->s32vector '())", new LispS32Vector());
		lperr(l, "(list->s32vector '(−2147483649))");
		lperr(l, "(list->s32vector '(2147483648))");
		lperr(l, "(list->s32vector 1)");
		lperr(l, "(list->s32vector '#(1 2 3 4))");
		lperr(l, "(list->s32vector '(1 2 3 4 . 5))");
		lperr(l, "(list->s32vector)");
	}

}
