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
import net.morilib.lisp.uvector.LispS8Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class S8VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testS8Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(s8vector? #s8(1 2))", T);
		eq(l, "(s8vector? #f)", F);
		eq(l, "(s8vector? 'a)", F);
		eq(l, "(s8vector? #\\a)", F);
		eq(l, "(s8vector? #(1 2))", F);
		eq(l, "(s8vector? vector?)", F);
		eq(l, "(s8vector? '(1 2))", F);
		eq(l, "(s8vector? 1)", F);
		eq(l, "(s8vector? \"aaa\")", F);
		eq(l, "(s8vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(s8vector 1 2 3)",
				new LispS8Vector((byte)1, (byte)2, (byte)3));
		equal(l, "(s8vector)", new LispS8Vector());
		lperr(l, "(s8vector -129)");
		lperr(l, "(s8vector 128)");
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-s8vector 4 1)",
				new LispS8Vector((byte)1, (byte)1, (byte)1, (byte)1));
		equal(l, "(make-s8vector 0 1)", new LispS8Vector());
		equal(l, "(make-s8vector 0)", new LispS8Vector());
		lperr(l, "(make-s8vector 3 128)");
		lperr(l, "(make-s8vector 4.0)");
		lperr(l, "(make-s8vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(s8vector-length '#s8(1 2 3 4))", 4);
		eqi(l, "(s8vector-length #s8())", 0);
		lperr(l, "(s8vector-length '(1 2 3 4))");
		lperr(l, "(s8vector-length '())");
		lperr(l, "(s8vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(s8vector-ref '#s8(1 2 3 4) 2)", 3);
		eqi(l, "(s8vector-ref '#s8(1 2 3 4) 0)", 1);
		eqi(l, "(s8vector-ref '#s8(1 2 3 4) 3)", 4);
		lperr(l, "(s8vector-ref '#s8(1 2 3 4) 4)");
		lperr(l, "(s8vector-ref '#s8(1 2 3 4) -1)");
		lperr(l, "(s8vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(s8vector-ref '#s8(1 2 3 4) 1.0)");
		lperr(l, "(s8vector-ref '#s8(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#s8(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(s8vector-set! v 1 5)");
		equal(l.get("v"), new LispS8Vector(
				(byte)1, (byte)5, (byte)3, (byte)4));
		lperr(l, "(s8vector-set! v 1 -129)");
		lperr(l, "(s8vector-set! v 1 128)");
		lperr(l, "(s8vector-set! v -1 5)");
		lperr(l, "(s8vector-set! v 4 5)");
		lperr(l, "(s8vector-set! v 1.0 5)");
		lperr(l, "(s8vector-set! l 1 5)");
		lperr(l, "(s8vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(s8vector->list '#s8(1 2 3 4))", list(1, 2, 3, 4));
		equal(l, "(s8vector->list '#s8())", Nil.NIL);
		lperr(l, "(s8vector->list 1)");
		lperr(l, "(s8vector->list '(1 2 3 4))");
		lperr(l, "(s8vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->s8vector '(1 2 3 4))",new LispS8Vector(
				(byte)1, (byte)2, (byte)3, (byte)4));
		equal(l, "(list->s8vector '())", new LispS8Vector());
		lperr(l, "(list->s8vector '(-129))");
		lperr(l, "(list->s8vector '(128))");
		lperr(l, "(list->s8vector 1)");
		lperr(l, "(list->s8vector '#(1 2 3 4))");
		lperr(l, "(list->s8vector '(1 2 3 4 . 5))");
		lperr(l, "(list->s8vector)");
	}

}
