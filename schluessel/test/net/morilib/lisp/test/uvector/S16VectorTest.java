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
import net.morilib.lisp.uvector.LispS16Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class S16VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testS16Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(s16vector? #s16(1 2))", T);
		eq(l, "(s16vector? #f)", F);
		eq(l, "(s16vector? 'a)", F);
		eq(l, "(s16vector? #\\a)", F);
		eq(l, "(s16vector? #(1 2))", F);
		eq(l, "(s16vector? vector?)", F);
		eq(l, "(s16vector? '(1 2))", F);
		eq(l, "(s16vector? 1)", F);
		eq(l, "(s16vector? \"aaa\")", F);
		eq(l, "(s16vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(s16vector 1 2 3)",
				new LispS16Vector((short)1, (short)2, (short)3));
		equal(l, "(s16vector)", new LispS16Vector());
		lperr(l, "(s16vector -32769)");
		lperr(l, "(s16vector 32768)");
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-s16vector 4 1)",
				new LispS16Vector((short)1, (short)1, (short)1, (short)1));
		equal(l, "(make-s16vector 0 1)", new LispS16Vector());
		equal(l, "(make-s16vector 0)", new LispS16Vector());
		lperr(l, "(make-s16vector 3 32768)");
		lperr(l, "(make-s16vector 4.0)");
		lperr(l, "(make-s16vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(s16vector-length '#s16(1 2 3 4))", 4);
		eqi(l, "(s16vector-length #s16())", 0);
		lperr(l, "(s16vector-length '(1 2 3 4))");
		lperr(l, "(s16vector-length '())");
		lperr(l, "(s16vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(s16vector-ref '#s16(1 2 3 4) 2)", 3);
		eqi(l, "(s16vector-ref '#s16(1 2 3 4) 0)", 1);
		eqi(l, "(s16vector-ref '#s16(1 2 3 4) 3)", 4);
		lperr(l, "(s16vector-ref '#s16(1 2 3 4) 4)");
		lperr(l, "(s16vector-ref '#s16(1 2 3 4) -1)");
		lperr(l, "(s16vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(s16vector-ref '#s16(1 2 3 4) 1.0)");
		lperr(l, "(s16vector-ref '#s16(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#s16(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(s16vector-set! v 1 5)");
		equal(l.get("v"), new LispS16Vector(
				(short)1, (short)5, (short)3, (short)4));
		lperr(l, "(s16vector-set! v 1 -32769)");
		lperr(l, "(s16vector-set! v 1 32768)");
		lperr(l, "(s16vector-set! v -1 5)");
		lperr(l, "(s16vector-set! v 4 5)");
		lperr(l, "(s16vector-set! v 1.0 5)");
		lperr(l, "(s16vector-set! l 1 5)");
		lperr(l, "(s16vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(s16vector->list '#s16(1 2 3 4))", list(1, 2, 3, 4));
		equal(l, "(s16vector->list '#s16())", Nil.NIL);
		lperr(l, "(s16vector->list 1)");
		lperr(l, "(s16vector->list '(1 2 3 4))");
		lperr(l, "(s16vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->s16vector '(1 2 3 4))",new LispS16Vector(
				(short)1, (short)2, (short)3, (short)4));
		equal(l, "(list->s16vector '())", new LispS16Vector());
		lperr(l, "(list->s16vector '(-32769))");
		lperr(l, "(list->s16vector '(32768))");
		lperr(l, "(list->s16vector 1)");
		lperr(l, "(list->s16vector '#(1 2 3 4))");
		lperr(l, "(list->s16vector '(1 2 3 4 . 5))");
		lperr(l, "(list->s16vector)");
	}

}
