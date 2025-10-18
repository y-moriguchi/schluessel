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
import net.morilib.lisp.uvector.LispU16Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class U16VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testU16Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(u16vector? #u16(1 2))", T);
		eq(l, "(u16vector? #f)", F);
		eq(l, "(u16vector? 'a)", F);
		eq(l, "(u16vector? #\\a)", F);
		eq(l, "(u16vector? #(1 2))", F);
		eq(l, "(u16vector? vector?)", F);
		eq(l, "(u16vector? '(1 2))", F);
		eq(l, "(u16vector? 1)", F);
		eq(l, "(u16vector? \"aaa\")", F);
		eq(l, "(u16vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(u16vector 1 2 65535)",
				new LispU16Vector((short)1, (short)2, (short)65535));
		equal(l, "(u16vector)", new LispU16Vector());
		lperr(l, "(u16vector -1)");
		lperr(l, "(u16vector 65536)");
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-u16vector 3 65535)",
				new LispU16Vector((short)65535, (short)65535, (short)65535));
		equal(l, "(make-u16vector 0 1)", new LispU16Vector());
		equal(l, "(make-u16vector 0)", new LispU16Vector());
		lperr(l, "(make-u16vector 3 65536)");
		lperr(l, "(make-u16vector 4.0)");
		lperr(l, "(make-u16vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(u16vector-length '#u16(1 2 3 4))", 4);
		eqi(l, "(u16vector-length #u16())", 0);
		lperr(l, "(u16vector-length '(1 2 3 4))");
		lperr(l, "(u16vector-length '())");
		lperr(l, "(u16vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(u16vector-ref '#u16(1 2 65535 4) 2)", 65535);
		eqi(l, "(u16vector-ref '#u16(1 2 3 4) 0)", 1);
		eqi(l, "(u16vector-ref '#u16(1 2 3 4) 3)", 4);
		lperr(l, "(u16vector-ref '#u16(1 2 3 4) 4)");
		lperr(l, "(u16vector-ref '#u16(1 2 3 4) -1)");
		lperr(l, "(u16vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(u16vector-ref '#u16(1 2 3 4) 1.0)");
		lperr(l, "(u16vector-ref '#u16(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#u16(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(u16vector-set! v 1 65535)");
		equal(l.get("v"), new LispU16Vector(
				(short)1, (short)65535, (short)3, (short)4));
		lperr(l, "(u16vector-set! v 1 -1)");
		lperr(l, "(u16vector-set! v 1 65536)");
		lperr(l, "(u16vector-set! v -1 5)");
		lperr(l, "(u16vector-set! v 4 5)");
		lperr(l, "(u16vector-set! v 1.0 5)");
		lperr(l, "(u16vector-set! l 1 5)");
		lperr(l, "(u16vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(u16vector->list '#u16(1 2 3 65535))",
				list(1, 2, 3, 65535));
		equal(l, "(u16vector->list '#u16())", Nil.NIL);
		lperr(l, "(u16vector->list 1)");
		lperr(l, "(u16vector->list '(1 2 3 4))");
		lperr(l, "(u16vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->u16vector '(1 2 3 65535))",new LispU16Vector(
				(short)1, (short)2, (short)3, (short)65535));
		equal(l, "(list->u16vector '())", new LispU16Vector());
		lperr(l, "(list->u16vector '(-1))");
		lperr(l, "(list->u16vector '(65536))");
		lperr(l, "(list->u16vector 1)");
		lperr(l, "(list->u16vector '#(1 2 3 4))");
		lperr(l, "(list->u16vector '(1 2 3 4 . 5))");
		lperr(l, "(list->u16vector)");
	}

}
