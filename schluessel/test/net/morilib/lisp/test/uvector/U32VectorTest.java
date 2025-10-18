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

import net.morilib.lisp.LispInteger;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.LispU32Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class U32VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testU32Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(u32vector? #u32(1 2))", T);
		eq(l, "(u32vector? #f)", F);
		eq(l, "(u32vector? 'a)", F);
		eq(l, "(u32vector? #\\a)", F);
		eq(l, "(u32vector? #(1 2))", F);
		eq(l, "(u32vector? vector?)", F);
		eq(l, "(u32vector? '(1 2))", F);
		eq(l, "(u32vector? 1)", F);
		eq(l, "(u32vector? \"aaa\")", F);
		eq(l, "(u32vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(u32vector 1 2 #xffffffff)",
				new LispU32Vector((int)1, (int)2, 0xffffffff));
		equal(l, "(u32vector)", new LispU32Vector());
		lperr(l, "(u32vector -1)");
		lperr(l, "(u32vector #x100000000)");
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-u32vector 3 #xffffffff)",
				new LispU32Vector(0xffffffff, 0xffffffff, 0xffffffff));
		equal(l, "(make-u32vector 0 1)", new LispU32Vector());
		equal(l, "(make-u32vector 0)", new LispU32Vector());
		lperr(l, "(make-u32vector 3 #x100000000)");
		lperr(l, "(make-u32vector 4.0)");
		lperr(l, "(make-u32vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(u32vector-length '#u32(1 2 3 4))", 4);
		eqi(l, "(u32vector-length #u32())", 0);
		lperr(l, "(u32vector-length '(1 2 3 4))");
		lperr(l, "(u32vector-length '())");
		lperr(l, "(u32vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqv(l, "(u32vector-ref '#u32(1 2 #xffffffff 4) 2)",
				LispInteger.valueOf(0xffffffffl));
		eqi(l, "(u32vector-ref '#u32(1 2 3 4) 0)", 1);
		eqi(l, "(u32vector-ref '#u32(1 2 3 4) 3)", 4);
		lperr(l, "(u32vector-ref '#u32(1 2 3 4) 4)");
		lperr(l, "(u32vector-ref '#u32(1 2 3 4) -1)");
		lperr(l, "(u32vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(u32vector-ref '#u32(1 2 3 4) 1.0)");
		lperr(l, "(u32vector-ref '#u32(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#u32(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(u32vector-set! v 1 #xffffffff)");
		equal(l.get("v"), new LispU32Vector(
				(int)1, 0xffffffff, (int)3, (int)4));
		lperr(l, "(u32vector-set! v 1 -1)");
		lperr(l, "(u32vector-set! v 1 #x100000000)");
		lperr(l, "(u32vector-set! v -1 5)");
		lperr(l, "(u32vector-set! v 4 5)");
		lperr(l, "(u32vector-set! v 1.0 5)");
		lperr(l, "(u32vector-set! l 1 5)");
		lperr(l, "(u32vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(u32vector->list '#u32(1 2 3 #xffffffff))",
				list(1, 2, 3, LispInteger.valueOf(0xffffffffl)));
		equal(l, "(u32vector->list '#u32())", Nil.NIL);
		lperr(l, "(u32vector->list 1)");
		lperr(l, "(u32vector->list '(1 2 3 4))");
		lperr(l, "(u32vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->u32vector '(1 2 3 #xffffffff))",new LispU32Vector(
				(int)1, (int)2, (int)3, 0xffffffff));
		equal(l, "(list->u32vector '())", new LispU32Vector());
		lperr(l, "(list->u32vector '(-1))");
		lperr(l, "(list->u32vector '(#x100000000))");
		lperr(l, "(list->u32vector 1)");
		lperr(l, "(list->u32vector '#(1 2 3 4))");
		lperr(l, "(list->u32vector '(1 2 3 4 . 5))");
		lperr(l, "(list->u32vector)");
	}

}
