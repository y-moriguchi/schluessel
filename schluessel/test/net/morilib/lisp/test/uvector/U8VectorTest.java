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
import net.morilib.lisp.uvector.LispU8Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class U8VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testU8Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(u8vector? #u8(1 2))", T);
		eq(l, "(u8vector? #f)", F);
		eq(l, "(u8vector? 'a)", F);
		eq(l, "(u8vector? #\\a)", F);
		eq(l, "(u8vector? #(1 2))", F);
		eq(l, "(u8vector? vector?)", F);
		eq(l, "(u8vector? '(1 2))", F);
		eq(l, "(u8vector? 1)", F);
		eq(l, "(u8vector? \"aaa\")", F);
		eq(l, "(u8vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(u8vector 1 2 255)",
				new LispU8Vector((byte)1, (byte)2, (byte)255));
		equal(l, "(u8vector)", new LispU8Vector());
		lperr(l, "(u8vector -1)");
		lperr(l, "(u8vector 256)");
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-u8vector 3 255)",
				new LispU8Vector((byte)255, (byte)255, (byte)255));
		equal(l, "(make-u8vector 0 1)", new LispU8Vector());
		equal(l, "(make-u8vector 0)", new LispU8Vector());
		lperr(l, "(make-u8vector 3 256)");
		lperr(l, "(make-u8vector 4.0)");
		lperr(l, "(make-u8vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(u8vector-length '#u8(1 2 3 4))", 4);
		eqi(l, "(u8vector-length #u8())", 0);
		lperr(l, "(u8vector-length '(1 2 3 4))");
		lperr(l, "(u8vector-length '())");
		lperr(l, "(u8vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(u8vector-ref '#u8(1 2 255 4) 2)", 255);
		eqi(l, "(u8vector-ref '#u8(1 2 3 4) 0)", 1);
		eqi(l, "(u8vector-ref '#u8(1 2 3 4) 3)", 4);
		lperr(l, "(u8vector-ref '#u8(1 2 3 4) 4)");
		lperr(l, "(u8vector-ref '#u8(1 2 3 4) -1)");
		lperr(l, "(u8vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(u8vector-ref '#u8(1 2 3 4) 1.0)");
		lperr(l, "(u8vector-ref '#u8(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#u8(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(u8vector-set! v 1 255)");
		equal(l.get("v"), new LispU8Vector(
				(byte)1, (byte)255, (byte)3, (byte)4));
		lperr(l, "(u8vector-set! v 1 -1)");
		lperr(l, "(u8vector-set! v 1 256)");
		lperr(l, "(u8vector-set! v -1 5)");
		lperr(l, "(u8vector-set! v 4 5)");
		lperr(l, "(u8vector-set! v 1.0 5)");
		lperr(l, "(u8vector-set! l 1 5)");
		lperr(l, "(u8vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(u8vector->list '#u8(1 2 3 255))",
				list(1, 2, 3, 255));
		equal(l, "(u8vector->list '#u8())", Nil.NIL);
		lperr(l, "(u8vector->list 1)");
		lperr(l, "(u8vector->list '(1 2 3 4))");
		lperr(l, "(u8vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->u8vector '(1 2 3 255))",new LispU8Vector(
				(byte)1, (byte)2, (byte)3, (byte)255));
		equal(l, "(list->u8vector '())", new LispU8Vector());
		lperr(l, "(list->u8vector '(-1))");
		lperr(l, "(list->u8vector '(256))");
		lperr(l, "(list->u8vector 1)");
		lperr(l, "(list->u8vector '#(1 2 3 4))");
		lperr(l, "(list->u8vector '(1 2 3 4 . 5))");
		lperr(l, "(list->u8vector)");
	}

	public void testEqualVector() {
		Scheme l = Scheme.newInstance();

		equal(l,"(u8vector=? #u8(1 2)   #u8(1 2 3))", F);
		equal(l,"(u8vector=? #u8(1 2 3) #u8(1 2))",   F);
		equal(l,"(u8vector=? #u8(1 2 3) #u8(1 2 3))", T);
		equal(l,"(u8vector=? #u8(2 2 3) #u8(1 2 3))", F);
		equal(l,"(u8vector=? #u8(1 2 3) #u8(1 3 3))", F);
	}

	public void testVectorCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(u8vector-compare #u8(1 2)   #u8(1 2 3))", -1);
		eqi  (l,"(u8vector-compare #u8(1 2 3) #u8(1 2))",   1);
		eqi  (l,"(u8vector-compare #u8(1 2 3) #u8(1 2 3))", 0);
		eqi  (l,"(u8vector-compare #u8(2 2 3) #u8(1 2 3))", 1);
		eqi  (l,"(u8vector-compare #u8(1 2 3) #u8(1 3 3))", -1);
	}

	public void testVectorCopyS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a #u8(1 2 3 4 5))");
		l.exec ("(u8vector-copy! #u8(6 7 8 9 0) 1 a 2 3)");
		equal(l,"(u8vector=? a #u8(1 2 7 8 9))", T);
		l.exec ("(u8vector-copy! #u8(6 7 8 9 0) 5 a 2 0)");
		equal(l,"(u8vector=? a #u8(1 2 7 8 9))", T);
		lperr(l,"(u8vector-copy! #u8(1 2) 0 a 0 3)");
		lperr(l,"(u8vector-copy! #u8(1 2) 0 a 4 2)");
		lperr(l,"(u8vector-copy! #u8(1 2) 0 a 5 0)");
		lperr(l,"(u8vector-copy! #u8(1 2) -1 a 0 1)");
		lperr(l,"(u8vector-copy! #u8(1 2) 0 a -1 1)");
	}

	public void testVectorCopy() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a #u8(1 2 3 4 5))");
		equal(l,"(u8vector=? (u8vector-copy a) #u8(1 2 3 4 5))", T);
	}

}
