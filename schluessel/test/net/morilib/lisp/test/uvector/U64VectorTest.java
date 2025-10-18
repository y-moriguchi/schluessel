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

import java.math.BigInteger;

import net.morilib.lisp.LispInteger;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.LispU64Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class U64VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testU64Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(u64vector? #u64(1 2))", T);
		eq(l, "(u64vector? #f)", F);
		eq(l, "(u64vector? 'a)", F);
		eq(l, "(u64vector? #\\a)", F);
		eq(l, "(u64vector? #(1 2))", F);
		eq(l, "(u64vector? vector?)", F);
		eq(l, "(u64vector? '(1 2))", F);
		eq(l, "(u64vector? 1)", F);
		eq(l, "(u64vector? \"aaa\")", F);
		eq(l, "(u64vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(u64vector 1 2 #xffffffffffffffff)",
				new LispU64Vector((int)1, (int)2, 0xffffffffffffffffl));
		equal(l, "(u64vector)", new LispU64Vector());
		lperr(l, "(u64vector -1)");
		lperr(l, "(u64vector #x10000000000000000)");
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-u64vector 3 #xffffffffffffffff)",
				new LispU64Vector(0xffffffffffffffffl,
						0xffffffffffffffffl, 0xffffffffffffffffl));
		equal(l, "(make-u64vector 0 1)", new LispU64Vector());
		equal(l, "(make-u64vector 0)", new LispU64Vector());
		lperr(l, "(make-u64vector 3 #x10000000000000000)");
		lperr(l, "(make-u64vector 4.0)");
		lperr(l, "(make-u64vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(u64vector-length '#u64(1 2 3 4))", 4);
		eqi(l, "(u64vector-length #u64())", 0);
		lperr(l, "(u64vector-length '(1 2 3 4))");
		lperr(l, "(u64vector-length '())");
		lperr(l, "(u64vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqv(l, "(u64vector-ref '#u64(1 2 #xffffffffffffffff 4) 2)",
				LispInteger.valueOf(new BigInteger(
						"ffffffffffffffff", 16)));
		eqi(l, "(u64vector-ref '#u64(1 2 3 4) 0)", 1);
		eqi(l, "(u64vector-ref '#u64(1 2 3 4) 3)", 4);
		lperr(l, "(u64vector-ref '#u64(1 2 3 4) 4)");
		lperr(l, "(u64vector-ref '#u64(1 2 3 4) -1)");
		lperr(l, "(u64vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(u64vector-ref '#u64(1 2 3 4) 1.0)");
		lperr(l, "(u64vector-ref '#u64(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#u64(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(u64vector-set! v 1 #xffffffffffffffff)");
		equal(l.get("v"), new LispU64Vector(
				(int)1, 0xffffffffffffffffl, (int)3, (int)4));
		lperr(l, "(u64vector-set! v 1 -1)");
		lperr(l, "(u64vector-set! v 1 #x10000000000000000)");
		lperr(l, "(u64vector-set! v -1 5)");
		lperr(l, "(u64vector-set! v 4 5)");
		lperr(l, "(u64vector-set! v 1.0 5)");
		lperr(l, "(u64vector-set! l 1 5)");
		lperr(l, "(u64vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(u64vector->list '#u64(1 2 3 #xffffffffffffffff))",
				list(1, 2, 3, LispInteger.valueOf(new BigInteger(
						"ffffffffffffffff", 16))));
		equal(l, "(u64vector->list '#u64())", Nil.NIL);
		lperr(l, "(u64vector->list 1)");
		lperr(l, "(u64vector->list '(1 2 3 4))");
		lperr(l, "(u64vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->u64vector '(1 2 3 #xffffffffffffffff))",new LispU64Vector(
				(int)1, (int)2, (int)3, 0xffffffffffffffffl));
		equal(l, "(list->u64vector '())", new LispU64Vector());
		lperr(l, "(list->u64vector '(-1))");
		lperr(l, "(list->u64vector '(#x10000000000000000))");
		lperr(l, "(list->u64vector 1)");
		lperr(l, "(list->u64vector '#(1 2 3 4))");
		lperr(l, "(list->u64vector '(1 2 3 4 . 5))");
		lperr(l, "(list->u64vector)");
	}

}
