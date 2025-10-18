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

import net.morilib.lisp.LispDouble;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.LispF64Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class F64VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testF64Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(f64vector? #f64(1 2))", T);
		eq(l, "(f64vector? #f)", F);
		eq(l, "(f64vector? 'a)", F);
		eq(l, "(f64vector? #\\a)", F);
		eq(l, "(f64vector? #(1 2))", F);
		eq(l, "(f64vector? vector?)", F);
		eq(l, "(f64vector? '(1 2))", F);
		eq(l, "(f64vector? 1)", F);
		eq(l, "(f64vector? \"aaa\")", F);
		eq(l, "(f64vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(f64vector 1.5 2 3)",
				new LispF64Vector((double)1.5, (double)2, (double)3));
		equal(l, "(f64vector)", new LispF64Vector());
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-f64vector 4 1)",
				new LispF64Vector((double)1, (double)1, (double)1, (double)1));
		equal(l, "(make-f64vector 0 1)", new LispF64Vector());
		equal(l, "(make-f64vector 0)", new LispF64Vector());
		lperr(l, "(make-f64vector 4.0)");
		lperr(l, "(make-f64vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(f64vector-length '#f64(1 2 3 4))", 4);
		eqi(l, "(f64vector-length #f64())", 0);
		lperr(l, "(f64vector-length '(1 2 3 4))");
		lperr(l, "(f64vector-length '())");
		lperr(l, "(f64vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqv(l, "(f64vector-ref '#f64(1 2 3 4) 2)", new LispDouble(3));
		eqv(l, "(f64vector-ref '#f64(1 2 3 4) 0)", new LispDouble(1));
		eqv(l, "(f64vector-ref '#f64(1 2 3 4) 3)", new LispDouble(4));
		lperr(l, "(f64vector-ref '#f64(1 2 3 4) 4)");
		lperr(l, "(f64vector-ref '#f64(1 2 3 4) -1)");
		lperr(l, "(f64vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(f64vector-ref '#f64(1 2 3 4) 1.0)");
		lperr(l, "(f64vector-ref '#f64(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#f64(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(f64vector-set! v 1 5)");
		equal(l.get("v"), new LispF64Vector(
				(double)1, (double)5, (double)3, (double)4));
		lperr(l, "(f64vector-set! v -1 5)");
		lperr(l, "(f64vector-set! v 4 5)");
		lperr(l, "(f64vector-set! v 1.0 5)");
		lperr(l, "(f64vector-set! l 1 5)");
		lperr(l, "(f64vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(f64vector->list '#f64(1 2 3 4))",
				list(new LispDouble(1), new LispDouble(2),
						new LispDouble(3), new LispDouble(4)));
		equal(l, "(f64vector->list '#f64())", Nil.NIL);
		lperr(l, "(f64vector->list 1)");
		lperr(l, "(f64vector->list '(1 2 3 4))");
		lperr(l, "(f64vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->f64vector '(1 2 3 4))",new LispF64Vector(
				(double)1, (double)2, (double)3, (double)4));
		equal(l, "(list->f64vector '())", new LispF64Vector());
		lperr(l, "(list->f64vector 1)");
		lperr(l, "(list->f64vector '#(1 2 3 4))");
		lperr(l, "(list->f64vector '(1 2 3 4 . 5))");
		lperr(l, "(list->f64vector)");
	}

}
