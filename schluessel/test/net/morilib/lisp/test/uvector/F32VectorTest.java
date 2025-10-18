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

import net.morilib.lisp.LispFloat;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.LispF32Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class F32VectorTest extends TCSubr {

	/**
	 * 
	 */
	public void testF32Vector() {
		Scheme l = Scheme.newInstance();

		eq(l, "(f32vector? #f32(1 2))", T);
		eq(l, "(f32vector? #f)", F);
		eq(l, "(f32vector? 'a)", F);
		eq(l, "(f32vector? #\\a)", F);
		eq(l, "(f32vector? #(1 2))", F);
		eq(l, "(f32vector? vector?)", F);
		eq(l, "(f32vector? '(1 2))", F);
		eq(l, "(f32vector? 1)", F);
		eq(l, "(f32vector? \"aaa\")", F);
		eq(l, "(f32vector? (current-output-port))", F);
		lperr(l, "(vector?)");
	}

	/**
	 * 
	 */
	public void testVector() {
		Scheme l = Scheme.newInstance();

		equal(l, "(f32vector 1.5 2 3)",
				new LispF32Vector((float)1.5, (float)2, (float)3));
		equal(l, "(f32vector)", new LispF32Vector());
	}

	/**
	 * 
	 */
	public void testMakeVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(make-f32vector 4 1)",
				new LispF32Vector((float)1, (float)1, (float)1, (float)1));
		equal(l, "(make-f32vector 0 1)", new LispF32Vector());
		equal(l, "(make-f32vector 0)", new LispF32Vector());
		lperr(l, "(make-f32vector 4.0)");
		lperr(l, "(make-f32vector)");
	}

	/**
	 * 
	 */
	public void testVectorLength() {
		Scheme l = Scheme.newInstance();

		eqi(l, "(f32vector-length '#f32(1 2 3 4))", 4);
		eqi(l, "(f32vector-length #f32())", 0);
		lperr(l, "(f32vector-length '(1 2 3 4))");
		lperr(l, "(f32vector-length '())");
		lperr(l, "(f32vector-legnth)");
	}

	/**
	 * 
	 */
	public void testVectorRef() {
		Scheme l = Scheme.newInstance();
		
		eqv(l, "(f32vector-ref '#f32(1 2 3 4) 2)", new LispFloat(3));
		eqv(l, "(f32vector-ref '#f32(1 2 3 4) 0)", new LispFloat(1));
		eqv(l, "(f32vector-ref '#f32(1 2 3 4) 3)", new LispFloat(4));
		lperr(l, "(f32vector-ref '#f32(1 2 3 4) 4)");
		lperr(l, "(f32vector-ref '#f32(1 2 3 4) -1)");
		lperr(l, "(f32vector-ref '#(1 2 3 4) 1)");
		lperr(l, "(f32vector-ref '#f32(1 2 3 4) 1.0)");
		lperr(l, "(f32vector-ref '#f32(1 2 3 4))");
	}

	/**
	 * 
	 */
	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v '#f32(1 2 3 4))");
		l.exec("(define l '(1 2 3 4))");
		l.exec("(f32vector-set! v 1 5)");
		equal(l.get("v"), new LispF32Vector(
				(float)1, (float)5, (float)3, (float)4));
		lperr(l, "(f32vector-set! v -1 5)");
		lperr(l, "(f32vector-set! v 4 5)");
		lperr(l, "(f32vector-set! v 1.0 5)");
		lperr(l, "(f32vector-set! l 1 5)");
		lperr(l, "(f32vector-set! v 1)");
	}

	/**
	 * 
	 */
	public void testVectorToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(f32vector->list '#f32(1 2 3 4))",
				list(new LispFloat(1), new LispFloat(2),
						new LispFloat(3), new LispFloat(4)));
		equal(l, "(f32vector->list '#f32())", Nil.NIL);
		lperr(l, "(f32vector->list 1)");
		lperr(l, "(f32vector->list '(1 2 3 4))");
		lperr(l, "(f32vector->list)");
	}

	/**
	 * 
	 */
	public void testListToVector() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list->f32vector '(1 2 3 4))",new LispF32Vector(
				(float)1, (float)2, (float)3, (float)4));
		equal(l, "(list->f32vector '())", new LispF32Vector());
		lperr(l, "(list->f32vector 1)");
		lperr(l, "(list->f32vector '#(1 2 3 4))");
		lperr(l, "(list->f32vector '(1 2 3 4 . 5))");
		lperr(l, "(list->f32vector)");
	}

}
