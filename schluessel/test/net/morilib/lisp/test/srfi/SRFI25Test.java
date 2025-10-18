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
package net.morilib.lisp.test.srfi;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/22
 */
public class SRFI25Test extends TCSubr {

	public void testIsArray() {
		Scheme l = Scheme.newInstance();

		equal(l,"(array? (make-array (shape 0 1)))", T);
		equal(l,"(array? 1)", F);
	}

	public void testMakeArray() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a (make-array (shape 0 2 0 2 0 2) 1))");
		eqi  (l,"(array-ref a 0 0 0)", 1);
		eqi  (l,"(array-ref a 0 0 1)", 1);
		eqi  (l,"(array-ref a 0 1 0)", 1);
		eqi  (l,"(array-ref a 0 1 1)", 1);
		eqi  (l,"(array-ref a 1 0 0)", 1);
		eqi  (l,"(array-ref a 1 0 1)", 1);
		eqi  (l,"(array-ref a 1 1 0)", 1);
		eqi  (l,"(array-ref a 1 1 1)", 1);
	}

	public void testArray() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a (array (shape 0 2 0 2 0 2)" +
				" 1 2 3 4 5 6 7 8))");
		eqi  (l,"(array-ref a 0 0 0)", 1);
		eqi  (l,"(array-ref a 0 0 1)", 2);
		eqi  (l,"(array-ref a 0 1 0)", 3);
		eqi  (l,"(array-ref a 0 1 1)", 4);
		eqi  (l,"(array-ref a 1 0 0)", 5);
		eqi  (l,"(array-ref a 1 0 1)", 6);
		eqi  (l,"(array-ref a 1 1 0)", 7);
		eqi  (l,"(array-ref a 1 1 1)", 8);
	}

	public void testArrayRef() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a (array (shape 0 2 0 2 0 2)" +
				" 1 2 3 4 5 6 7 8))");
		eqi  (l,"(array-ref a 0 0 0)", 1);
		eqi  (l,"(array-ref a '#(0 0 1))", 2);
		eqi  (l,"(array-ref a (array (shape 0 3) 0 1 0))", 3);
	}

	public void testArraySetS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a (array (shape 0 2 0 2 0 2)" +
				" 1 2 3 4 5 6 7 8))");
		l.exec ("(array-set! a 0 0 0 11)");
		l.exec ("(array-set! a '#(0 0 1) 12)");
		l.exec ("(array-set! a (array (shape 0 3) 0 1 0) 13)");
		eqi  (l,"(array-ref a 0 0 0)", 11);
		eqi  (l,"(array-ref a '#(0 0 1))", 12);
		eqi  (l,"(array-ref a (array (shape 0 3) 0 1 0))", 13);
	}

	public void testShareArray() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a (array (shape 0 2 0 2 0 2)" +
				" 1 2 3 4 5 6 7 8))");
		l.exec ("(define b (share-array a (shape 0 2)" +
				" (lambda (x) (values x x x))))");
		l.exec ("(define c (share-array a (shape 0 2 0 2)" +
				" (lambda (x y) (values x x y))))");
		eqi  (l,"(array-ref b 0)", 1);
		eqi  (l,"(array-ref b 1)", 8);
		eqi  (l,"(array-ref c 0 1)", 2);
	}

}
