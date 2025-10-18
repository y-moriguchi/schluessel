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
package net.morilib.lisp.test.r6rs;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/21
 */
public class HashtableTest extends TCSubr {

	public void testHashFunction() {
		Scheme l = Scheme.newInstance();

		l.input("(define h1 (make-eq-hashtable))");
		l.input("(define h2 (make-eqv-hashtable))");
		l.input("(define h3 (make-hashtable equal? equal-hash))");
		l.input("(define h4 (make-hashtable string=? string-hash))");
		l.input("(define h5 (make-hashtable string-ci=? string-ci-hash))");
		l.input("(define h6 (make-hashtable eq? symbol-hash))");

		l.input("(hashtable-set! h1 'a 72)");
		l.input("(hashtable-set! h2 'a 72)");
		l.input("(hashtable-set! h3 'a 72)");
//		l.input("(hashtable-set! h4 'a 72)");
//		l.input("(hashtable-set! h5 'a 72)");
		l.input("(hashtable-set! h6 'a 72)");
		eqi  (l,"(hashtable-ref h1 'a)", 72);
		eqi  (l,"(hashtable-ref h2 'a)", 72);
		eqi  (l,"(hashtable-ref h3 'a)", 72);
//		eqi  (l,"(hashtable-ref h4 'a)", 72);
//		eqi  (l,"(hashtable-ref h5 'a)", 72);
		eqi  (l,"(hashtable-ref h6 'a)", 72);

		l.input("(hashtable-set! h1 1 72)");
		l.input("(hashtable-set! h2 1 72)");
		l.input("(hashtable-set! h3 1 72)");
//		l.input("(hashtable-set! h4 1 72)");
//		l.input("(hashtable-set! h5 1 72)");
		l.input("(hashtable-set! h6 1 72)");
//		eqi  (l,"(hashtable-ref h1 1)", 72);
		eqi  (l,"(hashtable-ref h2 1)", 72);
		eqi  (l,"(hashtable-ref h3 1)", 72);
//		eqi  (l,"(hashtable-ref h4 1)", 72);
//		eqi  (l,"(hashtable-ref h5 1)", 72);
//		eqi  (l,"(hashtable-ref h6 1)", 72);

		l.input("(hashtable-set! h1 '(1) 72)");
		l.input("(hashtable-set! h2 '(1) 72)");
		l.input("(hashtable-set! h3 '(1) 72)");
//		l.input("(hashtable-set! h4 '(1) 72)");
//		l.input("(hashtable-set! h5 '(1) 72)");
		l.input("(hashtable-set! h6 '(1) 72)");
//		eqi  (l,"(hashtable-ref h1 '(1))", 72);
//		eqi  (l,"(hashtable-ref h2 '(1))", 72);
		eqi  (l,"(hashtable-ref h3 '(1))", 72);
//		eqi  (l,"(hashtable-ref h4 '(1))", 72);
//		eqi  (l,"(hashtable-ref h5 '(1))", 72);
//		eqi  (l,"(hashtable-ref h6 '(1))", 72);

		l.input("(hashtable-set! h1 \"ku\" 72)");
		l.input("(hashtable-set! h2 \"ku\" 72)");
		l.input("(hashtable-set! h3 \"ku\" 72)");
		l.input("(hashtable-set! h4 \"ku\" 72)");
		l.input("(hashtable-set! h5 \"ku\" 72)");
		l.input("(hashtable-set! h6 \"ku\" 72)");
//		eqi  (l,"(hashtable-ref h1 \"ku\")", 72);
//		eqi  (l,"(hashtable-ref h2 \"ku\")", 72);
		eqi  (l,"(hashtable-ref h3 \"ku\")", 72);
		eqi  (l,"(hashtable-ref h4 \"ku\")", 72);
		eqi  (l,"(hashtable-ref h4 \"Ku\" -1)", -1);
		eqi  (l,"(hashtable-ref h5 \"ku\")", 72);
		eqi  (l,"(hashtable-ref h5 \"Ku\")", 72);
//		eqi  (l,"(hashtable-ref h6 \"ku\")", 72);
	}

	public void testProcedures() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(hashtable? (make-eqv-hashtable))", T);
		eq   (l,"(hashtable? 1)", F);

		l.input("(define h (make-eqv-hashtable))");
		eqi  (l,"(hashtable-size h)", 0);
		l.input("(hashtable-set! h 1 72)");
		l.input("(hashtable-set! h 2 91)");
		eqi  (l,"(hashtable-size h)", 2);

		l.input("(define g (hashtable-copy h))");
		eqi  (l,"(hashtable-ref g 1)", 72);
		eqi  (l,"(hashtable-ref g 2 -1)", 91);
		lperr(l,"(hashtable-set! g 3 -1)");

		eqi  (l,"(hashtable-ref h 1)", 72);
		eqi  (l,"(hashtable-ref h 2 -1)", 91);
		eqi  (l,"(hashtable-ref h 3 -1)", -1);

		l.input("(hashtable-delete! h 2)");
		eqi  (l,"(hashtable-ref h 2 -1)", -1);
		l.input("(hashtable-delete! h 3)");
		eqi  (l,"(hashtable-ref h 3 -1)", -1);
		lperr(l,"(hashtable-delete! g 1)");

		eq   (l,"(hashtable-contains? h 1)", T);
		eq   (l,"(hashtable-contains? h 3)", F);

		l.input("(hashtable-update! h 1 - 72)");
		l.input("(hashtable-update! h 2 - 91)");
		eqi  (l,"(hashtable-ref h 1)", -72);
		eqi  (l,"(hashtable-ref h 2 -1)", -91);
		lperr(l,"(hashtable-update! g 1 - -1)");

		l.input("(define i (hashtable-copy h #t))");
		l.input("(hashtable-clear! i)");
		eqi  (l,"(hashtable-size i)", 0);
		lperr(l,"(hashtable-clear! g)");

		eq   (l,"(hashtable-mutable? h)", T);
		eq   (l,"(hashtable-mutable? g)", F);
		eq   (l,"(hashtable-mutable? i)", T);
//		equal(l,"(hashtable-keys h)", vec(1, 2));
//		equal(l,"(hashtable-entries h)", vec(1, 2), vec(72, 91));
	}

}
