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
 * @author MORIGUCHI, Yuichiro 2011/05/17
 */
public class SRFI44VectorTest extends TCSubr {

	public void testVectorFoldLeft() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(vector-fold-left '#(1 2 3) - 1)", 1);
		eqi  (l,"(vector-fold-left '#(1 2 3) - 1 2)", 1, 0);
		eqi  (l,"(vector-fold-left '#() - 1)", 1);
		lperr(l,"(vector-fold-left '(1 2 3) - 1)");
	}

	public void testVectorFoldRight() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector-fold-right '#(1 2 3) cons '())", list(1, 2, 3));
		equal(l,"(vector-fold-right '#(1 2 3) cons '() '(0))",
				list(1, 2, 3), list(1, 2, 3, 0));
		equal(l,"(vector-fold-right '#() cons '())", list());
	}

	public void testVectorCopy() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector-copy '#(1 2 3 4))", vec(1, 2, 3, 4));
		equal(l,"(vector-copy '#(1 2 3 4) 1)", vec(2, 3, 4));
		equal(l,"(vector-copy '#(1 2 3 4) 1 3)", vec(2, 3));
		equal(l,"(vector-copy '#(1 2 3 4) 1 4)", vec(2, 3, 4));
		equal(l,"(vector-copy '#(1 2 3 4) 3 3)", vec());
		equal(l,"(vector-copy '#())", vec());
		lperr(l,"(vector-copy '#(1 2 3 4) 1 0)");
		lperr(l,"(vector-copy '#(1 2 3 4) 4)");
		lperr(l,"(vector-copy '#(1 2 3 4) 1 5)");
	}

	public void testVectorToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector->list '#(1 2 3 4))", list(1, 2, 3, 4));
		equal(l,"(vector->list '#())", list());
	}

	public void testIsVector() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector? '#(1 2 3 4))", T);
		equal(l,"(vector? '#())", T);
		equal(l,"(vector? 1)", F);
	}

	public void testVectorSize() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(vector-size '#(1 2 3 4))", 4);
		eqi  (l,"(vector-size '#())", 0);
	}

	public void testIsVectorEmpty() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector-empty? '#(1 2 3 4))", F);
		equal(l,"(vector-empty? '#())", T);
	}

	public void testIsVectorContains() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector-contains? '#(1 2 3 4) 1)", T);
		equal(l,"(vector-contains? '#(1 2 3 4) 5)", F);
		equal(l,"(vector-contains? '#() 1)", F);
	}

	public void testVectorCount() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(vector-count '#(1 2 1 3) 1)", 2);
		eqi  (l,"(vector-count '#() 1)", 0);
	}

	public void testVectorRef() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(vector-ref '#(1 2 3 4) 0)", 1);
		lperr(l,"(vector-ref '#(1 2 3 4) 5)");
		eqi  (l,"(vector-ref '#(1 2 3 4) 5 (lambda () 0))", 0);
		lperr(l,"(vector-ref '#() 0)");
		eqi  (l,"(vector-ref '#() 0 (lambda () 0))", 0);
	}

	public void testVectorGetAny() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(vector-get-any '#(1 2 3 4))", 1);
		equal(l,"(vector-get-any '#())", F);
		eqi  (l,"(vector-get-any '#() (lambda () 0))", 0);
	}

	public void testVectorGetLeft() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(vector-get-left '#(1 2 3 4))", 1);
		equal(l,"(vector-get-left '#())", F);
		eqi  (l,"(vector-get-left '#() (lambda () 0))", 0);
	}

	public void testVectorGetRight() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(vector-get-right '#(1 2 3 4))", 4);
		equal(l,"(vector-get-right '#())", F);
		eqi  (l,"(vector-get-right '#() (lambda () 0))", 0);
	}

	public void testVectorSet() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector-set '#(1 2 3) 1 4)", vec(1, 4, 3));
		equal(l,"(vector-set '#(1 2 3) 2 4)", vec(1, 2, 4));
		equal(l,"(vector-set '#(1 2 3) 0 4)", vec(4, 2, 3));
		lperr(l,"(vector-set '#() 0 4)");
		lperr(l,"(vector-set '#(1 2 3) -1 4)");
		lperr(l,"(vector-set '#(1 2 3) 3 4)");
	}

	public void testVectorSetS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector-set! '#(1 2 3) 1 4)", vec(1, 4, 3));
		equal(l,"(vector-set! '#(1 2 3) 2 4)", vec(1, 2, 4));
		equal(l,"(vector-set! '#(1 2 3) 0 4)", vec(4, 2, 3));
		lperr(l,"(vector-set! '#() 0 4)");
		lperr(l,"(vector-set! '#(1 2 3) -1 4)");
		lperr(l,"(vector-set! '#(1 2 3) 3 4)");
	}

	public void testVectorReplaceFrom() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector-replace-from '#(1 2 3 4) 0 '#(9 8))",
				vec(9, 8, 3, 4));
		equal(l,"(vector-replace-from '#(1 2 3 4) 0 '#(0 1 9 8) 2)",
				vec(9, 8, 3, 4));
		equal(l,"(vector-replace-from '#(1 2 3 4) 0 '#(0 1 9 8 7) 2 4)",
				vec(9, 8, 3, 4));
		equal(l,"(vector-replace-from '#(1 2 3 4) 1 '#(9 8))",
				vec(1, 9, 8, 4));
		equal(l,"(vector-replace-from '#(1 2 3 4) 2 '#(9 8))",
				vec(1, 2, 9, 8));
		lperr(l,"(vector-replace-from '#(1 2 3 4) 3 '#(9 8))");
		lperr(l,"(vector-replace-from '#() 0 '#(9 8))");
	}

	public void testVectorReplaceFromS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector-replace-from! '#(1 2 3 4) 0 '#(9 8))",
				vec(9, 8, 3, 4));
		equal(l,"(vector-replace-from! '#(1 2 3 4) 0 '#(0 1 9 8) 2)",
				vec(9, 8, 3, 4));
		equal(l,"(vector-replace-from! '#(1 2 3 4) 0 '#(0 1 9 8 7) 2 4)",
				vec(9, 8, 3, 4));
		equal(l,"(vector-replace-from! '#(1 2 3 4) 1 '#(9 8))",
				vec(1, 9, 8, 4));
		equal(l,"(vector-replace-from! '#(1 2 3 4) 2 '#(9 8))",
				vec(1, 2, 9, 8));
		lperr(l,"(vector-replace-from! '#(1 2 3 4) 3 '#(9 8))");
		lperr(l,"(vector-replace-from! '#() 0 '#(9 8))");
	}

	public void testVectorEq() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector= eqv?)", T);
		equal(l,"(vector= eqv? '#(1 2 3))", T);
		equal(l,"(vector= eqv? '#(1 2 3) '#(1 2 3))", T);
		equal(l,"(vector= eqv? '#(1 2 3) '#(1 2 3) '#(1 2 3))", T);
		equal(l,"(vector= eqv? '#(1 2 3) '#(1 2 4))", F);
		equal(l,"(vector= eqv? '#(1 2 3) '#(1 2 3) '#(2 2 3))", F);
		equal(l,"(vector= eqv? '#(1 2 3) '#(1 2 4) '#(1 2 3))", F);
		equal(l,"(vector= eqv? '#((1 2) 2 3) '#((1 2) 2 3))", F);
		equal(l,"(vector= equal? '#((1 2) 2 3) '#((1 2) 2 3))", T);
	}

}
