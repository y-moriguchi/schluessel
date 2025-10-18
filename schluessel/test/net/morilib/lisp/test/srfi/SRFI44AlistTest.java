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
 * @author MORIGUCHI, Yuichiro 2011/05/19
 */
public class SRFI44AlistTest extends TCSubr {

	public void testAlistMapFoldLeft() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-fold-left" +
				" '((1 . 2) (2 . 4) (3 . 9)) cons '())",
				list(cons(3, 9), cons(2, 4), cons(1, 2)));
		equal(l,"(alist-map-fold-left '() cons '(1))", list(1));
		lperr(l,"(alist-map-fold-left '#(1 2 3) - 1)");
	}

	public void testAlistMapFoldRight() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-fold-right" +
				" '((1 . 2) (2 . 4) (3 . 9)) cons '())",
				list(cons(1, 2), cons(2, 4), cons(3, 9)));
		equal(l,"(alist-map-fold-right '() cons '(1))", list(1));
		lperr(l,"(alist-map-fold-right '#(1 2 3) - 1)");
	}

	public void testAlistMapFoldKeysLeft() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(alist-map-fold-keys-left" +
				" '((1 . 2) (2 . 4) (3 . 9)) - 1)", 1);
		eqi  (l,"(alist-map-fold-keys-left" +
				" '((1 . 2) (2 . 4) (3 . 9)) - 1 2)", 1, 0);
		eqi  (l,"(alist-map-fold-keys-left '() - 1)", 1);
		lperr(l,"(alist-map-fold-keys-left '#(1 2 3) - 1)");
	}

	public void testAlistMapFoldKeysRight() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-fold-keys-right" +
				" '((1 . 2) (2 . 4) (3 . 9)) cons '())",
				list(1, 2, 3));
		equal(l,"(alist-map-fold-keys-right" +
				" '((1 . 2) (2 . 4) (3 . 9)) cons '() '(4))",
				list(1, 2, 3), list(1, 2, 3, 4));
		equal(l,"(alist-map-fold-keys-right '() cons '(1))", list(1));
		lperr(l,"(alist-map-fold-keys-right '#(1 2 3) - 1)");
	}

	public void testAlistMapCount() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(alist-map-count" +
				" '((1 . 2) (2 . 4) (1 . 2) (3 . 6)) 2)", 2);
		eqi  (l,"(alist-map-count '() 1)", 0);
	}

	public void testAlistMapKeysCount() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(alist-map-key-count" +
				" '((1 . 2) (2 . 4) (1 . 2) (3 . 6)) 1)", 2);
		eqi  (l,"(alist-map-key-count '() 1)", 0);
	}

	public void testIsAlistMapContains() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-contains-key?" +
				" '((1 . 2) (2 . 4) (1 . 2) (3 . 6)) 1)", T);
		equal(l,"(alist-map-contains-key?" +
				" '((1 . 2) (2 . 4) (1 . 2) (3 . 6)) 5)", F);
		equal(l,"(alist-map-contains-key? '() 1)", F);
	}

	public void testAlistMapSize() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(alist-map-size '((1 . 2) (2 . 4) (1 . 2) (3 . 6)))", 4);
		eqi  (l,"(alist-map-size '())", 0);
	}

	public void testAlistMapCopy() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-copy" +
				" '((1 . 2) (2 . 4) (3 . 6)))",
				list(cons(1, 2), cons(2, 4), cons(3, 6)));
	}

	public void testAlistMapToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map->list" +
				" '((1 . 2) (2 . 4) (3 . 6)))",
				list(cons(1, 2), cons(2, 4), cons(3, 6)));
	}

	public void testAlistMapKeysToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-keys->list" +
				" '((1 . 2) (2 . 4) (3 . 6)))", list(1, 2, 3));
	}

	public void testIsAlistMap() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map? '((1 . 2) (2 . 4) (3 . 6)))", T);
		equal(l,"(alist-map? '())", T);
		equal(l,"(alist-map? 1)", F);
	}

	public void testAlistMapEq() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map= eqv?)", T);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (3 . 6)))", T);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (3 . 6))" +
				" '((1 . 2) (2 . 4) (3 . 6)))", T);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (3 . 6))" +
				" '((1 . 2) (2 . 4) (3 . 6))" +
				" '((1 . 2) (2 . 4) (3 . 6)))", T);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (3 . 6))" +
				" '((1 . 2) (2 . 4) (3 . 8)))", F);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (3 . 6))" +
				" '((1 . 2) (2 . 4) (3 . 6))" +
				" '((1 . 2) (2 . 4) (4 . 6)))", F);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (3 . 6))" +
				" '((1 . 2) (2 . 5) (3 . 6))" +
				" '((1 . 2) (2 . 4) (3 . 6)))", F);
		equal(l,"(alist-map= eqv?" +
				" '((1 . (2 3)) (2 . 4) (3 . 6))" +
				" '((1 . (2 3)) (2 . 4) (3 . 6)))", F);
		equal(l,"(alist-map= equal?" +
				" '((1 . (2 3)) (2 . 4) (3 . 6))" +
				" '((1 . (2 3)) (2 . 4) (3 . 6)))", T);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (3 . 6))" +
				" '((3 . 6) (2 . 4) (1 . 2)))", T);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (1 . 6))" +
				" '((1 . 6) (2 . 4) (1 . 2)))", T);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (1 . 6) (1 . 8))" +
				" '((1 . 6) (2 . 4) (1 . 2)))", F);
		equal(l,"(alist-map= eqv?" +
				" '((1 . 2) (2 . 4) (1 . 6))" +
				" '((1 . 6) (2 . 4) (1 . 2) (1 . 8)))", F);
	}

	public void testAlistMapGet() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(alist-map-get" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1)", 2);
		equal(l,"(alist-map-get" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 4)", F);
		eqi  (l,"(alist-map-get" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3))" +
				" 4 (lambda () 8))", 8);
		equal(l,"(alist-map-get '() 1)", F);
	}

	public void testAlistMapGetAll() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-get-all" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1)", list(2, 3));
		equal(l,"(alist-map-get-all" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 2)", list(4));
		equal(l,"(alist-map-get-all" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 4)", list());
		equal(l,"(alist-map-get-all '() 1)", list());
	}

	public void testAlistMapPut() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-put" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 4)",
				list(cons(1, 4), cons(2, 4), cons(3, 6), cons(1, 3)),
				newZ(2));
		equal(l,"(alist-map-put" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 6)",
				list(cons(3, 6), cons(1, 2), cons(2, 4), cons(1, 3)),
				F);
		equal(l,"(alist-map-put" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 6 (lambda () 8))",
				list(cons(3, 6), cons(1, 2), cons(2, 4), cons(1, 3)),
				newZ(8));
		equal(l,"(alist-map-put '() 1 2)", list(cons(1, 2)), F);
	}

	public void testAlistMapPutS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-put!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 4)",
				list(cons(1, 4), cons(2, 4), cons(3, 6), cons(1, 3)),
				newZ(2));
		equal(l,"(alist-map-put!" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 6)",
				list(cons(3, 6), cons(1, 2), cons(2, 4), cons(1, 3)),
				F);
		equal(l,"(alist-map-put!" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 6 (lambda () 8))",
				list(cons(3, 6), cons(1, 2), cons(2, 4), cons(1, 3)),
				newZ(8));
		equal(l,"(alist-map-put! '() 1 2)", list(cons(1, 2)), F);
	}

	public void testAlistMapReplaceAll() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-replace-all" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 4 5)",
				list(cons(2, 4), cons(3, 6), cons(1, 4), cons(1, 5)));
		equal(l,"(alist-map-replace-all" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 4)",
				list(cons(2, 4), cons(3, 6), cons(1, 4)));
		equal(l,"(alist-map-replace-all" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1)",
				list(cons(2, 4), cons(3, 6)));
		equal(l,"(alist-map-replace-all" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 6)",
				list(cons(1, 2), cons(2, 4), cons(1, 3), cons(3, 6)));
		equal(l,"(alist-map-replace-all '() 1 2 3)",
				list(cons(1, 2), cons(1, 3)));
	}

	public void testAlistMapReplaceAllS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-replace-all!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 4 5)",
				list(cons(2, 4), cons(3, 6), cons(1, 4), cons(1, 5)));
		equal(l,"(alist-map-replace-all!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 4)",
				list(cons(2, 4), cons(3, 6), cons(1, 4)));
		equal(l,"(alist-map-replace-all!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1)",
				list(cons(2, 4), cons(3, 6)));
		equal(l,"(alist-map-replace-all!" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 6)",
				list(cons(1, 2), cons(2, 4), cons(1, 3), cons(3, 6)));
		equal(l,"(alist-map-replace-all! '() 1 2 3)",
				list(cons(1, 2), cons(1, 3)));
	}

	public void testAlistMapUpdate() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define (^2 x) (* x x))");
		equal(l,"(alist-map-update" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 ^2)",
				list(cons(1, 4), cons(2, 4), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-update" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 ^2)",
				list(cons(3, F), cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-update" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 ^2 (lambda () 8))",
				list(cons(3, 8), cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-update '() 1 ^2)", list(cons(1, F)));
	}

	public void testAlistMapUpdateS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define (^2 x) (* x x))");
		equal(l,"(alist-map-update!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 ^2)",
				list(cons(1, 4), cons(2, 4), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-update!" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 ^2)",
				list(cons(3, F), cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-update!" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 ^2 (lambda () 8))",
				list(cons(3, 8), cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-update! '() 1 ^2)", list(cons(1, F)));
	}

	public void testAlistMapUpdateAll() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define (^2 x) (* x x))");
		equal(l,"(alist-map-update-all" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 ^2)",
				list(cons(1, 4), cons(2, 4), cons(3, 6), cons(1, 9)));
		equal(l,"(alist-map-update-all" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 ^2)",
				list(cons(3, F), cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-update-all" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 ^2 (lambda () 8))",
				list(cons(3, 8), cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-update-all '() 1 ^2)", list(cons(1, F)));
	}

	public void testAlistMapUpdateAllS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define (^2 x) (* x x))");
		equal(l,"(alist-map-update-all!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1 ^2)",
				list(cons(1, 4), cons(2, 4), cons(3, 6), cons(1, 9)));
		equal(l,"(alist-map-update-all!" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 ^2)",
				list(cons(3, F), cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-update-all!" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3 ^2 (lambda () 8))",
				list(cons(3, 8), cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-update-all! '() 1 ^2)", list(cons(1, F)));
	}

	public void testAlistMapDelete() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-delete" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1)",
				list(cons(2, 4), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-delete" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 2)",
				list(cons(1, 2), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-delete" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3)",
				list(cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-delete '() 1)", list());
	}

	public void testAlistMapDeleteS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-delete!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1)",
				list(cons(2, 4), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-delete!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 2)",
				list(cons(1, 2), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-delete!" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3)",
				list(cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-delete! '() 1)", list());
	}

	public void testAlistMapDeleteAll() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-delete-all" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1)",
				list(cons(2, 4), cons(3, 6)));
		equal(l,"(alist-map-delete-all" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3)",
				list(cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-delete-all '() 1)", list());
	}

	public void testAlistMapDeleteAllS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-delete-all!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) 1)",
				list(cons(2, 4), cons(3, 6)));
		equal(l,"(alist-map-delete-all!" +
				" '((1 . 2) (2 . 4) (1 . 3)) 3)",
				list(cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-delete-all! '() 1)", list());
	}

	public void testAlistMapDeleteFrom() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-delete-from" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3) (1 . 4))" +
				" '(1 3 1 4))",
				list(cons(2, 4), cons(1, 4)));
		equal(l,"(alist-map-delete-from" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) '(1))",
				list(cons(2, 4), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-delete-from" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) '())",
				list(cons(1, 2), cons(2, 4), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-delete-from" +
				" '((1 . 2) (2 . 4) (1 . 3)) '(3))",
				list(cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-delete-from '() '(1))", list());
	}

	public void testAlistMapDeleteFromS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-delete-from!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3) (1 . 4))" +
				" '(1 3 1 4))",
				list(cons(2, 4), cons(1, 4)));
		equal(l,"(alist-map-delete-from!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) '(1))",
				list(cons(2, 4), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-delete-from!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) '())",
				list(cons(1, 2), cons(2, 4), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-delete-from!" +
				" '((1 . 2) (2 . 4) (1 . 3)) '(3))",
				list(cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-delete-from! '() '(1))", list());
	}

	public void testAlistMapDeleteAllFrom() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-delete-all-from" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3) (1 . 4))" +
				" '(1 3 1 4))",
				list(cons(2, 4)));
		equal(l,"(alist-map-delete-all-from" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) '(1))",
				list(cons(2, 4), cons(3, 6)));
		equal(l,"(alist-map-delete-all-from" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3)) '())",
				list(cons(1, 2), cons(2, 4), cons(3, 6), cons(1, 3)));
		equal(l,"(alist-map-delete-all-from" +
				" '((1 . 2) (2 . 4) (1 . 3)) '(3))",
				list(cons(1, 2), cons(2, 4), cons(1, 3)));
		equal(l,"(alist-map-delete-all-from '() '(1))", list());
	}

	public void testAlistMapClear() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-clear" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3) (1 . 4)))",
				list());
		equal(l,"(alist-map-clear '())", list());
	}

	public void testAlistMapClearS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-clear!" +
				" '((1 . 2) (2 . 4) (3 . 6) (1 . 3) (1 . 4)))",
				list());
		equal(l,"(alist-map-clear! '())", list());
	}

	public void testAlistMapAddFrom() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-add-from" +
				" '((1 . 2) (2 . 4)) '((3 . 6) (1 . 3) (1 . 4)))",
				list(cons(1, 2), cons(2, 4),
						cons(3, 6), cons(1, 3), cons(1, 4)));
		equal(l,"(alist-map-add-from '((1 . 2) (2 . 4)) '())",
				list(cons(1, 2), cons(2, 4)));
		equal(l,"(alist-map-add-from" +
				" '() '((3 . 6) (1 . 3) (1 . 4)))",
				list(cons(3, 6), cons(1, 3), cons(1, 4)));
		equal(l,"(alist-map-add-from '() '())", list());
	}

	public void testAlistMapAddFromS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(alist-map-add-from!" +
				" '((1 . 2) (2 . 4)) '((3 . 6) (1 . 3) (1 . 4)))",
				list(cons(1, 2), cons(2, 4),
						cons(3, 6), cons(1, 3), cons(1, 4)));
		equal(l,"(alist-map-add-from! '((1 . 2) (2 . 4)) '())",
				list(cons(1, 2), cons(2, 4)));
		equal(l,"(alist-map-add-from!" +
				" '() '((3 . 6) (1 . 3) (1 . 4)))",
				list(cons(3, 6), cons(1, 3), cons(1, 4)));
		equal(l,"(alist-map-add-from! '() '())", list());
	}

}
