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
 * @author MORIGUCHI, Yuichiro 2011/05/14
 */
public class SRFI44ListTest extends TCSubr {

	public void testListFoldLeft() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list-fold-left '(1 2 3) - 1)", 1);
		eqi  (l,"(list-fold-left '(1 2 3) - 1 2)", 1, 0);
		eqi  (l,"(list-fold-left '() - 1)", 1);
		lperr(l,"(list-fold-left #(1 2 3) - 1)");
	}

	public void testListFoldRight() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-fold-right '(1 2 3) cons '())", list(1, 2, 3));
		equal(l,"(list-fold-right '(1 2 3) cons '() '(0))",
				list(1, 2, 3), list(1, 2, 3, 0));
		equal(l,"(list-fold-right '() cons '())", list());
	}

	public void testListCopy() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-copy '(1 2 3 4))", list(1, 2, 3, 4));
		equal(l,"(equal? (list-copy '#0=(1 . #0#)) '#0=(1 . #0#))", T);
		equal(l,"(list-copy '(1 2 3 4) 1)", list(2, 3, 4));
		equal(l,"(list-copy '(1 2 3 4) 1 3)", list(2, 3));
		equal(l,"(list-copy '(1 2 3 4) 1 4)", list(2, 3, 4));
		equal(l,"(list-copy '(1 2 3 4) 3 3)", list());
		equal(l,"(list-copy '())", list());
		lperr(l,"(list-copy '(1 2 3 4) 1 0)");
		lperr(l,"(list-copy '(1 2 3 4) 4)");
		lperr(l,"(list-copy '(1 2 3 4) 1 5)");
	}

	public void testListToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list->list '(1 2 3 4))", list(1, 2, 3, 4));
		equal(l,"(list->list '())", list());
	}

	public void testIsList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list? '(1 2 3 4))", T);
		equal(l,"(list? '#0=(1 . #0#))", F);
		equal(l,"(list? '())", T);
		equal(l,"(list? 1)", F);
	}

	public void testListSize() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list-size '(1 2 3 4))", 4);
		equal(l,"(list-size '#0=(1 . #0#))", F);
		eqi  (l,"(list-size '())", 0);
	}

	public void testIsListEmpty() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-empty? '(1 2 3 4))", F);
		equal(l,"(list-empty? '#0=(1 . #0#))", F);
		equal(l,"(list-empty? '())", T);
	}

	public void testIsListContains() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-contains? '(1 2 3 4) 1)", T);
		equal(l,"(list-contains? '(1 2 3 4) 5)", F);
		equal(l,"(list-contains? '() 1)", F);
	}

	public void testListRef() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list-ref '(1 2 3 4) 0)", 1);
		lperr(l,"(list-ref '(1 2 3 4) 5)");
		eqi  (l,"(list-ref '(1 2 3 4) 5 (lambda () 0))", 0);
		lperr(l,"(list-ref '() 0)");
		eqi  (l,"(list-ref '() 0 (lambda () 0))", 0);
	}

	public void testListGetAny() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list-get-any '(1 2 3 4))", 1);
		equal(l,"(list-get-any '())", F);
		eqi  (l,"(list-get-any '() (lambda () 0))", 0);
	}

	public void testListGetLeft() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list-get-left '(1 2 3 4))", 1);
		equal(l,"(list-get-left '())", F);
		eqi  (l,"(list-get-left '() (lambda () 0))", 0);
	}

	public void testListGetRight() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list-get-right '(1 2 3 4))", 4);
		equal(l,"(list-get-right '())", F);
		eqi  (l,"(list-get-right '() (lambda () 0))", 0);
	}

	public void testListCount() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list-count '(1 2 1 3) 1)", 2);
		eqi  (l,"(list-count '() 1)", 0);
	}

	public void testListEq() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list= eqv?)", T);
		equal(l,"(list= eqv? '(1 2 3))", T);
		equal(l,"(list= eqv? '(1 2 3) '(1 2 3))", T);
		equal(l,"(list= eqv? '(1 2 3) '(1 2 3) '(1 2 3))", T);
		equal(l,"(list= eqv? '(1 2 3) '(1 2 4))", F);
		equal(l,"(list= eqv? '(1 2 3) '(1 2 3) '(2 2 3))", F);
		equal(l,"(list= eqv? '(1 2 3) '(1 2 4) '(1 2 3))", F);
		equal(l,"(list= eqv? '((1 2) 2 3) '((1 2) 2 3))", F);
		equal(l,"(list= equal? '((1 2) 2 3) '((1 2) 2 3))", T);
	}

	public void testListAdd() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-add '(1 2 3) 4)", list(4, 1, 2, 3));
		equal(l,"(list-add '() 4)", list(4));
	}

	public void testListAddS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-add! '(1 2 3) 4)", list(4, 1, 2, 3));
		equal(l,"(list-add! '() 4)", list(4));
	}

	public void testListSet() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-set '(1 2 3) 1 4)", list(1, 4, 3));
		equal(l,"(list-set '(1 2 3) 2 4)", list(1, 2, 4));
		equal(l,"(list-set '(1 2 3) 0 4)", list(4, 2, 3));
		lperr(l,"(list-set '() 0 4)");
		lperr(l,"(list-set '(1 2 3) -1 4)");
		lperr(l,"(list-set '(1 2 3) 3 4)");
	}

	public void testListSetS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-set! '(1 2 3) 1 4)", list(1, 4, 3));
		equal(l,"(list-set! '(1 2 3) 2 4)", list(1, 2, 4));
		equal(l,"(list-set! '(1 2 3) 0 4)", list(4, 2, 3));
		lperr(l,"(list-set! '() 0 4)");
		lperr(l,"(list-set! '(1 2 3) -1 4)");
		lperr(l,"(list-set! '(1 2 3) 3 4)");
	}

	public void testListInsertLeft() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-insert-left '(1 2 3) 0)", list(0, 1, 2, 3));
		equal(l,"(list-insert-left '() 0)", list(0));
	}

	public void testListInsertLeftS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-insert-left! '(1 2 3) 0)", list(0, 1, 2, 3));
		equal(l,"(list-insert-left! '() 0)", list(0));
	}

	public void testListInsertRight() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-insert-right '(1 2 3) 0)", list(1, 2, 3, 0));
		equal(l,"(list-insert-right '() 0)", list(0));
	}

	public void testListInsertRightS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-insert-right! '(1 2 3) 0)", list(1, 2, 3, 0));
		equal(l,"(list-insert-right! '() 0)", list(0));
	}

	public void testListDelete() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete '(1 2 1 2) 2)", list(1, 1, 2));
		equal(l,"(list-delete '(1 2 1 2) 1)", list(2, 1, 2));
		equal(l,"(list-delete '(1 2 1 2) 3)", list(1, 2, 1, 2));
		equal(l,"(list-delete '() 1)", list());
	}

	public void testListDeleteS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete! '(1 2 1 2) 2)", list(1, 1, 2));
		equal(l,"(list-delete! '(1 2 1 2) 1)", list(2, 1, 2));
		equal(l,"(list-delete! '(1 2 1 2) 3)", list(1, 2, 1, 2));
		equal(l,"(list-delete! '() 1)", list());
	}

	public void testListDeleteLeft() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-left '(1 2 1 2))",
				list(2, 1, 2), newZ(1));
		equal(l,"(list-delete-left '(1))", list(), newZ(1));
		lperr(l,"(list-delete-left '())");
	}

	public void testListDeleteLeftS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-left! '(1 2 1 2))",
				list(2, 1, 2), newZ(1));
		equal(l,"(list-delete-left! '(1))", list(), newZ(1));
		lperr(l,"(list-delete-left! '())");
	}

	public void testListDeleteRight() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-right '(1 2 1 2))",
				list(1, 2, 1), newZ(2));
		equal(l,"(list-delete-right '(1))", list(), newZ(1));
		lperr(l,"(list-delete-right '())");
	}

	public void testListDeleteRightS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-right! '(1 2 1 2))",
				list(1, 2, 1), newZ(2));
		equal(l,"(list-delete-right! '(1))", list(), newZ(1));
		lperr(l,"(list-delete-right! '())");
	}

	public void testListDeleteAll() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-all '(1 2 1 2) 2)", list(1, 1));
		equal(l,"(list-delete-all '(1 2 1 2) 1)", list(2, 2));
		equal(l,"(list-delete-all '(1 2 1 2) 3)", list(1, 2, 1, 2));
		equal(l,"(list-delete-all '() 1)", list());
	}

	public void testListDeleteAllS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-all! '(1 2 1 2) 2)", list(1, 1));
		equal(l,"(list-delete-all! '(1 2 1 2) 1)", list(2, 2));
		equal(l,"(list-delete-all! '(1 2 1 2) 3)", list(1, 2, 1, 2));
		equal(l,"(list-delete-all! '() 1)", list());
	}

	public void testListDeleteFrom() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-from '(1 2 2 2 3 3 4) '(1 2 2 3 3 5 5))",
				list(2, 4));
		equal(l,"(list-delete-from '(1 2 2 2 3 3 4) '())",
				list(1, 2, 2, 2, 3, 3, 4));
		equal(l,"(list-delete-from '() '(1 2 2 3 3))", list());
	}

	public void testListDeleteFromS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-from! '(1 2 2 2 3 3 4) '(1 2 2 3 3 5 5))",
				list(2, 4));
		equal(l,"(list-delete-from! '(1 2 2 2 3 3 4) '())",
				list(1, 2, 2, 2, 3, 3, 4));
		equal(l,"(list-delete-from! '() '(1 2 2 3 3))", list());
	}

	public void testListDeleteAllFrom() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-all-from '(1 2 2 2 3 3 4) '(1 2 2 3 3 5 5))",
				list(4));
		equal(l,"(list-delete-all-from '(1 2 2 2 3 3 4) '())",
				list(1, 2, 2, 2, 3, 3, 4));
		equal(l,"(list-delete-all-from '() '(1 2 2 3 3))", list());
	}

	public void testListDeleteAllFromS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-all-from! '(1 2 2 2 3 3 4) '(1 2 2 3 3 5 5))",
				list(4));
		equal(l,"(list-delete-all-from! '(1 2 2 2 3 3 4) '())",
				list(1, 2, 2, 2, 3, 3, 4));
		equal(l,"(list-delete-all-from! '() '(1 2 2 3 3))", list());
	}

	public void testListReplaceFrom() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-replace-from '(1 2 3 4) 0 '(9 8))",
				list(9, 8, 3, 4));
		equal(l,"(list-replace-from '(1 2 3 4) 0 '(0 1 9 8) 2)",
				list(9, 8, 3, 4));
		equal(l,"(list-replace-from '(1 2 3 4) 0 '(0 1 9 8 7) 2 4)",
				list(9, 8, 3, 4));
		equal(l,"(list-replace-from '(1 2 3 4) 1 '(9 8))",
				list(1, 9, 8, 4));
		equal(l,"(list-replace-from '(1 2 3 4) 2 '(9 8))",
				list(1, 2, 9, 8));
		lperr(l,"(list-replace-from '(1 2 3 4) 3 '(9 8))");
		lperr(l,"(list-replace-from '() 0 '(9 8))");
	}

	public void testListReplaceFromS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-replace-from! '(1 2 3 4) 0 '(9 8))",
				list(9, 8, 3, 4));
		equal(l,"(list-replace-from! '(1 2 3 4) 0 '(0 1 9 8) 2)",
				list(9, 8, 3, 4));
		equal(l,"(list-replace-from! '(1 2 3 4) 0 '(0 1 9 8 7) 2 4)",
				list(9, 8, 3, 4));
		equal(l,"(list-replace-from! '(1 2 3 4) 1 '(9 8))",
				list(1, 9, 8, 4));
		equal(l,"(list-replace-from! '(1 2 3 4) 2 '(9 8))",
				list(1, 2, 9, 8));
		lperr(l,"(list-replace-from! '(1 2 3 4) 3 '(9 8))");
		lperr(l,"(list-replace-from! '() 0 '(9 8))");
	}

	public void testListClear() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-clear '(1 2 3))", list());
		equal(l,"(list-clear '())", list());
	}

	public void testListClearS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-clear! '(1 2 3))", list());
		equal(l,"(list-clear! '())", list());
	}

	public void testListInsert() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-insert '(1 2 3) 1 4)", list(1, 4, 2, 3));
		equal(l,"(list-insert '(1 2 3) 2 4)", list(1, 2, 4, 3));
		equal(l,"(list-insert '(1 2 3) 3 4)", list(1, 2, 3, 4));
		equal(l,"(list-insert '(1 2 3) 0 4)", list(4, 1, 2, 3));
		equal(l,"(list-insert '() 0 4)", list(4));
		lperr(l,"(list-insert '(1 2 3) -1 4)");
		lperr(l,"(list-insert '(1 2 3) 4 4)");
	}

	public void testListInsertS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-insert! '(1 2 3) 1 4)", list(1, 4, 2, 3));
		equal(l,"(list-insert! '(1 2 3) 2 4)", list(1, 2, 4, 3));
		equal(l,"(list-insert! '(1 2 3) 3 4)", list(1, 2, 3, 4));
		equal(l,"(list-insert! '(1 2 3) 0 4)", list(4, 1, 2, 3));
		equal(l,"(list-insert! '() 0 4)", list(4));
		lperr(l,"(list-insert! '(1 2 3) -1 4)");
		lperr(l,"(list-insert! '(1 2 3) 4 4)");
	}

	public void testListDeleteAt() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-at '(1 2 3) 1)", list(1, 3));
		equal(l,"(list-delete-at '(1 2 3) 2)", list(1, 2));
		equal(l,"(list-delete-at '(1 2 3) 0)", list(2, 3));
		lperr(l,"(list-delete-at '() 0 4)");
		lperr(l,"(list-delete-at '(1 2 3) -1)");
		lperr(l,"(list-delete-at '(1 2 3) 3)");
	}

	public void testListDeleteAtS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(list-delete-at! '(1 2 3) 1)", list(1, 3));
		equal(l,"(list-delete-at! '(1 2 3) 2)", list(1, 2));
		equal(l,"(list-delete-at! '(1 2 3) 0)", list(2, 3));
		lperr(l,"(list-delete-at! '() 0 4)");
		lperr(l,"(list-delete-at! '(1 2 3) -1)");
		lperr(l,"(list-delete-at! '(1 2 3) 3)");
	}

}
