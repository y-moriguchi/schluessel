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
 * @author MORIGUCHI, Yuichiro 2011/08/20
 */
public class SRFI44TreeSetTest extends TCSubr {

	public void testIsCollection() {
		Scheme l = Scheme.newInstance();

		equal(l,"(collection? (make-tree-set))", T);
		equal(l,"(collection? 1)", F);
		equal(l,"(collection? #(1))", T);
	}

	public void testCollectionName() {
		Scheme l = Scheme.newInstance();

		equal(l,"(collection-name (make-tree-set))", sym("tree-set"));
	}

	public void testIsTreeSet() {
		Scheme l = Scheme.newInstance();

		equal(l,"(tree-set? (make-tree-set))", T);
		equal(l,"(tree-set? 1)", F);
		equal(l,"(tree-set? #(1))", F);
	}

	public void testTreeSetSize() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(tree-set-size (tree-set 1 2 3))", 3);
		eqi  (l,"(tree-set-size (tree-set))", 0);
		lperr(l,"(tree-set-size #(1))");
	}

	public void testTreeSetCount() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(tree-set-count (tree-set 1 2 3) 1)", 1);
		eqi  (l,"(tree-set-count (tree-set 1 2 3) 0)", 0);
		eqi  (l,"(tree-set-count (tree-set) 1)", 0);
		eqi  (l,"(tree-set-count (tree-set '(1 2 3)) '(1 2 3))", 1);
		lperr(l,"(tree-set-count #(1) 1)");
	}

	public void testTreeSetGetAny() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(tree-set-get-any (tree-set 1 2 3) (lambda () 2))", 1);
		eqi  (l,"(tree-set-get-any (tree-set > 1 2 3) (lambda () 2))", 3);
		eqi  (l,"(tree-set-get-any (tree-set) (lambda () 2))", 2);
		equal(l,"(tree-set-get-any (tree-set))", F);
	}

	public void testIsTreeSetEmpty() {
		Scheme l = Scheme.newInstance();

		equal(l,"(tree-set-empty? (tree-set))", T);
		equal(l,"(tree-set-empty? (tree-set 1))", F);
	}

	public void testTreeSetToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(tree-set->list (tree-set 1 2 3))", list(1, 2, 3));
		equal(l,"(tree-set->list (tree-set > 1 2 3))", list(3, 2, 1));
		equal(l,"(tree-set->list (tree-set))", list());
	}

	public void testTreeSetClear() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set-empty? (tree-set-clear hs))", T);
		equal(l,"(tree-set-empty? hs)", F);
	}

	public void testTreeSetClearS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set-empty? (tree-set-clear! hs))", T);
		equal(l,"(tree-set-empty? hs)", T);
	}

	public void testIsTreeSetEqual() {
		Scheme l = Scheme.newInstance();

		equal(l,"(tree-set= eqv? (tree-set 1 2) (tree-set 2 1))", T);
		equal(l,"(tree-set= eqv? (tree-set 1 2) (tree-set 2 3))", F);
		equal(l,"(tree-set= eqv? (tree-set 1 2) (tree-set 2))", F);
		equal(l,"(tree-set= eqv? (tree-set 1 2) (tree-set 1 2 3))", F);
		equal(l,"(tree-set= eqv? (tree-set 1 2) (hash-set 2 1))", T);
		equal(l,"(tree-set= eqv? (tree-set 1 2 3)" +
				"           (hash-set 2 1 3)" +
				"           (tree-set 3 2 1))", T);
		equal(l,"(tree-set= equal? (tree-set '(1 2) 3)" +
				"           (tree-set '(1 2) 3))", T);
		equal(l,"(tree-set= eqv? (tree-set) (make-tree-set))", T);
		equal(l,"(tree-set= eqv? (tree-set 1 2))", T);
		equal(l,"(tree-set= eqv? (tree-set 1 2) #(1 2))", T);
		equal(l,"(tree-set= eqv? (tree-set 1 2) '(1 2))", T);
		lperr(l,"(tree-set= eqv? '(1 2) (tree-set 1 2))");
	}

	public void testTreeSetCopy() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs1 (tree-set 1 2))");
		l.exec ("(define hs2 (tree-set-copy hs1))");
		equal(l,"(tree-set= eqv? hs1 hs2)", T);
		equal(l,"(eq? hs1 hs2)", F);
		l.exec ("(define hs3 (tree-set equal? '(1 2) 3))");
		l.exec ("(define hs4 (tree-set-copy hs3))");
		equal(l,"(tree-set= eqv? hs3 hs4)", T);
		equal(l,"(eq? hs3 hs4)", F);
	}

	public void testIsSet() {
		Scheme l = Scheme.newInstance();

		equal(l,"(set? (tree-set))", T);
		equal(l,"(set? #(1))", F);
	}

	public void testIsTreeSetContains() {
		Scheme l = Scheme.newInstance();

		equal(l,"(tree-set-contains? (tree-set 1 2 3) 1)", T);
		equal(l,"(tree-set-contains? (tree-set 1 2 3) 0)", F);
		equal(l,"(tree-set-contains? (tree-set) 1)", F);
		equal(l,"(tree-set-contains? (tree-set '(1 2 3)) '(1 2 3))", T);
		lperr(l,"(tree-set-count #(1) 1)");
	}

	public void testIsTreeSetSubset() {
		Scheme l = Scheme.newInstance();

		equal(l,"(tree-set-subset? (tree-set 1 2) (tree-set 2 1))", T);
		equal(l,"(tree-set-subset? (tree-set 1 2) (tree-set 2 3))", F);
		equal(l,"(tree-set-subset? (tree-set 1 2) (tree-set 2))", F);
		equal(l,"(tree-set-subset? (tree-set 1 2) (tree-set 1 2 3))", T);
		equal(l,"(tree-set-subset? (tree-set 1 2) (hash-set 1 2 3))", T);
		equal(l,"(tree-set-subset? (tree-set 1 2 3)" +
				"           (hash-set 2 1 3)" +
				"           (tree-set 3 2 1))", T);
		equal(l,"(tree-set-subset? (tree-set 1 2)" +
				"           (hash-set 2 1 3)" +
				"           (tree-set 4 2 1))", T);
		equal(l,"(tree-set-subset? (tree-set 1 2)" +
				"           (hash-set 2 1 3)" +
				"           (tree-set 3 4 1))", F);
		equal(l,"(tree-set-subset? (tree-set) (make-tree-set))", T);
		equal(l,"(tree-set-subset? (tree-set 1 2))", T);
	}

	public void testTreeSetAdd() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv? (tree-set-add hs 3) (tree-set 1 2 3))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
		lperr(l,"(tree-set-add hs \\#a)");
	}

	public void testTreeSetAddS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv? (tree-set-add! hs 3) (tree-set 1 2 3))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2 3))", T);
	}

	public void testTreeSetDelete() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2 3))");
		equal(l,"(tree-set= eqv? (tree-set-delete hs 3) (tree-set 1 2))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2 3))", T);
		lperr(l,"(tree-set-delete hs \\#a)");
	}

	public void testTreeSetDeleteS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2 3))");
		equal(l,"(tree-set= eqv? (tree-set-delete! hs 3) (tree-set 1 2))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
	}

	public void testTreeSetUnion() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-union hs (hash-set 2 3 4))" +
				" (tree-set 1 2 3 4))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
	}

	public void testTreeSetUnionS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-union! hs (hash-set 2 3 4))" +
				" (tree-set 1 2 3 4))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2 3 4))", T);
	}

	public void testTreeSetIntersection() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-intersection hs (hash-set 2 3 4))" +
				" (tree-set 2))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
	}

	public void testTreeSetIntersectionS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-intersection! hs (hash-set 2 3 4))" +
				" (tree-set 2))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 2))", T);
	}

	public void testTreeSetDifference() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-difference hs (hash-set 2 3 4))" +
				" (tree-set 1))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
	}

	public void testTreeSetDifferenceS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-difference! hs (hash-set 2 3 4))" +
				" (tree-set 1))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1))", T);
	}

	public void testTreeSetSymmetricDifference() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-symmetric-difference hs (hash-set 2 3 4))" +
				" (tree-set 1 3 4))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
	}

	public void testTreeSetSymmetricDifferenceS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-symmetric-difference! hs (hash-set 2 3 4))" +
				" (tree-set 1 3 4))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 3 4))", T);
	}

	public void testTreeSetAddFrom() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-add-from hs '(2 3 4 3 2))" +
				" (tree-set 1 2 3 4))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
	}

	public void testTreeSetAddFromS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-add-from! hs '(2 3 4 3 2))" +
				" (tree-set 1 2 3 4))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2 3 4))", T);
	}

	public void testTreeSetDeleteFrom() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-delete-from hs '(2 3 4 3 2))" +
				" (tree-set 1))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
	}

	public void testTreeSetDeleteFromS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2))");
		equal(l,"(tree-set= eqv?" +
				" (tree-set-delete-from! hs '(2 3 4 3 2))" +
				" (tree-set 1))", T);
		equal(l,"(tree-set= eqv? hs (tree-set 1))", T);
	}

	public void testIsOrderingCollection() {
		Scheme l = Scheme.newInstance();

		equal(l,"(ordered-collection? (make-tree-set))", T);
		equal(l,"(ordered-collection? 1)", F);
		equal(l,"(ordered-collection? #(1))", F);
	}

	public void testTreeSetGetLeft() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(tree-set-get-left (tree-set 1 2 3))", 1);
		eqi  (l,"(tree-set-get-left (tree-set > 1 2 3))", 3);
		equal(l,"(tree-set-get-left (tree-set))", F);
	}

	public void testTreeSetGetRight() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(tree-set-get-right (tree-set 1 2 3))", 3);
		eqi  (l,"(tree-set-get-right (tree-set > 1 2 3))", 1);
		equal(l,"(tree-set-get-right (tree-set))", F);
	}

	public void testTreeDeleteLeft() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2 3))");
		equal(l,"(receive (a b) (tree-set-delete-left hs)" +
				"  (list (tree-set= eqv? a (tree-set 2 3)) b))", list(T, 1));
		equal(l,"(tree-set= eqv? hs (tree-set 1 2 3))", T);
		l.exec ("(define hs (tree-set > 1 2 3))");
		equal(l,"(receive (a b) (tree-set-delete-left hs)" +
				"  (list (tree-set= eqv? a (tree-set 1 2)) b))", list(T, 3));
		equal(l,"(tree-set= eqv? hs (tree-set 1 2 3))", T);
	}

	public void testTreeDeleteLeftS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2 3))");
		equal(l,"(receive (a b) (tree-set-delete-left! hs)" +
				"  (list (tree-set= eqv? a (tree-set 2 3)) b))", list(T, 1));
		equal(l,"(tree-set= eqv? hs (tree-set 2 3))", T);
		l.exec ("(define hs (tree-set > 1 2 3))");
		equal(l,"(receive (a b) (tree-set-delete-left! hs)" +
				"  (list (tree-set= eqv? a (tree-set 1 2)) b))", list(T, 3));
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
	}

	public void testTreeDeleteRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2 3))");
		equal(l,"(receive (a b) (tree-set-delete-right hs)" +
				"  (list (tree-set= eqv? a (tree-set 1 2)) b))", list(T, 3));
		equal(l,"(tree-set= eqv? hs (tree-set 1 2 3))", T);
		l.exec ("(define hs (tree-set > 1 2 3))");
		equal(l,"(receive (a b) (tree-set-delete-right hs)" +
				"  (list (tree-set= eqv? a (tree-set 2 3)) b))", list(T, 1));
		equal(l,"(tree-set= eqv? hs (tree-set 1 2 3))", T);
	}

	public void testTreeDeleteRightS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (tree-set 1 2 3))");
		equal(l,"(receive (a b) (tree-set-delete-right! hs)" +
				"  (list (tree-set= eqv? a (tree-set 1 2)) b))", list(T, 3));
		equal(l,"(tree-set= eqv? hs (tree-set 1 2))", T);
		l.exec ("(define hs (tree-set > 1 2 3))");
		equal(l,"(receive (a b) (tree-set-delete-right! hs)" +
				"  (list (tree-set= eqv? a (tree-set 2 3)) b))", list(T, 1));
		equal(l,"(tree-set= eqv? hs (tree-set 2 3))", T);
	}

}
