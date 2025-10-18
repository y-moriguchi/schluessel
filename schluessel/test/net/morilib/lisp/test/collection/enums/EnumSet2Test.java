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
package net.morilib.lisp.test.collection.enums;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/20
 */
public class EnumSet2Test extends TCSubr {

	public void testIsCollection() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		equal(l,"(collection? (s))", T);
		equal(l,"(collection? 1)", F);
		equal(l,"(collection? #(1))", T);
	}

	public void testCollectionName() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		equal(l,"(collection-name (s))", sym("enum-set2"));
	}

	public void testIsEnumSet() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		equal(l,"(enum-set2? (s))", T);
		equal(l,"(enum-set2? 1)", F);
		equal(l,"(enum-set2? #(1))", F);
	}

	public void testEnumSetSize() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		eqi  (l,"(enum-set2-size (s 1 2 3))", 3);
		eqi  (l,"(enum-set2-size (s))", 0);
		lperr(l,"(enum-set2-size #(1))");
	}

	public void testEnumSetCount() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		eqi  (l,"(enum-set2-count (s 1 2 3) 1)", 1);
		eqi  (l,"(enum-set2-count (s 1 2 3) 0)", 0);
		eqi  (l,"(enum-set2-count (s) 1)", 0);
		lperr(l,"(enum-set2-count #(1) 1)");
	}

	public void testEnumSetGetAny() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		eqi  (l,"(enum-set2-get-any (s 1) (lambda () 2))", 1);
		eqi  (l,"(enum-set2-get-any (s) (lambda () 2))", 2);
		equal(l,"(enum-set2-get-any (s))", F);
	}

	public void testIsEnumSetEmpty() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		equal(l,"(enum-set2-empty? (s))", T);
		equal(l,"(enum-set2-empty? (s 1))", F);
	}

	public void testEnumSetToList() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		equal(l,"(enum-set2->list (s 1))", list(1));
		equal(l,"(enum-set2->list (s))", list());
	}

	public void testEnumSetClear() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2-empty? (enum-set2-clear hs))", T);
		equal(l,"(enum-set2-empty? hs)", F);
	}

	public void testEnumSetClearS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2-empty? (enum-set2-clear! hs))", T);
		equal(l,"(enum-set2-empty? hs)", T);
	}

	public void testIsEnumSetEqual() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		equal(l,"(enum-set2= eqv? (s 1 2) (s 2 1))", T);
		equal(l,"(enum-set2= eqv? (s 1 2) (s 2 3))", F);
		equal(l,"(enum-set2= eqv? (s 1 2) (s 2))", F);
		equal(l,"(enum-set2= eqv? (s 1 2) (s 1 2 3))", F);
		equal(l,"(enum-set2= eqv? (s 1 2 3)" +
				"           (s 2 1 3)" +
				"           (s 3 2 1))", T);
		equal(l,"(enum-set2= eqv? (s) (s))", T);
		equal(l,"(enum-set2= eqv? (s 1 2))", T);
		lperr(l,"(enum-set2= eqv? '(1 2) (s 1 2))");
	}

	public void testEnumSetCopy() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		l.exec ("(define hs1 (s 1 2))");
		l.exec ("(define hs2 (enum-set2-copy hs1))");
		equal(l,"(enum-set2= eqv? hs1 hs2)", T);
		equal(l,"(eq? hs1 hs2)", F);
	}

	public void testIsSet() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		equal(l,"(set? (s))", T);
		equal(l,"(set? #(1))", F);
	}

	public void testIsEnumSetContains() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		equal(l,"(enum-set2-contains? (s 1 2 3) 1)", T);
		equal(l,"(enum-set2-contains? (s 1 2 3) 0)", F);
		equal(l,"(enum-set2-contains? (s) 1)", F);
		lperr(l,"(enum-set2-count #(1) 1)");
	}

	public void testIsEnumSetSubset2() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		equal(l,"(enum-set2-subset? (s 1 2) (s 2 1))", T);
		equal(l,"(enum-set2-subset? (s 1 2) (s 2 3))", F);
		equal(l,"(enum-set2-subset? (s 1 2) (s 2))", F);
		equal(l,"(enum-set2-subset? (s 1 2) (s 1 2 3))", T);
		equal(l,"(enum-set2-subset? (s 1 2 3)" +
				"           (s 2 1 3)" +
				"           (s 3 2 1))", T);
		equal(l,"(enum-set2-subset? (s 1 2)" +
				"           (s 2 1 3)" +
				"           (s 4 2 1))", T);
		equal(l,"(enum-set2-subset? (s 1 2)" +
				"           (s 2 1 3)" +
				"           (s 3 4 1))", F);
		equal(l,"(enum-set2-subset? (s) (s))", T);
		equal(l,"(enum-set2-subset? (s 1 2))", T);
	}

	public void testEnumSetAdd() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv? (enum-set2-add hs 3) (s 1 2 3))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2))", T);
	}

	public void testEnumSetAddS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv? (enum-set2-add! hs 3) (s 1 2 3))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2 3))", T);
	}

	public void testEnumSetDelete() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		l.exec ("(define hs (s 1 2 3))");
		equal(l,"(enum-set2= eqv? (enum-set2-delete hs 3) (s 1 2))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2 3))", T);
	}

	public void testEnumSetDeleteS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3)))");
		l.exec ("(define hs (s 1 2 3))");
		equal(l,"(enum-set2= eqv? (enum-set2-delete! hs 3) (s 1 2))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2))", T);
	}

	public void testEnumSetUnion() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-union hs (s 2 3 4))" +
				" (s 1 2 3 4))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2))", T);
	}

	public void testEnumSetUnionS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-union! hs (s 2 3 4))" +
				" (s 1 2 3 4))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2 3 4))", T);
	}

	public void testEnumSetIntersection() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-intersection hs (s 2 3 4))" +
				" (s 2))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2))", T);
	}

	public void testEnumSetIntersectionS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-intersection! hs (s 2 3 4))" +
				" (s 2))", T);
		equal(l,"(enum-set2= eqv? hs (s 2))", T);
	}

	public void testEnumSetDifference() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-difference hs (s 2 3 4))" +
				" (s 1))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2))", T);
	}

	public void testEnumSetDifferenceS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-difference! hs (s 2 3 4))" +
				" (s 1))", T);
		equal(l,"(enum-set2= eqv? hs (s 1))", T);
	}

	public void testEnumSetSymmetricDifference() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-symmetric-difference hs (s 2 3 4))" +
				" (s 1 3 4))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2))", T);
	}

	public void testEnumSetSymmetricDifferenceS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-symmetric-difference! hs (s 2 3 4))" +
				" (s 1 3 4))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 3 4))", T);
	}

	public void testEnumSetAddFrom() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-add-from hs '(2 3 4 3 2))" +
				" (s 1 2 3 4))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2))", T);
	}

	public void testEnumSetAddFromS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-add-from! hs '(2 3 4 3 2))" +
				" (s 1 2 3 4))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2 3 4))", T);
	}

	public void testEnumSetDeleteFrom() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-delete-from hs '(2 3 4 3 2))" +
				" (s 1))", T);
		equal(l,"(enum-set2= eqv? hs (s 1 2))", T);
	}

	public void testEnumSetDeleteFromS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		l.exec ("(define hs (s 1 2))");
		equal(l,"(enum-set2= eqv?" +
				" (enum-set2-delete-from! hs '(2 3 4 3 2))" +
				" (s 1))", T);
		equal(l,"(enum-set2= eqv? hs (s 1))", T);
	}

	public void testIsNeighborhoodOf() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		equal(l,"(neighborhood-of? (s 1 2) 1)", T);
		equal(l,"(neighborhood-of? (s 1 2) 3)", F);
		equal(l,"(neighborhood-of? (s 1 2) 0)", F);
	}

	public void testIsContained() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		equal(l,"(topology-contained? (s 1 2) #r[1 2])", T);
		equal(l,"(topology-contained? (s 1 2 3) #r[1 2])", F);
	}

	public void testIsIndependent() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		equal(l,"(topology-independent? (s 1 2) #r(2 4))", T);
		equal(l,"(topology-independent? (s 1 2 3) #r(2 4))", F);
	}

	public void testUnionTopology() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		equal(l,"(eqv? (topology-union (s 1 2) (s 2 3)) (s 1 2 3))", T);
		equal(l,"(eqv? (topology-union (s 1 2) #r[1 2]) #r[1 2])", T);
	}

	public void testIntersectionTopology() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)))");
		equal(l,"(eqv? (topology-intersection (s 1 2) (s 2 3)) (s 2))", T);
		equal(l,"(eqv? (topology-intersection (s 1 2) #r[1 2]) (s 1 2))", T);
	}

	public void testInterior() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)" +
				"            '(((:power 1 2 3)) (3 4) (4))))");
		equal(l,"(eqv? (topology-interior (s 1 2)) (s 1 2))", T);
		equal(l,"(eqv? (topology-interior (s 3 4)) (s 3 4))", T);
		equal(l,"(eqv? (topology-interior (s 1 2 4)) (s 1 2))", T);
		equal(l,"(eqv? (topology-interior (s 1 2 3 4)) (s 1 2 3 4))", T);
		equal(l,"(eqv? (topology-interior (s)) (s))", T);
		lperr(l,"(make-enum-set2-class '(1 2 3 4) '((1 2) (2 3)))");
		lperr(l,"(make-enum-set2-class '(1 2 3 4) '((1 2) (2) (2 3) (3 4)))");
		lperr(l,"(make-enum-set2-class '(1 2 3 4) '((:power 1 2 3) (3 4) (4))))");
	}

	public void testClosure() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4 5)" +
				"            '(((:power 1 2 3)) (3 4) (4))))");
		equal(l,"(eqv? (topology-closure (s 1 3)) (s 1 3 4 5))", T);
		equal(l,"(eqv? (topology-closure (s 1 2 5)) (s 1 2 5))", T);
		equal(l,"(eqv? (topology-closure (s 2 5)) (s 2 4 5))", T);
		equal(l,"(eqv? (topology-closure (s 5)) (s 4 5))", T);
		equal(l,"(eqv? (topology-closure (s 1 2 3 4 5)) (s 1 2 3 4 5))", T);
		equal(l,"(eqv? (topology-closure (s)) (s))", T);
	}

	public void testIsOpen() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4)" +
				"            '(((:power 1 2 3)) (3 4) (4))))");
		equal(l,"(topology-open? (s 1 2))", T);
		equal(l,"(topology-open? (s 1 3 4))", F);
		equal(l,"(topology-open? (s 2 4))", F);
		equal(l,"(topology-open? (s 4))", T);
		equal(l,"(topology-open? (s))", T);
		equal(l,"(topology-open? (s 1 2 3 4))", T);
	}

	public void testIsClosed() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s (make-enum-set2-class '(1 2 3 4 5)" +
				"            '(((:power 1 2 3)) (3 4) (4))))");
		equal(l,"(topology-closed? (s 1 3 4 5))", T);
		equal(l,"(topology-closed? (s 1 3 4))", F);
		equal(l,"(topology-closed? (s 2 5))", F);
		equal(l,"(topology-closed? (s))", T);
		equal(l,"(topology-closed? (s 1 2 3 4 5))", T);
	}

}
