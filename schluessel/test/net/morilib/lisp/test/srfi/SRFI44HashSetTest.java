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
public class SRFI44HashSetTest extends TCSubr {

	public void testIsCollection() {
		Scheme l = Scheme.newInstance();

		equal(l,"(collection? (make-hash-set))", T);
		equal(l,"(collection? 1)", F);
		equal(l,"(collection? #(1))", T);
	}

	public void testCollectionName() {
		Scheme l = Scheme.newInstance();

		equal(l,"(collection-name (make-hash-set))", sym("hash-set"));
	}

	public void testIsHashSet() {
		Scheme l = Scheme.newInstance();

		equal(l,"(hash-set? (make-hash-set))", T);
		equal(l,"(hash-set? 1)", F);
		equal(l,"(hash-set? #(1))", F);
	}

	public void testHashSetSize() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(hash-set-size (hash-set 1 2 3))", 3);
		eqi  (l,"(hash-set-size (hash-set))", 0);
		lperr(l,"(hash-set-size #(1))");
	}

	public void testHashSetCount() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(hash-set-count (hash-set 1 2 3) 1)", 1);
		eqi  (l,"(hash-set-count (hash-set 1 2 3) 0)", 0);
		eqi  (l,"(hash-set-count (hash-set) 1)", 0);
		eqi  (l,"(hash-set-count (hash-set '(1 2 3)) '(1 2 3))", 0);
		eqi  (l,"(hash-set-count (hash-set equal? '(1 2 3)) '(1 2 3))", 1);
		eqi  (l,"(hash-set-count (hash-set string-ci=? \"a\") \"A\")", 0);
		eqi  (l,"(hash-set-count" +
				" (hash-set string-ci=? string-ci-hash \"a\") \"A\")", 1);
		lperr(l,"(hash-set-count #(1) 1)");
	}

	public void testHashSetGetAny() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(hash-set-get-any (hash-set 1) (lambda () 2))", 1);
		eqi  (l,"(hash-set-get-any (hash-set) (lambda () 2))", 2);
		equal(l,"(hash-set-get-any (hash-set))", F);
	}

	public void testIsHashSetEmpty() {
		Scheme l = Scheme.newInstance();

		equal(l,"(hash-set-empty? (hash-set))", T);
		equal(l,"(hash-set-empty? (hash-set 1))", F);
	}

	public void testHashSetToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(hash-set->list (hash-set 1))", list(1));
		equal(l,"(hash-set->list (hash-set))", list());
	}

	public void testHashSetClear() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set-empty? (hash-set-clear hs))", T);
		equal(l,"(hash-set-empty? hs)", F);
	}

	public void testHashSetClearS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set-empty? (hash-set-clear! hs))", T);
		equal(l,"(hash-set-empty? hs)", T);
	}

	public void testIsHashSetEqual() {
		Scheme l = Scheme.newInstance();

		equal(l,"(hash-set= eqv? (hash-set 1 2) (hash-set 2 1))", T);
		equal(l,"(hash-set= eqv? (hash-set 1 2) (hash-set 2 3))", F);
		equal(l,"(hash-set= eqv? (hash-set 1 2) (hash-set 2))", F);
		equal(l,"(hash-set= eqv? (hash-set 1 2) (hash-set 1 2 3))", F);
		equal(l,"(hash-set= eqv? (hash-set 1 2 3)" +
				"           (hash-set 2 1 3)" +
				"           (hash-set 3 2 1))", T);
		equal(l,"(hash-set= equal? (hash-set '(1 2) 3)" +
				"           (hash-set '(1 2) 3))", T);
		equal(l,"(hash-set= eqv? (hash-set equal? '(1 2) 3)" +
				"           (hash-set '(1 2) 3))", F);
		equal(l,"(hash-set= eqv? (hash-set) (make-hash-set))", T);
		equal(l,"(hash-set= eqv? (hash-set 1 2))", T);
		equal(l,"(hash-set= eqv? (hash-set 1 2) #(1 2))", T);
		equal(l,"(hash-set= eqv? (hash-set 1 2) '(1 2))", T);
		lperr(l,"(hash-set= eqv? '(1 2) (hash-set 1 2))");
	}

	public void testHashSetCopy() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs1 (hash-set 1 2))");
		l.exec ("(define hs2 (hash-set-copy hs1))");
		equal(l,"(hash-set= eqv? hs1 hs2)", T);
		equal(l,"(eq? hs1 hs2)", F);
		l.exec ("(define hs3 (hash-set equal? '(1 2) 3))");
		l.exec ("(define hs4 (hash-set-copy hs3))");
		equal(l,"(hash-set= eqv? hs3 hs4)", T);
		equal(l,"(eq? hs3 hs4)", F);
	}

	public void testIsSet() {
		Scheme l = Scheme.newInstance();

		equal(l,"(set? (hash-set))", T);
		equal(l,"(set? #(1))", F);
	}

	public void testIsHashSetContains() {
		Scheme l = Scheme.newInstance();

		equal(l,"(hash-set-contains? (hash-set 1 2 3) 1)", T);
		equal(l,"(hash-set-contains? (hash-set 1 2 3) 0)", F);
		equal(l,"(hash-set-contains? (hash-set) 1)", F);
		equal(l,"(hash-set-contains? (hash-set '(1 2 3)) '(1 2 3))", F);
		equal(l,"(hash-set-contains? (hash-set equal? '(1 2 3)) '(1 2 3))", T);
		equal(l,"(hash-set-contains? (hash-set string-ci=? \"a\") \"A\")", F);
		equal(l,"(hash-set-contains?" +
				" (hash-set string-ci=? string-ci-hash \"a\") \"A\")", T);
		lperr(l,"(hash-set-count #(1) 1)");
	}

	public void testIsHashSetSubset() {
		Scheme l = Scheme.newInstance();

		equal(l,"(hash-set-subset? (hash-set 1 2) (hash-set 2 1))", T);
		equal(l,"(hash-set-subset? (hash-set 1 2) (hash-set 2 3))", F);
		equal(l,"(hash-set-subset? (hash-set 1 2) (hash-set 2))", F);
		equal(l,"(hash-set-subset? (hash-set 1 2) (hash-set 1 2 3))", T);
		equal(l,"(hash-set-subset? (hash-set 1 2 3)" +
				"           (hash-set 2 1 3)" +
				"           (hash-set 3 2 1))", T);
		equal(l,"(hash-set-subset? (hash-set 1 2)" +
				"           (hash-set 2 1 3)" +
				"           (hash-set 4 2 1))", T);
		equal(l,"(hash-set-subset? (hash-set 1 2)" +
				"           (hash-set 2 1 3)" +
				"           (hash-set 3 4 1))", F);
		equal(l,"(hash-set-subset?" +
				"           (hash-set equal? '(1 2) 3)" +
				"           (hash-set 3 '(1 2)))", T);
		equal(l,"(hash-set-subset?" +
				"           (hash-set eqv? '(1 2) 3)" +
				"           (hash-set '(1 2) 3))", F);
		equal(l,"(hash-set-subset? (hash-set) (make-hash-set))", T);
		equal(l,"(hash-set-subset? (hash-set 1 2))", T);
	}

	public void testHashSetAdd() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv? (hash-set-add hs 3) (hash-set 1 2 3))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2))", T);
	}

	public void testHashSetAddS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv? (hash-set-add! hs 3) (hash-set 1 2 3))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2 3))", T);
	}

	public void testHashSetDelete() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2 3))");
		equal(l,"(hash-set= eqv? (hash-set-delete hs 3) (hash-set 1 2))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2 3))", T);
	}

	public void testHashSetDeleteS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2 3))");
		equal(l,"(hash-set= eqv? (hash-set-delete! hs 3) (hash-set 1 2))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2))", T);
	}

	public void testHashSetUnion() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-union hs (hash-set 2 3 4))" +
				" (hash-set 1 2 3 4))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2))", T);
	}

	public void testHashSetUnionS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-union! hs (hash-set 2 3 4))" +
				" (hash-set 1 2 3 4))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2 3 4))", T);
	}

	public void testHashSetIntersection() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-intersection hs (hash-set 2 3 4))" +
				" (hash-set 2))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2))", T);
	}

	public void testHashSetIntersectionS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-intersection! hs (hash-set 2 3 4))" +
				" (hash-set 2))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 2))", T);
	}

	public void testHashSetDifference() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-difference hs (hash-set 2 3 4))" +
				" (hash-set 1))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2))", T);
	}

	public void testHashSetDifferenceS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-difference! hs (hash-set 2 3 4))" +
				" (hash-set 1))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1))", T);
	}

	public void testHashSetSymmetricDifference() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-symmetric-difference hs (hash-set 2 3 4))" +
				" (hash-set 1 3 4))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2))", T);
	}

	public void testHashSetSymmetricDifferenceS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-symmetric-difference! hs (hash-set 2 3 4))" +
				" (hash-set 1 3 4))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 3 4))", T);
	}

	public void testHashSetAddFrom() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-add-from hs '(2 3 4 3 2))" +
				" (hash-set 1 2 3 4))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2))", T);
	}

	public void testHashSetAddFromS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-add-from! hs '(2 3 4 3 2))" +
				" (hash-set 1 2 3 4))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2 3 4))", T);
	}

	public void testHashSetDeleteFrom() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-delete-from hs '(2 3 4 3 2))" +
				" (hash-set 1))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1 2))", T);
	}

	public void testHashSetDeleteFromS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define hs (hash-set 1 2))");
		equal(l,"(hash-set= eqv?" +
				" (hash-set-delete-from! hs '(2 3 4 3 2))" +
				" (hash-set 1))", T);
		equal(l,"(hash-set= eqv? hs (hash-set 1))", T);
	}

}
