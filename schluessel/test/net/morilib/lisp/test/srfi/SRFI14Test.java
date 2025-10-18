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
 * @author MORIGUCHI, Yuichiro 2011/11/13
 */
public class SRFI14Test extends TCSubr {

	public void testIsCharSet() {
		Scheme l = Scheme.newInstance();

		equal(l, "(char-set? #[a-z])", T);
		equal(l, "(char-set? 1)", F);
	}

	public void testCharSetEqual() {
		Scheme l = Scheme.newInstance();

		equal(l, "(char-set= #[a-z] #[a-z])", T);
		equal(l, "(char-set= #[a-z] #[a-x])", F);
		equal(l, "(char-set= #[a-z] #[a-z] #[a-z])", T);
		equal(l, "(char-set= #[a-z] #[a-z] #[a-x])", F);
		equal(l, "(char-set= #[a-z])", T);
		equal(l, "(char-set=)", T);
		lperr(l, "(char-set= 1)");
	}

	public void testCharSetLessEq() {
		Scheme l = Scheme.newInstance();

		equal(l, "(char-set<= #[a-x] #[a-z])", T);
		equal(l, "(char-set<= #[a-z] #[a-z])", T);
		equal(l, "(char-set<= #[a-z] #[a-x])", F);
		equal(l, "(char-set<= #[a-x] #[a-z] #[ab-yzA-Z])", T);
		equal(l, "(char-set<= #[a-x] #[a-z] #[a-x])", F);
		equal(l, "(char-set<= #[a-z])", T);
		equal(l, "(char-set<=)", T);
		lperr(l, "(char-set<= 1)");
	}

	public void testCharSetHash() {
		Scheme l = Scheme.newInstance();

		equal(l, "(= (char-set-hash #[a-z]) (char-set-hash #[a-z]))", T);
		equal(l,"(eqv? #[a-z] #[a-z])", T);
	}

	public void testCharSetCursor() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define cst #[abce])");
		l.exec ("(define cur (char-set-cursor cst))");
		equal(l,"(end-of-char-set? cur)", F);
		equal(l,"(char-set-ref cst cur)", chr('a'));
		l.exec ("(char-set-cursor-next cst cur)");
		equal(l,"(end-of-char-set? cur)", F);
		equal(l,"(char-set-ref cst cur)", chr('b'));
		l.exec ("(char-set-cursor-next cst cur)");
		equal(l,"(end-of-char-set? cur)", F);
		equal(l,"(char-set-ref cst cur)", chr('c'));
		l.exec ("(char-set-cursor-next cst cur)");
		equal(l,"(end-of-char-set? cur)", F);
		equal(l,"(char-set-ref cst cur)", chr('e'));
		l.exec ("(char-set-cursor-next cst cur)");
		equal(l,"(end-of-char-set? cur)", T);
		lperr(l,"(char-set-ref cst cur)");
		lperr(l,"(char-set-cursor-next cst cur)");
	}

	public void testCharSetFold() {
		Scheme l = Scheme.newInstance();

		equal(l,"(char-set-fold cons '() #[aby])", list('y', 'b', 'a'));
		lperr(l,"(char-set-fold 1 '() #[abc])");
		lperr(l,"(char-set-fold cons '() 1)");
	}

	public void testCharSetUnfold() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (char-set-unfold null? car cdr '(#\\a #\\b #\\y)) #[aby])", T);
		equal(l,"(eqv? (char-set-unfold null? car cdr '(#\\a #\\b #\\y) #[c]) #[abcy])", T);
//		lperr(l,"(char-set-unfold 1 car cdr '() #[a])");
//		lperr(l,"(char-set-unfold null? 1 cdr '() #[a])");
//		lperr(l,"(char-set-unfold null? car 1 '() #[a])");
//		lperr(l,"(char-set-unfold null? car cdr '() 1)");
	}

	public void testCharSetForEach() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s '())");
		l.exec ("(char-set-for-each (lambda (x) (set! s (cons x s))) #[aby])");
		equal(l,"s", list('y', 'b', 'a'));
//		lperr(l,"(char-set-for-each 1 #[a])");
//		lperr(l,"(char-set-for-each car 1)");
	}

	public void testCharSetMap() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (char-set-map char-downcase #[ABY]) #[aby])", T);
//		lperr(l,"(char-set-map 1 #[a])");
//		lperr(l,"(char-set-map car 1)");
	}

	public void testCharSetCopy() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[aby])");
		l.exec ("(define t (char-set-copy s))");
		equal(l,"(eqv? s t)", T);
		equal(l,"(eq? s t)", F);
		lperr(l,"(char-set-copy 1)");
	}

	public void testCharSet() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (char-set #\\a #\\b #\\y) #[aby])", T);
		equal(l,"(eqv? (char-set) #[])", T);
		lperr(l,"(char-set #\\a 1 #\\b)");
	}

	public void testListToCharSet() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[c])");
		equal(l,"(eqv? (list->char-set '(#\\a #\\b #\\y)) #[aby])", T);
		equal(l,"(eqv? (list->char-set '(#\\a #\\b #\\y) s) #[abcy])", T);
		l.exec ("(list->char-set! '(#\\a #\\b #\\y) s)");
		equal(l,"(eqv? s #[abcy])", T);
		lperr(l,"(list->char-set '(1))");
		lperr(l,"(list->char-set '(#\\a) 1)");
	}

	public void testStringToCharSet() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[c])");
		equal(l,"(eqv? (string->char-set \"ayb\") #[aby])", T);
		equal(l,"(eqv? (string->char-set \"ayb\" s) #[abcy])", T);
		l.exec ("(string->char-set! \"ayb\" s)");
		equal(l,"(eqv? s #[abcy])", T);
		lperr(l,"(string->char-set '(1))");
		lperr(l,"(string->char-set \"a\" 1)");
	}

	public void testCharSetFilter() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[c])");
		equal(l,"(eqv? (char-set-filter char-lower-case? #[abByY]) #[aby])", T);
		equal(l,"(eqv? (char-set-filter char-lower-case? #[abByY] s) #[abcy])", T);
		l.exec ("(char-set-filter! char-lower-case? #[abByY] s)");
		equal(l,"(eqv? s #[abcy])", T);
		lperr(l,"(char-set-filter 1 '(1))");
		lperr(l,"(char-set-filter char-lower-case? 1)");
		lperr(l,"(char-set-filter char-lower-case? #[ab] 1)");
	}

	public void testUcsRangeToCharSet() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[c])");
		equal(l,"(eqv? (ucs-range->char-set 65 90) #[A-Z])", T);
		equal(l,"(eqv? (ucs-range->char-set 65 90 #f s) #[A-Zc])", T);
		l.exec ("(ucs-range->char-set! 65 90 #f s)");
		equal(l,"(eqv? s #[A-Zc])", T);
		lperr(l,"(ucs-range->char-set 65a 90)");
		lperr(l,"(ucs-range->char-set 65 90a)");
		lperr(l,"(ucs-range->char-set 65 90 #f 1)");
	}

	public void testToCharSet() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (->char-set \"aby\") #[aby])", T);
		equal(l,"(eqv? (->char-set #\\a) #[a])", T);
		equal(l,"(eqv? (->char-set #[ayb]) #[aby])", T);
		lperr(l,"(->char-set 1)");
	}

	public void testCharSetSize() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(char-set-size #[aby])", 3);
		eqi  (l,"(char-set-size #[])", 0);
		lperr(l,"(char-set-size 1)");
	}

	public void testCharSetCount() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(char-set-count char-lower-case? #[aAbByY])", 3);
		eqi  (l,"(char-set-count char-lower-case? #[ABY])", 0);
		lperr(l,"(char-set-count 1 #[aby])");
		lperr(l,"(char-set-count char-lower-case? 1)");
	}

	public void testCharSetToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(char-set->list #[ayb])", list('a', 'b', 'y'));
		equal(l,"(char-set->list #[])", list());
		lperr(l,"(char-set->list 1)");
	}

	public void testCharSetToString() {
		Scheme l = Scheme.newInstance();

		equal(l,"(char-set->string #[ayb])", str("aby"));
		equal(l,"(char-set->string #[])", str(""));
		lperr(l,"(char-set->string 1)");
	}

	public void testIsCharSetContains() {
		Scheme l = Scheme.newInstance();

		equal(l,"(char-set-contains? #[aby] #\\a)", T);
		equal(l,"(char-set-contains? #[aby] #\\c)", F);
		equal(l,"(char-set-contains? #[] #\\c)", F);
		lperr(l,"(char-set-contains? 1 #\\c)");
		lperr(l,"(char-set-contains? #[a] 1)");
	}

	public void testCharSetEveryAny() {
		Scheme l = Scheme.newInstance();

		equal(l,"(char-set-every char-lower-case? #[abc])", T);
		equal(l,"(char-set-every char-lower-case? #[abC])", F);
		equal(l,"(char-set-every char-lower-case? #[ABC])", F);
		equal(l,"(char-set-any   char-lower-case? #[abc])", T);
		equal(l,"(char-set-any   char-lower-case? #[abC])", T);
		equal(l,"(char-set-any   char-lower-case? #[ABC])", F);
		lperr(l,"(char-set-every 1 #[ABC])");
		lperr(l,"(char-set-any   1 #[abc])");
		lperr(l,"(char-set-every char-lower-case? 1)");
		lperr(l,"(char-set-any   char-lower-case? 1)");
	}

	public void testCharSetAdjoin() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[c])");
		equal(l,"(eqv? (char-set-adjoin  s #\\a #\\b) #[abc])", T);
		equal(l,"(eqv? (char-set-adjoin  s) #[c])", T);
		l.exec ("(char-set-adjoin! s #\\a #\\b)");
		equal(l,"(eqv? s #[abc])", T);
		lperr(l,"(char-set-adjoin)");
		lperr(l,"(char-set-adjoin 1 #\\a)");
		lperr(l,"(char-set-adjoin s 1)");
	}

	public void testCharSetDelete() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[abcz])");
		equal(l,"(eqv? (char-set-delete  s #\\a) #[bcz])", T);
		equal(l,"(eqv? (char-set-delete  s #\\b) #[acz])", T);
		equal(l,"(eqv? (char-set-delete  s #\\c) #[abz])", T);
		equal(l,"(eqv? (char-set-delete  s #\\z) #[abc])", T);
		equal(l,"(eqv? (char-set-delete  s) #[abcz])", T);
		l.exec ("(char-set-delete! s #\\a #\\b)");
		equal(l,"(eqv? s #[cz])", T);
		lperr(l,"(char-set-delete)");
		lperr(l,"(char-set-delete 1 #\\a)");
		lperr(l,"(char-set-delete s 1)");
	}

	public void testCharSetComplement() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[abc])");
		equal(l,"(eqv? (char-set-complement (char-set-complement s)) s)", T);
		l.exec ("(char-set-complement! s)");
		equal(l,"(char-set-contains? s #\\d)", T);
		lperr(l,"(char-set-complement 1)");
	}

	public void testCharSetUnion() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[abc])");
		equal(l,"(eqv? (char-set-union s #[c-x]) #[a-x])", T);
		equal(l,"(eqv? (char-set-union s #[c-x] #[A-Z]) #[A-Za-x])", T);
		equal(l,"(eqv? (char-set-union s) s)", T);
		equal(l,"(eqv? (char-set-union) #[])", T);
		l.exec ("(char-set-union! s #[c-x])");
		equal(l,"(eqv? s #[a-x])", T);
		lperr(l,"(char-set-union s 1 #[])");
	}

	public void testCharSetIntersection() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[a-zA-Z])");
		equal(l,"(eqv? (char-set-intersection s #[c-x]) #[c-x])", T);
		equal(l,"(eqv? (char-set-intersection s #[c-x] #[a-d]) #[cd])", T);
		equal(l,"(eqv? (char-set-intersection s) s)", T);
//		equal(l,"(eqv? (char-set-intersection) #[])", T);
		l.exec ("(char-set-intersection! s #[c-x])");
		equal(l,"(eqv? s #[c-x])", T);
		lperr(l,"(char-set-intersection s 1 #[])");
	}

	public void testCharSetXor() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[a-dA-Z])");
		equal(l,"(eqv? (char-set-xor s #[c-x]) #[abe-xA-Z])", T);
		equal(l,"(eqv? (char-set-xor s #[c-x] #[C-X]) #[abe-xABYZ])", T);
		equal(l,"(eqv? (char-set-xor s) s)", T);
		equal(l,"(eqv? (char-set-xor) #[])", T);
		l.exec ("(char-set-xor! s #[c-x])");
		equal(l,"(eqv? s #[abe-xA-Z])", T);
		lperr(l,"(char-set-xor s 1 #[])");
	}

	public void testCharSetDifference() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[a-z])");
		equal(l,"(eqv? (char-set-difference s #[c-x]) #[abyz])", T);
		equal(l,"(eqv? (char-set-difference s #[c-x] #[a-d]) #[yz])", T);
		equal(l,"(eqv? (char-set-difference s) s)", T);
		l.exec ("(char-set-difference! s #[c-x])");
		equal(l,"(eqv? s #[abyz])", T);
		lperr(l,"(char-set-difference)");
		lperr(l,"(char-set-difference s 1 #[])");
	}

	public void testCharSetDiffPlusIntersection() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define s #[a-z])");
		l.exec ("(define t #[c-x])");
		equal(l,"(equal? (multi-values->list (char-set-diff+intersection s t))" +
				" '(#[abyz] #[c-x]))", T);
		equal(l,"(equal? (multi-values->list (char-set-diff+intersection s t #[a-d]))" +
				" '(#[yz] #[a-x])) ", T);
		l.exec ("(char-set-diff+intersection! s t)");
		equal(l,"(eqv? s #[abyz])", T);
		equal(l,"(eqv? t #[c-x])", T);
		lperr(l,"(char-set-diff+intersection)");
		lperr(l,"(char-set-diff+intersection! s)");
		lperr(l,"(char-set-diff+intersection s 1 #[])");
	}

}
