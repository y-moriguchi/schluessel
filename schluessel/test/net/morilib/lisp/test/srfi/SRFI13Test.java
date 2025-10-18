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

import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/16
 */
public class SRFI13Test extends TCSubr {

	//
	private void equal(Scheme l, String scr, String s) {
		equal(l, scr, new LispString(s));
	}

	public void testIsStringNull() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-null? \"\")", T);
		equal(l,"(string-null? \"a\")", F);
	}

	public void testStringEvery() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-every #\\a \"aaaaa\")", T);
		equal(l,"(string-every #\\a \"aabaa\")", F);
		equal(l,"(string-every #\\a \"aabaa\" 3)", T);
		equal(l,"(string-every #\\a \"aabaa\" 0 2)", T);
		equal(l,"(string-every #[a-z] \"aabaa\")", T);
		equal(l,"(string-every #[a-z] \"aa0aa\")", F);
		equal(l,"(string-every #[a-z] \"ab0aa\" 3)", T);
		equal(l,"(string-every #[a-z] \"ab0ba\" 0 2)", T);
		equal(l,"(string-every (lambda (x) (char=? #\\a x)) \"aaaaa\")", T);
		equal(l,"(string-every (lambda (x) (char=? #\\a x)) \"aabaa\")", F);
		equal(l,"(string-every (lambda (x) (char=? #\\a x)) \"aabaa\" 3)", T);
		equal(l,"(string-every (lambda (x) (char=? #\\a x)) \"aabaa\" 0 2)", T);
		lperr(l,"(string-every)");
		lperr(l,"(string-every #\\a)");
		lperr(l,"(string-every 1 \"a\")");
	}

	public void testStringAny() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-any #\\a \"abcde\")", T);
		equal(l,"(string-any #\\a \"bcdef\")", F);
		equal(l,"(string-any #\\a \"abbba\" 3)", T);
		equal(l,"(string-any #\\a \"abbba\" 0 2)", T);
		equal(l,"(string-any #[a-z] \"01b23\")", T);
		equal(l,"(string-any #[a-z] \"12345\")", F);
		equal(l,"(string-any #[a-z] \"a101a\" 3)", T);
		equal(l,"(string-any #[a-z] \"a201a\" 0 2)", T);
		equal(l,"(string-any (lambda (x) (char=? #\\a x)) \"abcde\")", T);
		equal(l,"(string-any (lambda (x) (char=? #\\a x)) \"bcdef\")", F);
		equal(l,"(string-any (lambda (x) (char=? #\\a x)) \"abbba\" 3)", T);
		equal(l,"(string-any (lambda (x) (char=? #\\a x)) \"abbba\" 0 2)", T);
		lperr(l,"(string-any)");
		lperr(l,"(string-any #\\a)");
		lperr(l,"(string-any 1 \"a\")");
	}

	public void testStringTabulate() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-tabulate (lambda (x) (integer->char (+ x 40))) 5)",
				"()*+,");
		lperr(l,"(string-tabulate char?)");
	}

	public void testStringToList() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string->list \"abcde\" 2)", list('c', 'd', 'e'));
		equal(l,"(string->list \"abcde\" 2 4)", list('c', 'd'));
		lperr(l,"(string->list \"abcde\" 5)");
		lperr(l,"(string->list \"abcde\" 2 6)");
		lperr(l,"(string->list \"abcde\" 3 2)");
	}

	public void testReverseListToString() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(reverse-list->string '(#\\a #\\B #\\c))", "cBa");
	}

	public void testStringJoin() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-join '(\"a\" \"b\" \"c\"))", "a b c");
		equal(l,"(string-join '(\"a\" \"b\" \"c\") \":\")", "a:b:c");
		equal(l,"(string-join '(\"a\" \"b\" \"c\") \":\" 'suffix)",
				"a:b:c:");
		equal(l,"(string-join '(\"a\" \"b\" \"c\") \":\" 'prefix)",
				":a:b:c");
		equal(l,"(string-join '(\"a\" \"b\" \"c\") \":\" 'strict-infix)",
				"a:b:c");
		equal(l,"(string-join '(\"a\" \"b\" \"\") \":\")", "a:b:");
		equal(l,"(string-join '(\"a\" \"b\" \"\") \":\" 'suffix)",
				"a:b::");
		equal(l,"(string-join '(\"a\" \"b\" \"\") \":\" 'prefix)",
				":a:b:");
		equal(l,"(string-join '(\"a\" \"b\" \"\") \":\" 'strict-infix)",
				"a:b:");
		equal(l,"(string-join '(\"\") \":\" 'infix)", "");
		equal(l,"(string-join '(\"\") \":\" 'strict-infix)", "");
		equal(l,"(string-join '() \":\" 'infix)", "");
		lperr(l,"(string-join '() \":\" 'strict-infix)");
		lperr(l,"(string-join '(\"\") \":\" 'invaild)");
	}

	public void testStringCopy() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-copy \"abcde\")", "abcde");
		equal(l,"(string-copy \"abcde\" 2)", "cde");
		equal(l,"(string-copy \"abcde\" 2 4)", "cd");
	}

	public void testSubstringShared() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(substring/shared \"abcde\")", "abcde");
		equal(l,"(substring/shared \"abcde\" 2)", "cde");
		equal(l,"(substring/shared \"abcde\" 2 4)", "cd");
	}

	public void testStringCopyS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define s (string-copy \"aaaaa\"))");
		l.exec ("(string-copy! s 0 \"abcd\")");
		equal(l,"s", "abcda");
		l.exec ("(string-copy! s 2 \"1111\" 2)");
		equal(l,"s", "ab11a");
		l.exec ("(string-copy! s 1 \"12211\" 1 3)");
		equal(l,"s", "a221a");
		lperr(l,"(string-copy! s 0 \"11111111\")");
	}

	public void testStringTake() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-take \"0123456789\" 3)", "012");
	}

	public void testStringDrop() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-drop \"0123456789\" 3)", "3456789");
	}

	public void testStringTakeRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-take-right \"0123456789\" 3)", "789");
	}

	public void testStringDropRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-drop-right \"0123456789\" 3)", "0123456");
	}

	public void testStringPad() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-pad     \"325\" 5)", "  325");
		equal(l,"(string-pad   \"71325\" 5)", "71325");
		equal(l,"(string-pad \"8871325\" 5)", "71325");
		equal(l,"(string-pad     \"325\" 5 #\\0)", "00325");
		equal(l,"(string-pad \"8871325\" 5 #\\0 4)", "00325");
		equal(l,"(string-pad \"8871325\" 5 #\\0 4 6)", "00032");
		equal(l,"(string-pad \"8871325\" 3 #\\0 2 7)", "325");
	}

	public void testStringPadRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-pad-right     \"325\" 5)", "325  ");
		equal(l,"(string-pad-right   \"71325\" 5)", "71325");
		equal(l,"(string-pad-right \"8871325\" 5)", "88713");
		equal(l,"(string-pad-right     \"325\" 5 #\\0)", "32500");
		equal(l,"(string-pad-right \"8871325\" 5 #\\0 4)", "32500");
		equal(l,"(string-pad-right \"8871325\" 5 #\\0 4 6)", "32000");
		equal(l,"(string-pad-right \"8871325\" 3 #\\0 2 7)", "713");
	}

	public void testStringTrim() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-trim \" 012-aaaa-210 \")", "012-aaaa-210 ");
		equal(l,"(string-trim \" 012-aaaa-210 \" #[ 0-5])", "-aaaa-210 ");
		equal(l,"(string-trim \" 012-aaaa-210 \"" +
				"  (lambda (x) (eqv? x #\\space)))", "012-aaaa-210 ");
		equal(l,"(string-trim \" 012-aaaa-210 \" #\\- 4)", "aaaa-210 ");
		equal(l,"(string-trim \" 012-aaaa-210 \" #\\- 4 10)", "aaaa-");
	}

	public void testStringTrimRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-trim-right \" 012-aaaa-210 \")", " 012-aaaa-210");
		equal(l,"(string-trim-right \" 012-aaaa-210 \" #[ 0-5])", " 012-aaaa-");
		equal(l,"(string-trim-right \" 012-aaaa-210 \"" +
				"  (lambda (x) (eqv? x #\\space)))", " 012-aaaa-210");
		equal(l,"(string-trim-right \" 012-aaaa-210 \" #\\space 4)", "-aaaa-210");
		equal(l,"(string-trim-right \" 012-aaaa-210 \" #\\- 4 10)", "-aaaa");
	}

	public void testStringBoth() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-trim-both \" 012-aaaa-210 \")", "012-aaaa-210");
		equal(l,"(string-trim-both \" 012-aaaa-210 \" #[ 0-5])", "-aaaa-");
		equal(l,"(string-trim-both \" 012-aaaa-210 \"" +
				"  (lambda (x) (eqv? x #\\space)))", "012-aaaa-210");
		equal(l,"(string-trim-both \" 012-aaaa-210 \" #[- 0-5] 4)", "aaaa");
		equal(l,"(string-trim-both \" 012-aaaa-210 \" #\\- 4 10)", "aaaa");
	}

	public void testStringFillS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define s (string-copy \"aaaaa\"))");
		l.exec ("(string-fill! s #\\-)");
		equal(l,"s", "-----");
		l.exec ("(string-fill! s #\\+ 2)");
		equal(l,"s", "--+++");
		l.exec ("(string-fill! s #\\0 2 4)");
		equal(l,"s", "--00+");
	}

	public void testStringCompare() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define (z x) x)");
		l.exec ("(define (e x) (error \"error\"))");
		eqi  (l,"(string-compare \"aaa\" \"aab\" z e e)", 2);
		eqi  (l,"(string-compare \"aaa\" \"aaa\" e z e)", 3);
		eqi  (l,"(string-compare \"aba\" \"aaa\" e e z)", 1);
		eqi  (l,"(string-compare \"aa\" \"aaa\" z e e)", 2);
		eqi  (l,"(string-compare \"aaa\" \"aa\" e e z)", 2);
		eqi  (l,"(string-compare \"zaaa\" \"aab\" z e e 1 4 0 3)", 3);
		eqi  (l,"(string-compare \"aaa\" \"zaaa\" e z e 0 3 1 4)", 3);
		eqi  (l,"(string-compare \"aba\" \"zaaa\" e e z 0 3 1 4)", 1);
	}

	public void testStringCompareCi() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define (z x) x)");
		l.exec ("(define (e x) (error \"error\"))");
		eqi  (l,"(string-compare-ci \"aaa\" \"aaB\" z e e)", 2);
		eqi  (l,"(string-compare-ci \"aAa\" \"aaa\" e z e)", 3);
		eqi  (l,"(string-compare-ci \"aBa\" \"aaa\" e e z)", 1);
		eqi  (l,"(string-compare-ci \"aa\" \"Aaa\" z e e)", 2);
		eqi  (l,"(string-compare-ci \"Aaa\" \"aa\" e e z)", 2);
		eqi  (l,"(string-compare-ci \"zaaa\" \"aaB\" z e e 1 4 0 3)", 3);
		eqi  (l,"(string-compare-ci \"aaa\" \"zaAa\" e z e 0 3 1 4)", 3);
		eqi  (l,"(string-compare-ci \"aBa\" \"zaaa\" e e z 0 3 1 4)", 1);
	}

	public void testStringCompare2() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define (z x) x)");
		l.exec ("(define (e x) (error \"error\"))");
		// <
		eqv  (l,"(string< \"aaa\" \"aab\")", T);
		eqv  (l,"(string< \"aaa\" \"aaa\")", F);
		eqv  (l,"(string< \"aba\" \"aaa\")", F);
		eqv  (l,"(string< \"aa\" \"aaa\")", T);
		eqv  (l,"(string< \"aaa\" \"aa\")", F);
		eqv  (l,"(string< \"zaaa\" \"aab\"1 4 0 3)", T);
		eqv  (l,"(string< \"aaa\" \"zaaa\"0 3 1 4)", F);
		eqv  (l,"(string< \"aba\" \"zaaa\"0 3 1 4)", F);
		// >
		eqv  (l,"(string> \"aaa\" \"aab\")", F);
		eqv  (l,"(string> \"aaa\" \"aaa\")", F);
		eqv  (l,"(string> \"aba\" \"aaa\")", T);
		eqv  (l,"(string> \"aa\" \"aaa\")", F);
		eqv  (l,"(string> \"aaa\" \"aa\")", T);
		eqv  (l,"(string> \"zaaa\" \"aab\"1 4 0 3)", F);
		eqv  (l,"(string> \"aaa\" \"zaaa\"0 3 1 4)", F);
		eqv  (l,"(string> \"aba\" \"zaaa\"0 3 1 4)", T);
		// <=
		eqv  (l,"(string<= \"aaa\" \"aab\")", T);
		eqv  (l,"(string<= \"aaa\" \"aaa\")", T);
		eqv  (l,"(string<= \"aba\" \"aaa\")", F);
		eqv  (l,"(string<= \"aa\" \"aaa\")", T);
		eqv  (l,"(string<= \"aaa\" \"aa\")", F);
		eqv  (l,"(string<= \"zaaa\" \"aab\"1 4 0 3)", T);
		eqv  (l,"(string<= \"aaa\" \"zaaa\"0 3 1 4)", T);
		eqv  (l,"(string<= \"aba\" \"zaaa\"0 3 1 4)", F);
		// >=
		eqv  (l,"(string>= \"aaa\" \"aab\")", F);
		eqv  (l,"(string>= \"aaa\" \"aaa\")", T);
		eqv  (l,"(string>= \"aba\" \"aaa\")", T);
		eqv  (l,"(string>= \"aa\" \"aaa\")", F);
		eqv  (l,"(string>= \"aaa\" \"aa\")", T);
		eqv  (l,"(string>= \"zaaa\" \"aab\"1 4 0 3)", F);
		eqv  (l,"(string>= \"aaa\" \"zaaa\"0 3 1 4)", T);
		eqv  (l,"(string>= \"aba\" \"zaaa\"0 3 1 4)", T);
		// =
		eqv  (l,"(string= \"aaa\" \"aab\")", F);
		eqv  (l,"(string= \"aaa\" \"aaa\")", T);
		eqv  (l,"(string= \"aba\" \"aaa\")", F);
		eqv  (l,"(string= \"aa\" \"aaa\")", F);
		eqv  (l,"(string= \"aaa\" \"aa\")", F);
		eqv  (l,"(string= \"zaaa\" \"aab\"1 4 0 3)", F);
		eqv  (l,"(string= \"aaa\" \"zaaa\"0 3 1 4)", T);
		eqv  (l,"(string= \"aba\" \"zaaa\"0 3 1 4)", F);
		// <>
		eqv  (l,"(string<> \"aaa\" \"aab\")", T);
		eqv  (l,"(string<> \"aaa\" \"aaa\")", F);
		eqv  (l,"(string<> \"aba\" \"aaa\")", T);
		eqv  (l,"(string<> \"aa\" \"aaa\")", T);
		eqv  (l,"(string<> \"aaa\" \"aa\")", T);
		eqv  (l,"(string<> \"zaaa\" \"aab\"1 4 0 3)", T);
		eqv  (l,"(string<> \"aaa\" \"zaaa\"0 3 1 4)", F);
		eqv  (l,"(string<> \"aba\" \"zaaa\"0 3 1 4)", T);
	}

	public void testStringCompareCi2() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define (z x) x)");
		l.exec ("(define (e x) (error \"error\"))");
		// <
		eqv  (l,"(string-ci< \"aaa\" \"aaB\")", T);
		eqv  (l,"(string-ci< \"aaa\" \"aAa\")", F);
		eqv  (l,"(string-ci< \"aBa\" \"aaa\")", F);
		eqv  (l,"(string-ci< \"aa\" \"aAa\")", T);
		eqv  (l,"(string-ci< \"aAa\" \"aa\")", F);
		eqv  (l,"(string-ci< \"zaaa\" \"aaB\"1 4 0 3)", T);
		eqv  (l,"(string-ci< \"aaa\" \"zaAa\"0 3 1 4)", F);
		eqv  (l,"(string-ci< \"aBa\" \"zaaa\"0 3 1 4)", F);
		// >
		eqv  (l,"(string-ci> \"aaa\" \"aaB\")", F);
		eqv  (l,"(string-ci> \"aaa\" \"aAa\")", F);
		eqv  (l,"(string-ci> \"aBa\" \"aaa\")", T);
		eqv  (l,"(string-ci> \"aa\" \"aAa\")", F);
		eqv  (l,"(string-ci> \"aAa\" \"aa\")", T);
		eqv  (l,"(string-ci> \"zaaa\" \"aaB\"1 4 0 3)", F);
		eqv  (l,"(string-ci> \"aaa\" \"zaAa\"0 3 1 4)", F);
		eqv  (l,"(string-ci> \"aBa\" \"zaaa\"0 3 1 4)", T);
		// <=
		eqv  (l,"(string-ci<= \"aaa\" \"aaB\")", T);
		eqv  (l,"(string-ci<= \"aaa\" \"aAa\")", T);
		eqv  (l,"(string-ci<= \"aBa\" \"aaa\")", F);
		eqv  (l,"(string-ci<= \"aa\" \"Aaa\")", T);
		eqv  (l,"(string-ci<= \"aAa\" \"aa\")", F);
		eqv  (l,"(string-ci<= \"zaaa\" \"aaB\"1 4 0 3)", T);
		eqv  (l,"(string-ci<= \"aaa\" \"zaAa\"0 3 1 4)", T);
		eqv  (l,"(string-ci<= \"aBa\" \"zaaa\"0 3 1 4)", F);
		// >=
		eqv  (l,"(string-ci>= \"aaa\" \"aaB\")", F);
		eqv  (l,"(string-ci>= \"aaa\" \"aAa\")", T);
		eqv  (l,"(string-ci>= \"aBa\" \"aaa\")", T);
		eqv  (l,"(string-ci>= \"aa\" \"Aaa\")", F);
		eqv  (l,"(string-ci>= \"Aaa\" \"aa\")", T);
		eqv  (l,"(string-ci>= \"zaaa\" \"aaB\"1 4 0 3)", F);
		eqv  (l,"(string-ci>= \"aaa\" \"zaAa\"0 3 1 4)", T);
		eqv  (l,"(string-ci>= \"aBa\" \"zaaa\"0 3 1 4)", T);
		// =
		eqv  (l,"(string-ci= \"aaa\" \"aaB\")", F);
		eqv  (l,"(string-ci= \"aaa\" \"aAa\")", T);
		eqv  (l,"(string-ci= \"aBa\" \"aaa\")", F);
		eqv  (l,"(string-ci= \"aa\" \"Aaa\")", F);
		eqv  (l,"(string-ci= \"Aaa\" \"aa\")", F);
		eqv  (l,"(string-ci= \"zaaa\" \"aaB\"1 4 0 3)", F);
		eqv  (l,"(string-ci= \"aaa\" \"zaAa\"0 3 1 4)", T);
		eqv  (l,"(string-ci= \"aBa\" \"zaaa\"0 3 1 4)", F);
		// <>
		eqv  (l,"(string-ci<> \"aaa\" \"aaB\")", T);
		eqv  (l,"(string-ci<> \"aaa\" \"aAa\")", F);
		eqv  (l,"(string-ci<> \"aBa\" \"aaa\")", T);
		eqv  (l,"(string-ci<> \"aa\" \"Aaa\")", T);
		eqv  (l,"(string-ci<> \"Aaa\" \"aa\")", T);
		eqv  (l,"(string-ci<> \"zaaa\" \"aaB\"1 4 0 3)", T);
		eqv  (l,"(string-ci<> \"aaa\" \"zaAa\"0 3 1 4)", F);
		eqv  (l,"(string-ci<> \"aBa\" \"zaaa\"0 3 1 4)", T);
	}

	public void testStringHash() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqv  (l,"(eqv? (string-hash \"aaa\" 3) (string-hash \"aaa\" 3))", T);
		eqv  (l,"(eqv? (string-hash \"aaa\" 3) (string-hash \"zaaa\" 3 1))", T);
		eqv  (l,"(eqv? (string-hash \"aaa\" 3) (string-hash \"aaaz\" 3 0 3))", T);
	}

	public void testStringHashCi() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqv  (l,"(eqv? (string-hash-ci \"aAa\" 3) (string-hash \"aaa\" 3))", T);
		eqv  (l,"(eqv? (string-hash-ci \"aAa\" 3) (string-hash \"zaaa\" 3 1))", T);
		eqv  (l,"(eqv? (string-hash-ci \"aAa\" 3) (string-hash \"aaaz\" 3 0 3))", T);
	}

	public void testStringPrefixLength() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-prefix-length \"pre-l\" \"pre-fix\")", 4);
		eqi  (l,"(string-prefix-length \"pre-l\" \"pre-fix\" 1 5 1 7)", 3);
	}

	public void testStringSuffixLength() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-suffix-length \"l-fix\" \"pre-fix\")", 4);
		eqi  (l,"(string-suffix-length \"l-fix\" \"pre-fix\" 0 4 0 6)", 3);
	}

	public void testStringPrefixLengthCi() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-prefix-length-ci \"pre-l\" \"PRE-fix\")", 4);
		eqi  (l,"(string-prefix-length-ci \"pre-l\" \"PRE-fix\" 1 5 1 7)", 3);
	}

	public void testStringSuffixLengthCi() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-suffix-length-ci \"l-fix\" \"pre-FIX\")", 4);
		eqi  (l,"(string-suffix-length-ci \"l-fix\" \"pre-FIX\" 0 4 0 6)", 3);
	}

	public void testIsStringPrefix() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqv  (l,"(string-prefix? \"pre-\" \"pre-fix\")", T);
		eqv  (l,"(string-prefix? \"pre-l\" \"pre-fix\")", F);
		eqv  (l,"(string-prefix? \"pre-\" \"pre-fix\" 1 4 1 7)", T);
	}

	public void testIsStringSuffix() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqv  (l,"(string-suffix? \"-fix\" \"pre-fix\")", T);
		eqv  (l,"(string-suffix? \"l-fix\" \"pre-fix\")", F);
		eqv  (l,"(string-suffix? \"-fix\" \"pre-fix\" 0 3 0 6)", T);
	}

	public void testIsStringPrefixCi() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqv  (l,"(string-prefix-ci? \"pre-\" \"PRE-fix\")", T);
		eqv  (l,"(string-prefix-ci? \"pre-l\" \"PRE-fix\")", F);
		eqv  (l,"(string-prefix-ci? \"pre-l\" \"PRE-fix\" 1 4 1 7)", T);
	}

	public void testIsStringSuffixCi() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqv  (l,"(string-suffix-ci? \"-fix\" \"pre-FIX\")", T);
		eqv  (l,"(string-suffix-ci? \"l-fix\" \"pre-FIX\")", F);
		eqv  (l,"(string-suffix-ci? \"-fix\" \"pre-FIX\" 0 3 0 6)", T);
	}

	public void testStringIndex() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-index \"abbba\" #\\b)", 1);
		eqi  (l,"(string-index \"abbba\" #[b-x])", 1);
		eqi  (l,"(string-index \"abbba\" (lambda (x) (char=? x #\\b)))", 1);
		eqv  (l,"(string-index \"azzza\" #\\b)", F);
		eqv  (l,"(string-index \"azzza\" #[b-x])", F);
		eqv  (l,"(string-index \"azzza\" (lambda (x) (char=? x #\\b)))", F);
	}

	public void testStringIndexRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-index-right \"abbba\" #\\b)", 3);
		eqi  (l,"(string-index-right \"abbba\" #[b-x])", 3);
		eqi  (l,"(string-index-right \"abbba\" (lambda (x) (char=? x #\\b)))", 3);
		eqv  (l,"(string-index-right \"azzza\" #\\b)", F);
		eqv  (l,"(string-index-right \"azzza\" #[b-x])", F);
		eqv  (l,"(string-index-right \"azzza\" (lambda (x) (char=? x #\\b)))", F);
	}

	public void testStringSkip() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-skip \"abbba\" #\\a)", 1);
		eqi  (l,"(string-skip \"abbba\" #[a])", 1);
		eqi  (l,"(string-skip \"abbba\" (lambda (x) (char=? x #\\a)))", 1);
		eqv  (l,"(string-skip \"zzzzz\" #\\z)", F);
		eqv  (l,"(string-skip \"azzza\" #[a-z])", F);
		eqv  (l,"(string-skip \"zzzzz\" (lambda (x) (char=? x #\\z)))", F);
	}

	public void testStringSkipRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-skip-right \"abbba\" #\\a)", 3);
		eqi  (l,"(string-skip-right \"abbba\" #[a])", 3);
		eqi  (l,"(string-skip-right \"abbba\" (lambda (x) (char=? x #\\a)))", 3);
		eqv  (l,"(string-skip-right \"zzzzz\" #\\z)", F);
		eqv  (l,"(string-skip-right \"azzza\" #[a-z])", F);
		eqv  (l,"(string-skip-right \"zzzzz\" (lambda (x) (char=? x #\\z)))", F);
	}

	public void testStringCount() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-count \"abbba\" #\\a)", 1);
		eqi  (l,"(string-count \"abbba\" #[a])", 1);
		eqi  (l,"(string-count \"abbba\" (lambda (x) (char=? x #\\a)))", 1);
		eqi  (l,"(string-count \"bazzza\" #\\b 1)", 0);
		eqi  (l,"(string-count \"bazzza\" #[b-x] 1)", 0);
		eqi  (l,"(string-count \"bazzza\" (lambda (x) (char=? x #\\b)) 1)", 0);
		eqi  (l,"(string-count \"0zzzzz\" #\\z 1)", 5);
		eqi  (l,"(string-count \"0azzza\" #[a-z] 1)", 5);
		eqi  (l,"(string-count \"0zzzzz\" (lambda (x) (char=? x #\\z)) 1)", 5);
	}

	public void testStringContains() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-contains \"1234512345\" \"234\")", 1);
		eqv  (l,"(string-contains \"1234512345\" \"aaa\")", F);
		eqi  (l,"(string-contains \"1234512345\" \"234\" 5)", 6);
	}

	public void testStringContainsCi() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		eqi  (l,"(string-contains-ci \"abcdeABCDE\" \"BCD\")", 1);
		eqv  (l,"(string-contains-ci \"abcdeABCDE\" \"000\")", F);
		eqi  (l,"(string-contains-ci \"abcdeABCDE\" \"bcd\" 5)", 6);
	}

	public void testStringTitlecase() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-titlecase \"title 3com --oNE, two.\")",
				"Title 3com --One, two.");
	}

	public void testStringTitlecaseS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define s (string-copy \"aatitle 3com --oNE, two.aa\"))");
		l.exec ("(string-titlecase! s 2 24)");
		equal(l,"s", "aaTitle 3com --One, two.aa");
	}

	public void testStringUpcase() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-upcase \"title 3com --oNE, two.\")",
				"TITLE 3COM --ONE, TWO.");
	}

	public void testStringUpcaseS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define s (string-copy \"aatitle 3com --oNE, two.aa\"))");
		l.exec ("(string-upcase! s 2 24)");
		equal(l,"s", "aaTITLE 3COM --ONE, TWO.aa");
	}

	public void testStringDowncase() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-downcase \"title 3com --oNE, two.\")",
				"title 3com --one, two.");
	}

	public void testStringDowncaseS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define s (string-copy \"AAtitle 3com --oNE, two.AA\"))");
		l.exec ("(string-downcase! s 2 24)");
		equal(l,"s", "AAtitle 3com --one, two.AA");
	}

	public void testStringReverse() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-reverse \"Able was I ere I saw elba.\")",
				".able was I ere I saw elbA");
		equal(l,"(string-reverse \"Able was I ee I saw elba.\")",
				".able was I ee I saw elbA");
	}

	public void testStringReverseS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define s (string-copy \"AAAble was I ere I saw elba.aa\"))");
		l.exec ("(string-reverse! s 2 28)");
		equal(l,"s", "AA.able was I ere I saw elbAaa");
	}

	public void testStringConcatenate() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-concatenate '(\"a\" \"b\" \"c\"))", "abc");
	}

	public void testStringConcatenateShared() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-concatenate/shared '(\"a\" \"b\" \"c\"))", "abc");
	}

	public void testStringAppendShared() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-append/shared \"a\" \"b\" \"c\")", "abc");
	}

	public void testStringConcatenateReverse() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-concatenate-reverse '(\"a\" \"b\" \"c\"))", "cba");
	}

	public void testStringConcatenateReverseShared() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-concatenate-reverse/shared '(\"a\" \"b\" \"c\"))", "cba");
	}

	public void testStringMap() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-map (lambda (x) (char-upcase x)) \"abc\")", "ABC");
	}

	public void testStringMapS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define s (string-copy \"abcde\"))");
		l.exec ("(string-map! (lambda (x) (char-upcase x)) s 1 4)");
		equal(l,"s", "aBCDe");
	}

	public void testStringFold() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-fold cons '() \"abc\")", list('c', 'b', 'a'));
	}

	public void testStringFoldRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-fold-right cons '() \"abc\")", list('a', 'b', 'c'));
	}

	public void testStringUnfold() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define (kons x y) (string-append (string x) y))");
		l.exec ("(define (kar  x)   (string-ref    x 0))");
		l.exec ("(define (kdr  x)   (string-drop   x 1))");
		l.exec ("(define knull? string-null?)");
		l.exec ("(define knil \"\")");
		equal(l,"(string-fold-right kons knil" +
				"  (string-unfold knull? kar kdr \"abcde\"))", "abcde");
		equal(l,"(string-unfold knull? kar kdr" +
				"  (string-fold-right kons knil \"abcde\"))", "abcde");
	}

	public void testStringUnfoldRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define (kons x y) (string-append (string x) y))");
		l.exec ("(define (kar  x)   (string-ref    x 0))");
		l.exec ("(define (kdr  x)   (string-drop   x 1))");
		l.exec ("(define knull? string-null?)");
		l.exec ("(define knil \"\")");
		equal(l,"(string-fold kons knil" +
				"  (string-unfold-right knull? kar kdr \"abcde\"))", "abcde");
		equal(l,"(string-unfold-right knull? kar kdr" +
				"  (string-fold kons knil \"abcde\"))", "abcde");
	}

	public void testStringForEach() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define r '())");
		l.exec ("(string-for-each (lambda (x) (set! r (cons x r))) \"abc\")");
		equal(l,"r", list('c', 'b', 'a'));
	}

	public void testStringForEachIndex() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define r '())");
		l.exec ("(string-for-each-index (lambda (x) (set! r (cons x r))) \"abc\")");
		equal(l,"r", list(2, 1, 0));
	}

	public void testXsubstring() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(xsubstring \"abcdef\" 2)","cdefab");
		equal(l,"(xsubstring \"abcdef\" -2)","efabcd");
		equal(l,"(xsubstring \"abc\" 0 7)","abcabca");
	}

	public void testStringXcopyS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define s (string-copy \"0000000000\"))");
		l.exec ("(string-xcopy! s 2 \"abcdef\" 2)");
		equal(l,"s","00cdefab00");
		l.exec ("(string-xcopy! s 2 \"abcdef\" -2)");
		equal(l,"s","00efabcd00");
		l.exec ("(string-xcopy! s 2 \"abc\" 0 7)");
		equal(l,"s","00abcabca0");
	}

	public void testStringReplace() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-replace \"The TCL programmer endured daily ridicule.\"" +
                "  \"another miserable perl drone\" 4 7 8 22)",
                "The miserable perl programmer endured daily ridicule.");
		equal(l,"(string-replace \"It's easy to code it up in Scheme.\"" +
				"  \"lots of fun\" 5 9)",
				"It's lots of fun to code it up in Scheme.");
	}

	public void testStringTokenize() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-tokenize \"Help make programs run, run, RUN!\")",
				list("Help", "make", "programs", "run,", "run,", "RUN!"));
		equal(l,"(string-tokenize \"らき☆すた　けいおん！ 咲ーSakiー\")",
				list("らき☆すた", "けいおん！", "咲ーSakiー"));
	}

	public void testStringFilter() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-filter #\\space \" \ta b c  \")", "     ");
		equal(l,"(string-filter #[ \t] \" \ta b c  \")", " \t    ");
		equal(l,"(string-filter (lambda (x) (char=? x #\\space))" +
				"  \" \ta b c  \")", "     ");
	}

	public void testStringDelete() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-delete #\\space \" \ta b c  \")", "\tabc");
		equal(l,"(string-delete #[ \t] \" \ta b c  \")", "abc");
		equal(l,"(string-delete (lambda (x) (char=? x #\\space))" +
				"  \" \ta b c  \")", "\tabc");
	}

	public void testStringParseStartPlusEnd() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-parse-start+end car \"abc\" '())",
				Nil.NIL,
				LispInteger.valueOf(0),
				LispInteger.valueOf(3));
		equal(l,"(string-parse-start+end car \"abc\" '(1))",
				Nil.NIL,
				LispInteger.valueOf(1),
				LispInteger.valueOf(3));
		equal(l,"(string-parse-start+end car \"abc\" '(1 2))",
				Nil.NIL,
				LispInteger.valueOf(1),
				LispInteger.valueOf(2));
		equal(l,"(string-parse-start+end car \"abc\" '(1 3 3))",
				list(3),
				LispInteger.valueOf(1),
				LispInteger.valueOf(3));
		lperr(l,"(string-parse-start+end car \"abc\" '(3))");
		lperr(l,"(string-parse-start+end car \"abc\" '(1 4))");
	}

	public void testStringParseFinalStartPlusEnd() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(string-parse-final-start+end car \"abc\" '())",
				Nil.NIL,
				LispInteger.valueOf(0),
				LispInteger.valueOf(3));
		equal(l,"(string-parse-final-start+end car \"abc\" '(1))",
				Nil.NIL,
				LispInteger.valueOf(1),
				LispInteger.valueOf(3));
		equal(l,"(string-parse-final-start+end car \"abc\" '(1 2))",
				Nil.NIL,
				LispInteger.valueOf(1),
				LispInteger.valueOf(2));
		lperr(l,"(string-parse-final-start+end car \"abc\" '(1 3 3))");
		lperr(l,"(string-parse-final-start+end car \"abc\" '(3))");
		lperr(l,"(string-parse-final-start+end car \"abc\" '(1 4))");
	}

	public void testLetStringStartPlusEnd() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(let-string-start+end (b e r) car \"abc\" '() (list r b e))",
				list(Nil.NIL, 0, 3));
		equal(l,"(let-string-start+end (b e r) car \"abc\" '(1) (list r b e))",
				list(Nil.NIL, 1, 3));
		equal(l,"(let-string-start+end (b e r) car \"abc\" '(1 3) (list r b e))",
				list(Nil.NIL, 1, 3));
		equal(l,"(let-string-start+end (b e r) car \"abc\" '(1 3 3) (list r b e))",
				list(list(3), 1, 3));
		lperr(l,"(let-string-start+end (b e r) car \"abc\" '(3) (list r b e))");
		lperr(l,"(let-string-start+end (b e r) car \"abc\" '(1 4) (list r b e))");

		equal(l,"(let-string-start+end (b e) car \"abc\" '() (list b e))",
				list(0, 3));
		equal(l,"(let-string-start+end (b e) car \"abc\" '(1) (list b e))",
				list(1, 3));
		equal(l,"(let-string-start+end (b e) car \"abc\" '(1 3) (list b e))",
				list(1, 3));
		lperr(l,"(let-string-start+end (b e) car \"abc\" '(1 3 3) (list b e))");
		lperr(l,"(let-string-start+end (b e) car \"abc\" '(3) (list b e))");
		lperr(l,"(let-string-start+end (b e) car \"abc\" '(1 4) (list b e))");
	}

	public void testIsSubstringSpecOk() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(substring-spec-ok? \"abc\" 0 3)", T);
		equal(l,"(substring-spec-ok? \"abc\" 3 3)", T);
		equal(l,"(substring-spec-ok? \"abc\" -1 3)", F);
		equal(l,"(substring-spec-ok? \"abc\" 0 4)", F);
	}

	public void testMakeKmpRestartVector() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		equal(l,"(make-kmp-restart-vector \"ABCDABD\")",
				vec(-1, 0, 0, 0, 0, 1, 2));
		equal(l,"(make-kmp-restart-vector \"A\")", vec(-1));
		lperr(l,"(make-kmp-restart-vector \"\")");
	}

	public void testStringKmpPartialSearch() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-13)");
		l.exec ("(define rv (make-kmp-restart-vector \"ABCDABD\"))");
		eqi  (l,"(string-kmp-partial-search \"ABCDABD\" rv" +
				"  \"ABCDABCDABDAB\" 0)", -11);
		eqi  (l,"(string-kmp-partial-search \"ABCDABD\" rv" +
				"  \"ABCDABCDAB\" 0)", 6);
	}

}
