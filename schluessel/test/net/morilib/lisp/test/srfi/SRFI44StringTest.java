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
public class SRFI44StringTest extends TCSubr {

	public void testStringFoldLeft() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-fold-left \"123\"" +
				" (lambda (x y) (append (list x) y)) '())",
				list('3', '2', '1'));
		equal(l,"(string-fold-left \"123\"" +
				" (lambda (x y) (append (list x) y)) '() '(1))",
				list('3', '2', '1'), list('3', '2', '1', 1));
		equal(l,"(string-fold-left \"\"" +
				" (lambda (x y) (append (list x) y)) '(1))", list(1));
		lperr(l,"(string-fold-left '(1 2 3) - 1)");
	}

	public void testStringFoldRight() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-fold-right \"123\" cons '())", list('1', '2', '3'));
		equal(l,"(string-fold-right \"123\" cons '() '(0))",
				list('1', '2', '3'), list('1', '2', '3', 0));
		equal(l,"(string-fold-right \"\" cons '())", list());
	}

	public void testStringCopy() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-copy \"1234\")", str("1234"));
		equal(l,"(string-copy \"1234\" 1)", str("234"));
		equal(l,"(string-copy \"1234\" 1 3)", str("23"));
		equal(l,"(string-copy \"1234\" 1 4)", str("234"));
		equal(l,"(string-copy \"1234\" 3 3)", str(""));
		equal(l,"(string-copy \"\")", str(""));
		lperr(l,"(string-copy \"1234\" 1 0)");
		lperr(l,"(string-copy \"1234\" 4)");
		lperr(l,"(string-copy \"1234\" 1 5)");
	}

	public void testStringToList() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string->list \"1234\")", list('1', '2', '3', '4'));
		equal(l,"(string->list \"\")", list());
	}

	public void testIsString() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string? \"1234\")", T);
		equal(l,"(string? \"\")", T);
		equal(l,"(string? 1)", F);
	}

	public void testStringSize() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(string-size \"1234\")", 4);
		eqi  (l,"(string-size \"\")", 0);
	}

	public void testIsStringEmpty() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-empty? \"1234\")", F);
		equal(l,"(string-empty? \"\")", T);
	}

	public void testIsStringContains() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-contains? \"1234\" #\\1)", T);
		equal(l,"(string-contains? \"1234\" #\\5)", F);
		equal(l,"(string-contains? \"\" #\\1)", F);
	}

	public void testStringCount() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(string-count \"1213\" #\\1)", 2);
		eqi  (l,"(string-count \"\" #\\1)", 0);
	}

	public void testStringRef() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-ref \"1234\" 0)", chr('1'));
		lperr(l,"(string-ref \"1234\" 5)");
		eqi  (l,"(string-ref \"1234\" 5 (lambda () 0))", 0);
		lperr(l,"(string-ref \"\" 0)");
		eqi  (l,"(string-ref \"\" 0 (lambda () 0))", 0);
	}

	public void testStringGetAny() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-get-any \"1234\")", chr('1'));
		equal(l,"(string-get-any \"\")", F);
		eqi  (l,"(string-get-any \"\" (lambda () 0))", 0);
	}

	public void testStringGetLeft() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-get-left \"1234\")", chr('1'));
		equal(l,"(string-get-left \"\")", F);
		eqi  (l,"(string-get-left \"\" (lambda () 0))", 0);
	}

	public void testStringGetRight() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-get-right \"1234\")", chr('4'));
		equal(l,"(string-get-right \"\")", F);
		eqi  (l,"(string-get-right \"\" (lambda () 0))", 0);
	}

	public void testStringSet() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-set \"123\" 1 #\\4)", str("143"));
		equal(l,"(string-set \"123\" 2 #\\4)", str("124"));
		equal(l,"(string-set \"123\" 0 #\\4)", str("423"));
		lperr(l,"(string-set \"\" 0 4)");
		lperr(l,"(string-set \"123\" -1 #\\4)");
		lperr(l,"(string-set \"123\" 3 #\\4)");
		lperr(l,"(string-set \"123\" 1 4)");
	}

	public void testStringSetS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define x \"123\")");
		l.exec ("(string-set! x 1 #\\4)");
		equal(l,"x", str("143"));
		l.exec ("(define x \"123\")");
		l.exec ("(string-set! x 2 #\\4)");
		equal(l,"x", str("124"));
		l.exec ("(define x \"123\")");
		l.exec ("(string-set! x 0 #\\4)");
		equal(l,"x", str("423"));
		lperr(l,"(string-set! \"\" 0 4)");
		lperr(l,"(string-set! \"123\" -1 #\\4)");
		lperr(l,"(string-set! \"123\" 3 #\\4)");
		lperr(l,"(string-set! \"123\" 1 4)");
	}

	public void testStringReplaceFrom() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-replace-from \"1234\" 0 \"98\")",
				str("9834"));
		equal(l,"(string-replace-from \"1234\" 0 \"0198\" 2)",
				str("9834"));
		equal(l,"(string-replace-from \"1234\" 0 \"01987\" 2 4)",
				str("9834"));
		equal(l,"(string-replace-from \"1234\" 1 \"98\")",
				str("1984"));
		equal(l,"(string-replace-from \"1234\" 2 \"98\")",
				str("1298"));
		lperr(l,"(string-replace-from \"1234\" 3 \"98\")");
		lperr(l,"(string-replace-from \"\" 0 \"98\")");
		lperr(l,"(string-replace-from \"1234\" 0 '(9 8))");
	}

	public void testStringReplaceFromS() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string-replace-from! \"1234\" 0 \"98\")",
				str("9834"));
		equal(l,"(string-replace-from! \"1234\" 0 \"0198\" 2)",
				str("9834"));
		equal(l,"(string-replace-from! \"1234\" 0 \"01987\" 2 4)",
				str("9834"));
		equal(l,"(string-replace-from! \"1234\" 1 \"98\")",
				str("1984"));
		equal(l,"(string-replace-from! \"1234\" 2 \"98\")",
				str("1298"));
		lperr(l,"(string-replace-from! \"1234\" 3 \"98\")");
		lperr(l,"(string-replace-from! \"\" 0 \"98\")");
		lperr(l,"(string-replace-from! \"1234\" 0 '(9 8))");
	}

	public void testStringEq() {
		Scheme l = Scheme.newInstance();

		equal(l,"(string= eqv?)", T);
		equal(l,"(string= eqv? \"123\")", T);
		equal(l,"(string= eqv? \"123\" \"123\")", T);
		equal(l,"(string= eqv? \"123\" \"123\" \"123\")", T);
		equal(l,"(string= eqv? \"123\" \"124\")", F);
		equal(l,"(string= eqv? \"123\" \"123\" \"223\")", F);
		equal(l,"(string= eqv? \"123\" \"124\" \"123\")", F);
	}

}
