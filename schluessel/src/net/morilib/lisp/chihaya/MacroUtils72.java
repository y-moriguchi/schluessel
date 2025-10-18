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
package net.morilib.lisp.chihaya;

import net.morilib.lisp.EnvironmentObject;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.UnsyntaxMarker;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/04
 */
public class MacroUtils72 {

	//
	private static final Scheme SCM;

	//
	static {
		// defineのときに特別扱い
		String UNQ =
			"(define (unq x env)" +
			"  (cond ((null? x) '())" +
			"        ((not (pair? x)) x)" +
			"        ((or (eq? (car x)  'unquote72)" +
			"             (unsyntax-marker? (car x)))" +
			"          (eval (syntax->datum (cadr x)) env))" +
			"        ((and (pair? (cdr x))" +
			"              (or (eq? (cadr x) 'unquote72)" +
			"                  (unsyntax-marker? (car x))))" +
			"          (cons (unq (car x) env)" +
			"                (eval (syntax->datum (caddr x)) env)))" +
			"        (else (cons (unq (car x) env)" +
			"                    (unq (cdr x) env)))))";
		String UNQ2 =
			"(define (unq2 x)" +
			"  (cond ((null? x) '())" +
			"        ((not (pair? x)) x)" +
			"        ((eq? (car x) 'unquote72)" +
			"          `(unsyntax-marker ,(cadr x)))" +
			"        ((and (pair? (cdr x))" +
			"              (eq? (cadr x) 'unquote72))" +
			"          (cons (unq2 (car x))" +
			"                `(unsyntax-marker ,(caddr x))))" +
			"        (else (cons (unq2 (car x))" +
			"                    (unq2 (cdr x))))))";

		SCM = Scheme.newRnRS(5);
		SCM.input(UNQ);
		SCM.input(UNQ2);
		SCM.set("find-environment",
				new EnvironmentObject.FindEnvironment());
		SCM.set("syntax->datum", new SyntaxToDatum72());
		SCM.set("unsyntax-marker?",
				new UnsyntaxMarker.IsUnsyntaxMarker());
	}

	//
	private MacroUtils72() {}

	/**
	 * 
	 * @param x
	 * @param env
	 * @return
	 */
//	public static Datum unquote(Datum x, Datum env) {
//		return SCM.call("unq", x, env);
//	}

	/**
	 * 
	 * @param x
	 * @param env
	 * @return
	 */
//	public static Datum unquote2(Datum x) {
//		return SCM.call("unq2", x);
//	}

}
