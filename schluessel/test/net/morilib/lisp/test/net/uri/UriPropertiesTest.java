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
package net.morilib.lisp.test.net.uri;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/22
 */
public class UriPropertiesTest extends TCSubr {

	public void testIsUriAbsolute() {
		Scheme l = Scheme.newInstance();

		eq(l.input("(uri-absolute? (make-uri \"mailto:aaa@example.com\"))"), T);
		eq(l.input("(uri-absolute? (make-uri \"http://localhost/aaa/bbb\"))"), T);
		eq(l.input("(uri-absolute? (make-uri \"/aaa/bbb\"))"), F);
	}

	public void testIsUriOpaque() {
		Scheme l = Scheme.newInstance();

		eq(l.input("(uri-opaque? (make-uri \"mailto:aaa@example.com\"))"), T);
		eq(l.input("(uri-opaque? (make-uri \"http://localhost/aaa/bbb\"))"), F);
		eq(l.input("(uri-opaque? (make-uri \"/aaa/bbb\"))"), F);
	}

	public void testNormalizeUri() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define uri1 (make-uri \"http://localhost/aaa/\"))");
		eq   (l,"(eqv?" +
				"  (normalize-uri (make-uri \"http://localhost/aaa/./b/..\"))" +
				"  uri1)", T);
	}

	public void testRelativizeUri() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define uri1 (make-uri \"docs/guide/index.html\"))");
		l.exec ("(define uri2 (make-uri \"http://java.sun.com/j2se/1.3/\"))");
		eq   (l,"(eqv?" +
				"  (relativize-uri" +
				"    uri2" +
				"    (make-uri \"http://java.sun.com/j2se/1.3/docs/guide/index.html\"))" +
				"  uri1)", T);
	}

	public void testResolveUri() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define uri1" +
				"  (make-uri \"http://java.sun.com/j2se/1.3/docs/guide/collections/designfaq.html#28\"))");
		l.exec ("(define uri2 (make-uri \"http://java.sun.com/j2se/1.3/\"))");
		l.exec ("(define uri3" +
				"  (make-uri \"http://java.sun.com/j2se/1.3/demo/jfc/SwingSet2/src/SwingSet2.java\"))");
		eq   (l,"(eqv?" +
				"  (resolve-uri" +
				"    uri2" +
				"    (make-uri \"docs/guide/collections/designfaq.html#28\"))" +
				"  uri1)", T);
		eq   (l,"(eqv?" +
				"  (resolve-uri" +
				"    uri1" +
				"    \"../../../demo/jfc/SwingSet2/src/SwingSet2.java\")" +
				"  uri3)", T);
	}

	public void testUriProps1() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define uri1" +
				"  (make-uri \"http://localhost:8080/j2se/1.3/%E2%82%AC/%E2%82%AC.html?cur=%E2%82%AC#%E2%82%AC\"))");
		eqs  (l,"(uri-authority uri1)", "localhost:8080");
		eqs  (l,"(uri-fragment  uri1)", "\u20AC");
		eqs  (l,"(uri-host      uri1)", "localhost");
		eqs  (l,"(uri-path      uri1)", "/j2se/1.3/\u20AC/\u20AC.html");
		eqi  (l,"(uri-port      uri1)", 8080);
		eqs  (l,"(uri-query     uri1)", "cur=\u20AC");
		eqs  (l,"(uri-scheme    uri1)", "http");
		eqs  (l,"(uri-scheme-specific-part uri1)",
				"//localhost:8080/j2se/1.3/\u20AC/\u20AC.html?cur=\u20AC");
		eq   (l,"(uri-user-info uri1)", F);
		eqs  (l,"(uri-raw-authority uri1)", "localhost:8080");
		eqs  (l,"(uri-raw-fragment  uri1)", "%E2%82%AC");
		eqs  (l,"(uri-raw-path      uri1)", "/j2se/1.3/%E2%82%AC/%E2%82%AC.html");
		eqs  (l,"(uri-raw-query     uri1)", "cur=%E2%82%AC");
		eqs  (l,"(uri-raw-scheme-specific-part uri1)",
				"//localhost:8080/j2se/1.3/%E2%82%AC/%E2%82%AC.html?cur=%E2%82%AC");
		eq   (l,"(uri-raw-user-info uri1)", F);
	}

	public void testUriProps2() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define uri1 (make-uri \"j2se/1.3/%E2%82%AC/%E2%82%AC.html#28\"))");
		eq   (l,"(uri-authority uri1)", F);
		eqs  (l,"(uri-fragment  uri1)", "28");
		eq   (l,"(uri-host      uri1)", F);
		eqs  (l,"(uri-path      uri1)", "j2se/1.3/\u20AC/\u20AC.html");
		eq   (l,"(uri-port      uri1)", F);
		eq   (l,"(uri-query     uri1)", F);
		eq   (l,"(uri-scheme    uri1)", F);
		eqs  (l,"(uri-scheme-specific-part uri1)",
				"j2se/1.3/\u20AC/\u20AC.html");
		eq   (l,"(uri-user-info uri1)", F);
		eq   (l,"(uri-raw-authority uri1)", F);
		eqs  (l,"(uri-raw-fragment  uri1)", "28");
		eqs  (l,"(uri-raw-path      uri1)", "j2se/1.3/%E2%82%AC/%E2%82%AC.html");
		eq   (l,"(uri-raw-query     uri1)", F);
		eqs  (l,"(uri-raw-scheme-specific-part uri1)",
				"j2se/1.3/%E2%82%AC/%E2%82%AC.html");
		eq   (l,"(uri-raw-user-info uri1)", F);
	}

	public void testUriProps3() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define uri1 (make-uri \"ssh://%E2%82%AC@localhost:22\"))");
		eqs  (l,"(uri-authority uri1)", "\u20AC@localhost:22");
		eq   (l,"(uri-fragment  uri1)", F);
		eqs  (l,"(uri-host      uri1)", "localhost");
		eqs  (l,"(uri-path      uri1)", "");
		eqi  (l,"(uri-port      uri1)", 22);
		eq   (l,"(uri-query     uri1)", F);
		eqs  (l,"(uri-scheme    uri1)", "ssh");
		eqs  (l,"(uri-scheme-specific-part uri1)", "//\u20AC@localhost:22");
		eqs  (l,"(uri-user-info uri1)", "\u20AC");
		eqs  (l,"(uri-raw-authority uri1)", "%E2%82%AC@localhost:22");
		eq   (l,"(uri-raw-fragment  uri1)", F);
		eqs  (l,"(uri-raw-path      uri1)", "");
		eq   (l,"(uri-raw-query     uri1)", F);
		eqs  (l,"(uri-raw-scheme-specific-part uri1)",
				"//%E2%82%AC@localhost:22");
		eqs  (l,"(uri-raw-user-info uri1)", "%E2%82%AC");
	}

	public void testUriProps4() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define uri1 (make-uri \"news:comp.lang.java\"))");
		eq   (l,"(uri-authority uri1)", F);
		eq   (l,"(uri-fragment  uri1)", F);
		eq   (l,"(uri-host      uri1)", F);
		eq   (l,"(uri-path      uri1)", F);
		eq   (l,"(uri-port      uri1)", F);
		eq   (l,"(uri-query     uri1)", F);
		eqs  (l,"(uri-scheme    uri1)", "news");
		eqs  (l,"(uri-scheme-specific-part uri1)", "comp.lang.java");
		eq   (l,"(uri-user-info uri1)", F);
		eq   (l,"(uri-raw-authority uri1)", F);
		eq   (l,"(uri-raw-fragment  uri1)", F);
		eq   (l,"(uri-raw-path      uri1)", F);
		eq   (l,"(uri-raw-query     uri1)", F);
		eqs  (l,"(uri-raw-scheme-specific-part uri1)", "comp.lang.java");
	}

	public void testUriToAsciiString() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define uri1 (make-uri \"ssh://\u20AC@localhost:22\"))");
		eqs  (l,"(uri->ascii-string uri1)", "ssh://%E2%82%AC@localhost:22");
	}

}
