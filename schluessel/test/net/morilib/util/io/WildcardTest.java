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
package net.morilib.util.io;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/28
 */
public class WildcardTest extends TC {

	public void testMatches() {
		ok(Wildcard.matches("a", "a"));
		ng(Wildcard.matches("a", "b"));
		ok(Wildcard.matches("file", "file"));
		ng(Wildcard.matches("file", "files"));
		ng(Wildcard.matches("file", "fil"));
		ng(Wildcard.matches("file", "fill"));
		ok(Wildcard.matches("??", "ab"));
		ng(Wildcard.matches("??", "abc"));
		ok(Wildcard.matches("[a-z][a-z]", "ab"));
		ng(Wildcard.matches("[a-z][a-z]", "aB"));
		ok(Wildcard.matches("*", "abc"));
		ok(Wildcard.matches("*", ""));
		ok(Wildcard.matches("*a", "abca"));
		ng(Wildcard.matches("*a", "abc"));
		ok(Wildcard.matches("a*", "abca"));
		ng(Wildcard.matches("a*", "babc"));
		ok(Wildcard.matches("*.*", "file.txt"));
		ok(Wildcard.matches("*.*", ".bashrc"));
		ng(Wildcard.matches("*.*", "filetxt"));
		ok(Wildcard.matches("*ab", "aaaaab"));
	}

}
