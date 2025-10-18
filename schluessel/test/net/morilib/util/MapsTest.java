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
package net.morilib.util;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/27
 */
public class MapsTest extends TC {

	private void ex1(String s) {
		try {
			Maps.matchPropertyKey(s, "");
			fail();
		} catch(InvalidPatternException e) {
		}
	}

	public void testMatchPropertyKey1() {
		eq(Maps.matchPropertyKey("abc", "abc"), new String[0]);
		eq(Maps.matchPropertyKey("abc.def", "abc.def"), new String[0]);
		nil(Maps.matchPropertyKey("abc", "ab"));
		nil(Maps.matchPropertyKey("abc", "abcd"));
	}

	public void testMatchPropertyKey2() {
		eq(Maps.matchPropertyKey("ab.*", "ab.cd"),
				new String[] { "cd" });
		eq(Maps.matchPropertyKey("ab.*.ef", "ab.cd.ef"),
				new String[] { "cd" });
		eq(Maps.matchPropertyKey("ab.*.ef.*", "ab.cd.ef.ghi"),
				new String[] { "cd", "ghi" });
		eq(Maps.matchPropertyKey("*.ef.*", "cd.ef.ghi"),
				new String[] { "cd", "ghi" });
		nil(Maps.matchPropertyKey("ab.*.cd", "ab.cd"));
		nil(Maps.matchPropertyKey("ab.*.cd", "ab.aa.cd.ef"));
		ex1("ab.*a");
		ex1("a*.a");
		ex1("a.a*");
		ex1("a.*a");
	}

	public void testMatchPropertyKey3() {
		eq(Maps.matchPropertyKey("ab.**", "ab.gh.ef.cd"),
				new String[] { "gh.ef.cd" });
		eq(Maps.matchPropertyKey("ab.**.cd", "ab.gh.ef.cd"),
				new String[] { "gh.ef" });
		eq(Maps.matchPropertyKey("ab.**.cd.**.ef", "ab.AB.cd.CD.EF.ef"),
				new String[] { "AB", "CD.EF" });
		eq(Maps.matchPropertyKey("ab.**.cd.**", "ab.AB.cd.CD"),
				new String[] { "AB", "CD" });
		eq(Maps.matchPropertyKey("ab.**", "ab"),
				new String[] { "" });
		eq(Maps.matchPropertyKey("ab.**.cd", "ab.cd"),
				new String[] { "" });
		eq(Maps.matchPropertyKey("ab.**.cd.**.ef", "ab.cd.ef"),
				new String[] { "", "" });
		eq(Maps.matchPropertyKey("ab.**.cd.**", "ab.cd"),
				new String[] { "", "" });
		eq(Maps.matchPropertyKey("ab.**.cd", "ab.cd.ef.cd.cd"),
				new String[] { "cd.ef.cd" });
		eq(Maps.matchPropertyKey("ab.**.cd.**.cd", "ab.cd.ef.cd.cd"),
				new String[] { "cd.ef", "" });
		eq(Maps.matchPropertyKey("ab.**.cd.**", "ab.cd.ef.cd.cd"),
				new String[] { "cd.ef.cd", "" });
		ex1("a**.b");
		ex1("a.**b");
		ex1("a.a**");
		ex1("a.**a");
		ex1("a.***.b");
		ex1("a.***");
		ex1("***.a");
	}

}
