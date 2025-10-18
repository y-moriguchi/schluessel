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
package net.morilib.lingua;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/05
 */
public class LatinUtilsTest extends TC {

	public void testNormalizeClassic() {
		eq(LatinUtils.normalizeClassic("JUPITER"), "IVPITER");
		eq(LatinUtils.normalizeClassic("Jupiter"), "IVPITER");
		eq(LatinUtils.normalizeClassic("jupiter"), "IVPITER");
		eq(LatinUtils.normalizeClassic("IVPITER"), "IVPITER");
		eq(LatinUtils.normalizeClassic("Ivpiter"), "IVPITER");
		eq(LatinUtils.normalizeClassic("ivpiter"), "IVPITER");
		eq(LatinUtils.normalizeClassic("WlpesWlpesW"), "VVLPESVVLPESVV");
		eq(LatinUtils.normalizeClassic("wlpeswlpesW"), "VVLPESVVLPESVV");
		eq(LatinUtils.normalizeClassic("WLPESWLPESW"), "VVLPESVVLPESVV");
	}

	public void testNormalizeModern() {
		eq(LatinUtils.normalizeModern("IVPITER"), "JUPITER");
		eq(LatinUtils.normalizeModern("Ivpiter"), "Jupiter");
		eq(LatinUtils.normalizeModern("降臨せよ!Ivpiter"), "降臨せよ!Jupiter");
		eq(LatinUtils.normalizeModern("降臨せよ!IVPITER"), "降臨せよ!JUPITER");
		eq(LatinUtils.normalizeModern("cvivs"), "cujus");
		eq(LatinUtils.normalizeModern("ivivs"), "jujus");
		eq(LatinUtils.normalizeModern("Cvivs"), "Cujus");
		eq(LatinUtils.normalizeModern("Ivivs"), "Jujus");
		eq(LatinUtils.normalizeModern("CVIVS"), "CUJUS");
		eq(LatinUtils.normalizeModern("IVIVS"), "JUJUS");
		eq(LatinUtils.normalizeModern("iiii"), "jiji");
		eq(LatinUtils.normalizeModern("Iiii"), "Jiji");
		eq(LatinUtils.normalizeModern("IIII"), "JIJI");
		eq(LatinUtils.normalizeModern("vvvv"), "vuvu");
		eq(LatinUtils.normalizeModern("Vvvv"), "Vuvu");
		eq(LatinUtils.normalizeModern("VVVV"), "VUVU");
		eq(LatinUtils.normalizeModern("WlpesWlpesW"), "VulpesVulpesVu");
		eq(LatinUtils.normalizeModern("wlpes"), "vulpes");
		eq(LatinUtils.normalizeModern("WLPES"), "VULPES");
		eq(LatinUtils.normalizeModern("animalivm"), "animalium");
		eq(LatinUtils.normalizeModern("manvvm"), "manuum");
		eq(LatinUtils.normalizeModern("ANIMALIVM"), "ANIMALIUM");
		eq(LatinUtils.normalizeModern("MANVVM"), "MANUUM");
	}

	public void testEqualsAsLatin() {
		ok(LatinUtils.equalsAsLatin("Jupiter", "Ivpiter"));
		ok(LatinUtils.equalsAsLatin("JUPITER", "IVPITER"));
		ok(LatinUtils.equalsAsLatin("jupiter", "ivpiter"));
		ng(LatinUtils.equalsAsLatin("JUPITER", "Ivpiter"));
		ok(LatinUtils.equalsAsLatin("wlpes", "vulpes"));
	}

	public void testEqualsIgnoreCaseAsLatin() {
		ok(LatinUtils.equalsIgnoreCaseAsLatin("Jupiter", "Ivpiter"));
		ok(LatinUtils.equalsIgnoreCaseAsLatin("JUPITER", "IVPITER"));
		ok(LatinUtils.equalsIgnoreCaseAsLatin("jupiter", "ivpiter"));
		ok(LatinUtils.equalsIgnoreCaseAsLatin("JUPITER", "Ivpiter"));
		ok(LatinUtils.equalsIgnoreCaseAsLatin("wlpes", "vulpes"));
	}

}
