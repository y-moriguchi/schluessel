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
 * @author MORIGUCHI, Yuichiro 2012/08/28
 */
public class GreekUtilsTest extends TC {

	void l2g(String latin, String greek) {
		eq(GreekUtils.latinToGreekAlphabet(latin), greek);
	}

	void g2l(String greek, String latin) {
		eq(GreekUtils.greekToLatinAlphabet(greek), latin);
	}

	public void testLatinToGreekAlphabet() {
		l2g("habdgilmxyss", "αβδγιλμξυσς");
		l2g("chcc", "χcc");
		l2g("eeeeeue", "ηηευε");
		l2g("khkskik", "χξκικ");
		l2g("ooooouo", "ωωουο");
		l2g("phpspip", "φψπιπ");
		l2g("rhrir", "ρριρ");
		l2g("thtit", "θτιτ");
		l2g("ngnxnin", "γγγξνιν");
		l2g("zdziz", "ζζιζ");
		l2g("nksnkhnkink", "γξγχνκινκ");
		l2g("nchncinc", "γχνcινc");

		l2g("HABDGILMXYS", "ΑΒΔΓΙΛΜΞΥΣ");
		l2g("CHCC", "ΧCC");
		l2g("EEEEEUE", "ΗΗΕΥΕ");
		l2g("KHKSKIK", "ΧΞΚΙΚ");
		l2g("OOOOOUO", "ΩΩΟΥΟ");
		l2g("PHPSPIP", "ΦΨΠΙΠ");
		l2g("RHRIR", "ΡΡΙΡ");
		l2g("THTIT", "ΘΤΙΤ");
		l2g("NGNXNIN", "ΓΓΓΞΝΙΝ");
		l2g("ZDZIZ", "ΖΖΙΖ");
		l2g("NKSNKHNKINK", "ΓΞΓΧΝΚΙΝΚ");
		l2g("NCHNCINC", "ΓΧΝCΙΝC");

		l2g("ChCC", "ΧCC");
		l2g("EeEeEUE", "ΗΗΕΥΕ");
		l2g("KhKsKIK", "ΧΞΚΙΚ");
		l2g("OoOoOUO", "ΩΩΟΥΟ");
		l2g("PhPsPIP", "ΦΨΠΙΠ");
		l2g("RhRIR", "ΡΡΙΡ");
		l2g("ThTIT", "ΘΤΙΤ");
		l2g("NgNxNIN", "ΓγΓξΝΙΝ");
		l2g("ZdZIZ", "ΖΖΙΖ");
		l2g("NksNkhNkINk", "ΓξΓχΝκΙΝκ");
		l2g("NchNcINc", "ΓχΝcΙΝc");

		l2g("akropolis", "ακροπολις");
		l2g("psykhee", "ψυχη");
	}

	public void testGreekToLatinAlphabet() {
		g2l("ραβγδεζηθικλμνξοπρστφχψως",
				"rhabgdezdeethiklmnksoprstphchpsoos");
		g2l("υευουαυηυυ", "hyeuouaueeuy");
		g2l("υΕυΟυΑυΗυυ", "hyEuOuAuEeuy");

		g2l("ΡΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΦΧΨΩ",
				"RHABGDEZDEETHIKLMNKSOPRSTPHCHPSOO");
		g2l("ΥΕΥΟΥΑΥΗΥΥ", "HYEUOUAUEEUY");
		g2l("ΥεΥοΥαΥηΥΥ", "HyeUoUaUeeUY");
		g2l("ΡαΗαΘαΞαΦαΧαΨαΩα", "RhaEeaThaKsaPhaChaPsaOoa");
	}

}
