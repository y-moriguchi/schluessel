package net.morilib.lingua;

import net.morilib.lingua.numeral.LatinNumeral;
import junit.framework.TestCase;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/01
 */
public class LatinNumeralTest extends TestCase {

	//
	static final LatinNumeral.Gender M = LatinNumeral.Gender.MUSCLINE;
	static final LatinNumeral.Gender F = LatinNumeral.Gender.FEMININE;
	static final LatinNumeral.Gender N = LatinNumeral.Gender.NEUTER;

	//
	static final LatinNumeral.Case NO = LatinNumeral.Case.NOMINATIVE;
	static final LatinNumeral.Case GE = LatinNumeral.Case.GENITIVE;
	static final LatinNumeral.Case DA = LatinNumeral.Case.DATIVE;
	static final LatinNumeral.Case AC = LatinNumeral.Case.ACCUSATIVE;
	static final LatinNumeral.Case AB = LatinNumeral.Case.ABLATIVE;

	//
	static void t1(int i, LatinNumeral.Gender g, LatinNumeral.Case c,
			String s) {
		assertEquals(LatinNumeral.toNumeralWithMacron(i, g, c), s);
	}

	//
	static void e1(int i, LatinNumeral.Gender g, LatinNumeral.Case c) {
		try {
			LatinNumeral.toNumeralWithMacron(i, g, c);
			fail();
		} catch(IllegalArgumentException e) {}
	}

	public void testToNumeral1() {
		t1(1, M, NO, "ūnus");  t1(1, M, GE, "ūnīus");
		t1(1, M, DA, "ūnī");   t1(1, M, AC, "ūnum");
		t1(1, M, AB, "ūnō");
		t1(1, F, NO, "ūna");   t1(1, F, GE, "ūnīus");
		t1(1, F, DA, "ūnī");   t1(1, F, AC, "ūnam");
		t1(1, F, AB, "ūnā");
		t1(1, N, NO, "ūnum");  t1(1, N, GE, "ūnīus");
		t1(1, N, DA, "ūnī");   t1(1, N, AC, "ūnum");
		t1(1, N, AB, "ūnō");
	}

	public void testToNumeral2() {
		t1(2, M, NO, "duo");     t1(2, M, GE, "duōrum");
		t1(2, M, DA, "duōbus");  t1(2, M, AC, "duōs");
		t1(2, M, AB, "duōbus");
		t1(2, F, NO, "duae");    t1(2, F, GE, "duārum");
		t1(2, F, DA, "duābus");  t1(2, F, AC, "duās");
		t1(2, F, AB, "duābus");
		t1(2, N, NO, "duo");     t1(2, N, GE, "duōrum");
		t1(2, N, DA, "duōbus");  t1(2, N, AC, "duo");
		t1(2, N, AB, "duōbus");
	}

	public void testToNumeral3() {
		t1(3, M, NO, "trēs");    t1(3, M, GE, "trium");
		t1(3, M, DA, "tribus");  t1(3, M, AC, "trēs");
		t1(3, M, AB, "tribus");
		t1(3, F, NO, "trēs");    t1(3, F, GE, "trium");
		t1(3, F, DA, "tribus");  t1(3, F, AC, "trēs");
		t1(3, F, AB, "tribus");
		t1(3, N, NO, "tria");    t1(3, N, GE, "trium");
		t1(3, N, DA, "tribus");  t1(3, N, AC, "tria");
		t1(3, N, AB, "tribus");
	}

	public void testToNumeral9() {
		t1(4, M, NO, "quattuor");  t1(5, M, GE, "quīnque");
		t1(6, M, DA, "sex");       t1(7, M, AC, "septem");
		t1(8, M, AB, "octō");      t1(9, F, NO, "novem");
	}

	public void testToNumeral20() {
		t1(10, M, NO, "decem");
		t1(11, M, GE, "ūndecem");
		t1(12, M, DA, "duodecem");
		t1(13, M, AC, "tredecem");
		t1(14, M, AB, "quattordecem");
		t1(15, F, NO, "quīndecem");
		t1(16, F, DA, "sēdecem");
		t1(17, N, NO, "septendecem");
		t1(18, N, AC, "duodēvīgintī");
		t1(19, N, AB, "ūndēvīgintī");
		t1(20, N, DA, "vīgintī");
	}

	public void testToNumeral99() {
		t1(21, M, NO, "vīgintī ūnus");
		t1(22, M, GE, "vīgintī duōrum");
		t1(23, M, DA, "vīgintī tribus");
		t1(24, M, AC, "vīgintī quattuor");
		t1(25, M, AB, "vīgintī quīnque");
		t1(26, F, NO, "vīgintī sex");
		t1(27, F, DA, "vīgintī septem");
		t1(28, N, NO, "duodētrīgintā");
		t1(29, N, AC, "ūndētrīgintā");
		t1(30, N, AB, "trīgintā");
		t1(31, F, NO, "trīgintā ūna");
		t1(32, F, GE, "trīgintā duārum");
		t1(38, N, NO, "trīgintā octō");
		t1(39, N, AC, "trīgintā novem");
		t1(30, N, AB, "trīgintā");
		t1(40, N, AB, "quadrāgintā");
		t1(50, N, AB, "quīnquāgintā");
		t1(60, N, AB, "sexāgintā");
		t1(70, N, AB, "septuāgintā");
		t1(72, F, AB, "septuāgintā duābus");
		t1(80, N, AB, "octōgintā");
		t1(98, N, NO, "nōnāgintā octō");
		t1(99, N, AC, "nōnāgintā novem");
	}

	public void testToNumeral199() {
		t1(100, M, NO, "centum");
		t1(101, M, AB, "centum ūnō");
		t1(200, M, NO, "ducentī");
		t1(300, M, NO, "trecentī");
		t1(400, M, NO, "quadrigentī");
		t1(500, M, NO, "quīngentī");
		t1(600, M, NO, "sescentī");
		t1(700, M, NO, "septingentī");
		t1(800, M, NO, "octigentī");
		t1(900, M, NO, "nōngentī");

		t1(200, M, NO, "ducentī");
		t1(200, M, GE, "ducentōrum");
		t1(200, M, DA, "ducentīs");
		t1(200, M, AC, "ducentōs");
		t1(200, M, AB, "ducentīs");
		t1(200, F, NO, "ducentae");
		t1(200, F, GE, "ducentārum");
		t1(200, F, DA, "ducentīs");
		t1(200, F, AC, "ducentās");
		t1(200, F, AB, "ducentīs");
		t1(200, N, NO, "ducenta");
		t1(200, N, GE, "ducentōrum");
		t1(200, N, DA, "ducentīs");
		t1(200, N, AC, "ducenta");
		t1(200, N, AB, "ducentīs");

		t1(765, M, NO, "septingentī sexāgintā quīnque");
		t1(961, M, AB, "nōngentīs sexāgintā ūnō");
	}

	public void testToNumeral999999() {
		t1(1000, N, NO, "mille");
		t1(1001, N, NO, "mille ūnum");
		t1(1729, N, NO, "mille septingenta ūndētrīgintā");
		t1(2000, N, NO, "duo mīlia");
		t1(2000, N, GE, "duōrum mīlium");
		t1(2000, N, DA, "duōbus mīlibus");
		t1(2000, N, AC, "duo mīlia");
		t1(2000, N, AB, "duōbus mīlibus");
		t1(72000, N, NO, "septuāgintā duo mīlia");
		t1(720000, N, NO, "septingenta vīgintī mīlia");
	}

	public void testToNumeralError() {
		e1(0, N, NO);
		e1(1000, M, NO);
		assertNull(LatinNumeral.toNumeralWithMacron(1000000, N, NO));
	}

	//
	static void t1(int i, String s) {
		assertEquals(LatinNumeral.toNumberWithMacron(s), i);
	}

	public void testToNumber1() {
		t1(1, "ūnus");  t1(1, "ūnīus");
		t1(1, "ūnī");   t1(1, "ūnum");   t1(1, "ūnō");
		t1(1, "ūna");   t1(1, "ūnīus");
		t1(1, "ūnī");   t1(1, "ūnam");   t1(1, "ūnā");
		t1(1, "ūnum");  t1(1, "ūnīus");
		t1(1, "ūnī");   t1(1, "ūnum");   t1(1, "ūnō");
	}

	public void testToNumber2() {
		t1(2, "duo");     t1(2, "duōrum");
		t1(2, "duōbus");  t1(2, "duōs");    t1(2, "duōbus");
		t1(2, "duae");    t1(2, "duārum");
		t1(2, "duābus");  t1(2, "duās");    t1(2, "duābus");
		t1(2, "duo");     t1(2, "duōrum");
		t1(2, "duōbus");  t1(2, "duo");     t1(2, "duōbus");
	}

	public void testToNumber3() {
		t1(3, "trēs");    t1(3, "trium");
		t1(3, "tribus");  t1(3, "trēs");   t1(3, "tribus");
		t1(3, "trēs");    t1(3, "trium");
		t1(3, "tribus");  t1(3, "trēs");   t1(3, "tribus");
		t1(3, "tria");    t1(3, "trium");
		t1(3, "tribus");  t1(3, "tria");   t1(3, "tribus");
	}

	public void testToNumber9() {
		t1(4, "quattuor");  t1(5, "quīnque");
		t1(6, "sex");       t1(7, "septem");
		t1(8, "octō");      t1(9, "novem");
	}

	public void testToNumber20() {
		t1(10, "decem");
		t1(11, "ūndecem");
		t1(12, "duodecem");
		t1(13, "tredecem");
		t1(14, "quattordecem");
		t1(15, "quīndecem");
		t1(16, "sēdecem");
		t1(17, "septendecem");
		t1(18, "duodēvīgintī");
		t1(19, "ūndēvīgintī");
		t1(20, "vīgintī");
	}

	public void testToNumber99() {
		t1(21, "vīgintī ūnus");
		t1(22, "vīgintī duōrum");
		t1(23, "vīgintī tribus");
		t1(24, "vīgintī quattuor");
		t1(25, "vīgintī quīnque");
		t1(26, "vīgintī sex");
		t1(27, "vīgintī septem");
		t1(28, "duodētrīgintā");
		t1(29, "ūndētrīgintā");
		t1(30, "trīgintā");
		t1(31, "trīgintā ūna");
		t1(32, "trīgintā duārum");
		t1(38, "trīgintā octō");
		t1(39, "trīgintā novem");
		t1(30, "trīgintā");
		t1(40, "quadrāgintā");
		t1(50, "quīnquāgintā");
		t1(60, "sexāgintā");
		t1(70, "septuāgintā");
		t1(72, "septuāgintā duōbus");
		t1(80, "octōgintā");
		t1(98, "nōnāgintā octō");
		t1(99, "nōnāgintā novem");
	}

	public void testToNumber199() {
		t1(100, "centum");       t1(101, "centum ūnō");
		t1(200, "ducentī");      t1(300, "trecentī");
		t1(400, "quadrigentī");  t1(500, "quīngentī");
		t1(600, "sescentī");     t1(700, "septingentī");
		t1(800, "octigentī");    t1(900, "nōngentī");

		t1(200, "ducentī");     t1(200, "ducentōrum");
		t1(200, "ducentīs");    t1(200, "ducentōs");
		t1(200, "ducentīs");    t1(200, "ducentae");
		t1(200, "ducentārum");  t1(200, "ducentīs");
		t1(200, "ducentās");    t1(200, "ducentīs");
		t1(200, "ducenta");     t1(200, "ducentōrum");
		t1(200, "ducentīs");    t1(200, "ducenta");
		t1(200, "ducentīs");

		t1(765, "septingentī sexāgintā quīnque");
		t1(961, "nōngentīs sexāgintā ūnō");
	}

	public void testToNumber999999() {
		t1(1000, "mille");
		t1(1001, "mille ūnum");
		t1(1729, "mille septingenta ūndētrīgintā");
		t1(2000, "duo mīlia");
		t1(2000, "duōrum mīlium");
		t1(2000, "duōbus mīlibus");
		t1(2000, "duo mīlia");
		t1(2000, "duōbus mīlibus");
		t1(72000, "septuāgintā duo mīlia");
		t1(720000, "septingenta vīgintī mīlia");
	}

}
