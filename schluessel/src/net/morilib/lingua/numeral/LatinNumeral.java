package net.morilib.lingua.numeral;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/01
 */
public class LatinNumeral {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/12/01
	 */
	public static enum Gender {
		MUSCLINE, FEMININE, NEUTER
	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/12/01
	 */
	public static enum Case {
		NOMINATIVE, GENITIVE, DATIVE, ACCUSATIVE, ABLATIVE
	}

	//
	static final String[][][] UNUS_DUO_TRES = new String[][][] {
		{	{ "ūnus",   "ūna",    "ūnum" },
			{ "ūnīus",  "ūnīus",  "ūnīus" },
			{ "ūnī",    "ūnī",    "ūnī" },
			{ "ūnum",   "ūnam",   "ūnum" },
			{ "ūnō",    "ūnā",    "ūnō" }, },
		{	{ "duo",    "duae",   "duo" },
			{ "duōrum", "duārum", "duōrum" },
			{ "duōbus", "duābus", "duōbus" },
			{ "duōs",   "duās",   "duo" },
			{ "duōbus", "duābus", "duōbus" }, },
		{	{ "trēs",   "trēs",   "tria" },
			{ "trium",  "trium",  "trium" },
			{ "tribus", "tribus", "tribus" },
			{ "trēs",   "trēs",   "tria" },
			{ "tribus", "tribus", "tribus" }, }
	};

	//
	static final String[] NUM_19 = new String[] {
		null,       null,          null,           null,
		"quattuor", "quīnque",     "sex",          "septem",
		"octō",     "novem",       "decem",        "ūndecem",
		"duodecem", "tredecem",    "quattordecem", "quīndecem",
		"sēdecem",  "septendecem", "duodēvīgintī", "ūndēvīgintī"
	};

	//
	static final String DUODE = "duodē";
	static final String UNDE = "ūndē";
	static final String DUODETRIGINTA = "duodētrīgintā";
	static final String UNDETRIGINTA = "ūndētrīgintā";
	static final String[] NUM_99 = new String[] {
		null, null, 
		"vīgintī",   "trīgintā",    "quadrāgintā", "quīnquāgintā",
		"sexāgintā", "septuāgintā", "octōgintā",   "nōnāgintā"
	};

	//
	static final String CENTUM = "centum";
	static final String[][] CENTI = new String[][] {
		{ "entī",    "entae",   "enta" },
		{ "entōrum", "entārum", "entōrum" },
		{ "entīs",   "entīs",   "entīs" },
		{ "entōs",   "entās",   "enta" },
		{ "entīs",   "entīs",   "entīs" },
	};
	static final String[] PREFIX_900 = new String[] {
		null,    null,   "duc",     "trec",  "quadrig",
		"quīng", "sesc", "septing", "octig", "nōng"
	};

	//
	static final String MILLE = "mille";
	static final String[] MILIA = new String[] {
		"mīlia", "mīlium", "mīlibus", "mīlia", "mīlibus"
	};

	//
	static String removeMacron(String s) {
		StringBuffer b = new StringBuffer();
		char c;

		for(int i = 0; i < s.length(); i++) {
			c = s.charAt(i);
			switch(c) {
			case 'ā':  b.append('a');  break;
			case 'ē':  b.append('e');  break;
			case 'ī':  b.append('i');  break;
			case 'ō':  b.append('o');  break;
			case 'ū':  b.append('u');  break;
			case 'Ā':  b.append('A');  break;
			case 'Ē':  b.append('E');  break;
			case 'Ī':  b.append('I');  break;
			case 'Ō':  b.append('U');  break;
			case 'Ū':  b.append('O');  break;
			default:  b.append(c);  break;
			}
		}
		return b.toString();
	}

	/**
	 * 
	 * @param a
	 * @param gn
	 * @param ce
	 * @return
	 */
	public static String toNumeralWithMacron(int a,
			Gender gn, Case ce) {
		int i, x, c, m;
		String is, xs, cs, ms;

		if(a >= 1000 && !gn.equals(Gender.NEUTER)) {
			throw new IllegalArgumentException();
		} else if(a <= 0) {
			throw new IllegalArgumentException();
		} else if(a <= 3) {
			i = a - 1;
			return UNUS_DUO_TRES[i][ce.ordinal()][gn.ordinal()];
		} else if(a <= 19) {
			return NUM_19[a];
		} else if(a < 100) {
			is = ((i = a % 10) > 0) ?
					toNumeralWithMacron(i, gn, ce) : "";
			xs = NUM_99[x = a / 10];
			switch(i) {
			case 0:  return xs;
			case 8:  return (x < 3) ?
					DUODE + NUM_99[x + 1] : xs + " " + is;
			case 9:  return (x < 3) ?
					UNDE + NUM_99[x + 1] : xs + " " + is;
			default: return xs + " " + is;
			}
		} else if(a == 100) {
			return CENTUM;
		} else if(a < 200) {
			return CENTUM + " " + toNumeralWithMacron(a % 100, gn, ce);
		} else if(a < 1000) {
			xs = ((c = a % 100) > 0) ?
					" " + toNumeralWithMacron(c, gn, ce) : "";
			cs = PREFIX_900[a / 100];
			return cs + CENTI[ce.ordinal()][gn.ordinal()] + xs;
		} else if(a == 1000) {
			return MILLE;
		} else if(a < 2000) {
			return MILLE + " " + toNumeralWithMacron(a % 1000, gn, ce);
		} else if(a < 1000000) {
			ms = toNumeralWithMacron(a / 1000, gn, ce);
			cs = ((m = a % 1000) > 0) ?
					" " + toNumeralWithMacron(m, gn, ce) : "";
			return ms + " " + MILIA[ce.ordinal()] + cs;
		} else {
			return null;
		}
	}

	/**
	 * 
	 * @param a
	 * @param gn
	 * @param ce
	 * @return
	 */
	public static String toNumeral(int a, Gender gn, Case ce) {
		return removeMacron(toNumeralWithMacron(a, gn, ce));
	}

	//
	private static boolean eqs(String a, String s, boolean mc) {
		return (s != null &&
				(mc ? s.equals(a) : s.equals(removeMacron(a))));
	}

	//
	private static boolean eqss(String a, String s, boolean mc) {
		return (s != null &&
				(mc ? s.startsWith(a) :
					s.startsWith(removeMacron(a))));
	}

	//
	private static int to3(String s, boolean mc) {
		int b = 0;

		if(s == null)  return -1;
		for(int i = 0; i < UNUS_DUO_TRES.length; i++) {
			for(int k = UNUS_DUO_TRES[i][0].length - 1; k >= 0; k--) {
				for(int j = 0; j < UNUS_DUO_TRES[i].length; j++) {
					if(eqs(UNUS_DUO_TRES[i][j][k], s, mc)) {
						b |= (1 << j);
					}
				}
				if(b > 0)  return (i + 1) | (k << 2) | (b << 4);
			}
		}
		return -1;
	}

	//
	private static int to19(String s, boolean mc) {
		if(s == null)  return -1;
		for(int i = 4; i < NUM_19.length; i++) {
			if(eqs(NUM_19[i], s, mc))  return i;
		}
		return -1;
	}

	//
	private static int duodeundetriginta(String s, boolean mc) {
		if(eqs(DUODETRIGINTA, s, mc)) {
			return 28;
		} else if(eqs(UNDETRIGINTA, s, mc)) {
			return 29;
		} else {
			return -1;
		}
	}

	//
	private static int to90(String s, boolean mc) {
		if(s == null)  return -1;
		for(int i = 2; i < NUM_99.length; i++) {
			if(eqs(NUM_99[i], s, mc))  return i;
		}
		return -1;
	}

	//
	private static int to900(String s, boolean mc) {
		String x;
		int b = 0;

		if(s == null)  return -1;
		for(int i = 2; i < PREFIX_900.length; i++) {
			if(eqss(PREFIX_900[i], s, mc)) {
				x = PREFIX_900[i];
				for(int k = CENTI[0].length - 1; k >= 0; k--) {
					for(int j = 0; j < CENTI.length; j++) {
						if(eqs(x + CENTI[j][k], s, mc)) {
							b |= (1 << j);
						}
					}
					if(b > 0)  return i | (k << 4) | (b << 6);
				}
			}
		}
		return -1;
	}

	//
	private static int pmilia(String s, boolean mc) {
		int b = 0;

		if(s == null)  return -1;
		for(int i = 0; i < MILIA.length; i++) {
			if(eqs(MILIA[i], s, mc))  b |= 1 << i;
		}
		return b;
	}

	//
	private static void chkGenderCase(int ce, int gn, int c, int g) {
		if(ce >= 0 && (ce & c) == 0) {
			throw new NumberFormatException("invalid case");
		} else if(gn >= 0 && gn != g) {
			throw new NumberFormatException("invalid gender");
		}
	}

	//
	private static final int INIT = 1000;
	private static final int CENTI_1 = 1010;
	private static final int CENTI_99_1 = 1020;
	private static final int E_OR_MILLE = 1030;
	private static final int INIT_1000 = 1040;
	private static final int CENTI_2 = 1050;
	private static final int CENTI_99_2 = 1060;
	private static final int ACCEPT = 1070;

	//
	static int toNumber(String words, boolean mc) {
		int gn = -1, ce = -1, stat = INIT, a, r = 0, i = 0;
		String[] tok;
		String s;

		if(words == null)  throw new NullPointerException();
		tok = words.split(" ");
		if(tok.length == 0)  throw new IllegalArgumentException();

		while(true) {
			s = (i < tok.length) ? tok[i++] : null;
			switch(stat) {
			case INIT:
				if((a = to3(s, mc)) >= 0) {
					r  = a & 3;
					gn = (a >> 2) & 3;
					ce = (a >> 4);
					stat = E_OR_MILLE;
				} else if((a = to19(s, mc)) > 0) {
					r  = a;
					stat = E_OR_MILLE;
				} else if((a = duodeundetriginta(s, mc)) > 0) {
					r  = a;
					stat = E_OR_MILLE;
				} else if((a = to90(s, mc)) > 0) {
					r  = a * 10;
					stat = CENTI_99_1;
				} else if(eqs(CENTUM, s, mc)) {
					r  = 100;
					stat = CENTI_1;
				} else if((a = to900(s, mc)) >= 0) {
					r  = (a & 15) * 100;
					gn = (a >> 4) & 3;
					ce = (a >> 6);
					stat = CENTI_1;
				} else if(eqs(MILLE, s, mc)) {
					r  = 1000;
					stat = INIT_1000;
				} else {
					throw new NumberFormatException();
				}
				break;
			case CENTI_99_1:
				if((a = to3(s, mc)) >= 0) {
					r  += a & 3;
					gn = (a >> 2) & 3;
					ce = (a >> 4);
					stat = E_OR_MILLE;
				} else if((a = to19(s, mc)) > 0 && a <= 9) {
					r  += a;
					stat = E_OR_MILLE;
				} else if(eqs(MILLE, s, mc)) {
					r  *= 1000;
					stat = INIT_1000;
				} else if((a = pmilia(s, mc)) > 0) {
					chkGenderCase(ce, gn, a, Gender.NEUTER.ordinal());
					r  *= 1000;
					stat = INIT_1000;
				} else if(s == null) {
					return r;
				} else {
					throw new NumberFormatException();
				}
				break;
			case CENTI_1:
				if((a = to3(s, mc)) >= 0) {
					chkGenderCase(ce, gn, (a >> 2) & 3, a >> 4);
					r  += a & 3;
					gn = (a >> 2) & 3;
					ce = (a >> 4);
					stat = E_OR_MILLE;
				} else if((a = to19(s, mc)) > 0) {
					r  += a;
					stat = E_OR_MILLE;
				} else if((a = duodeundetriginta(s, mc)) > 0) {
					r  += a;
					stat = E_OR_MILLE;
				} else if((a = to90(s, mc)) > 0) {
					r  += a * 10;
					stat = CENTI_99_1;
				} else if(eqs(MILLE, s, mc)) {
					r  *= 1000;
					stat = INIT_1000;
				} else if((a = pmilia(s, mc)) > 0) {
					chkGenderCase(ce, gn, a, Gender.NEUTER.ordinal());
					r  *= 1000;
					stat = INIT_1000;
				} else if(s == null) {
					return r;
				} else {
					throw new NumberFormatException();
				}
				break;
			case E_OR_MILLE:
				if(eqs(MILLE, s, mc)) {
					r  = 1000;
					stat = INIT_1000;
				} else if((a = pmilia(s, mc)) > 0) {
					chkGenderCase(ce, gn, a, Gender.NEUTER.ordinal());
					r  *= 1000;
					stat = INIT_1000;
				} else if(s == null) {
					return r;
				} else {
					throw new NumberFormatException();
				}
				break;
			case INIT_1000:
				if((a = to3(s, mc)) >= 0) {
					r  += a & 3;
					gn = (a >> 2) & 3;
					ce = (a >> 4);
					stat = ACCEPT;
				} else if((a = to19(s, mc)) > 0) {
					r  += a;
					stat = ACCEPT;
				} else if((a = duodeundetriginta(s, mc)) > 0) {
					r  += a;
					stat = ACCEPT;
				} else if((a = to90(s, mc)) > 0) {
					r  += a * 10;
					stat = CENTI_99_2;
				} else if(eqs(CENTUM, s, mc)) {
					r  += 100;
					stat = CENTI_2;
				} else if((a = to900(s, mc)) >= 0) {
					r  += (a & 15) * 100;
					gn = (a >> 4) & 3;
					ce = (a >> 6);
					stat = CENTI_2;
				} else if(s == null) {
					return r;
				} else {
					throw new NumberFormatException();
				}
				break;
			case CENTI_99_2:
				if((a = to3(s, mc)) >= 0) {
					r  += a & 3;
					gn = (a >> 2) & 3;
					ce = (a >> 4);
					stat = ACCEPT;
				} else if((a = to19(s, mc)) > 0 && a <= 9) {
					r  += a;
					stat = ACCEPT;
				} else if(s == null) {
					return r;
				} else {
					throw new NumberFormatException();
				}
				break;
			case CENTI_2:
				if((a = to3(s, mc)) >= 0) {
					chkGenderCase(ce, gn, (a >> 2) & 3, a >> 4);
					r  += a & 3;
					gn = (a >> 2) & 3;
					ce = (a >> 4);
					stat = ACCEPT;
				} else if((a = to19(s, mc)) > 0) {
					r  += a;
					stat = ACCEPT;
				} else if((a = duodeundetriginta(s, mc)) > 0) {
					r  += a;
					stat = ACCEPT;
				} else if((a = to90(s, mc)) > 0) {
					r  += a * 10;
					stat = CENTI_99_2;
				} else if(s == null) {
					return r;
				} else {
					throw new NumberFormatException();
				}
				break;
			case ACCEPT:
				if(s == null) {
					return r;
				} else {
					throw new NumberFormatException();
				}
			default:
				throw new RuntimeException();
			}
		}
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static int toNumberWithMacron(String s) {
		return toNumber(s.toLowerCase(), true);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static int toNumber(String s) {
		return toNumber(s.toLowerCase(), false);
	}

}
