package net.morilib.lingua;

import java.util.regex.Pattern;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/01
 */
public class JapaneseUtils {

	//
	private static class HatsuonBuffer {

		private StringBuffer buf = new StringBuffer();
		private boolean hatsuon = false;
		private boolean macron = false;
		private boolean circum = false;
		private boolean oh = false;
		private boolean quote1 = false;
		private boolean nnn = false;
		private boolean hyphen = false;

		HatsuonBuffer(boolean oh, boolean macron,
				boolean circum, boolean quote1, boolean nnn,
				boolean hyphen) {
			this.oh     = oh;
			this.macron = macron;
			this.circum = circum;
			this.quote1 = quote1;
			this.nnn = nnn;
			this.hyphen = hyphen;
		}

		void app(String s) {
			char c = (buf.length() > 0) ?
					buf.charAt(buf.length() - 1) : 0;

			if(nnn || buf.length() == 0 || c != 'n') {
				// do nothing
			} else if(s.charAt(0) == 'b' ||
					s.charAt(0) == 'p' || s.charAt(0) == 'm') {
				buf.setCharAt(buf.length() - 1, 'm');
			}

			if(c == 'n' &&
					(isAiueo(s.charAt(0)) || s.charAt(0) == 'y')) {
				if(quote1) {
					buf.append('\'');
				} else if(hyphen) {
					buf.append('-');
				}
			}

			if(!hatsuon) {
				buf.append(s);
			} else if(isAiueo(s.charAt(0))) {
				throw new IllegalArgumentException();
			} else if(s.charAt(0) == 'c') {
				buf.append('t');
				buf.append(s);
			} else {
				buf.append(s.charAt(0));
				buf.append(s);
			}
			hatsuon = false;
		}

		void append(String s) {
			buf.append(s);
		}

		void append(char c) {
			buf.append(c);
		}

		void setHatsuon() {
			hatsuon = true;
		}

		int length() {
			return buf.length();
		}

		int charAt(int i) {
			return buf.charAt(i);
		}

		void accentize() {
			int l = buf.length();

			if(l == 0) {
				// do nothing
			} else if(macron) {
				switch(buf.charAt(l - 1)) {
				case 'a':  buf.setCharAt(l - 1, 'ā');  break;
				case 'e':  buf.setCharAt(l - 1, 'ē');  break;
				case 'i':  buf.setCharAt(l - 1, 'ī');  break;
				case 'o':  buf.setCharAt(l - 1, 'ō');  break;
				case 'u':  buf.setCharAt(l - 1, 'ū');  break;
				}
			} else if(circum) {
				switch(buf.charAt(l - 1)) {
				case 'a':  buf.setCharAt(l - 1, 'â');  break;
				case 'e':  buf.setCharAt(l - 1, 'ê');  break;
				case 'i':  buf.setCharAt(l - 1, 'î');  break;
				case 'o':  buf.setCharAt(l - 1, 'ô');  break;
				case 'u':  buf.setCharAt(l - 1, 'û');  break;
				}
			} else if(oh) {
				if(isAiueo(buf.charAt(l - 1))) {
					buf.append('h');
				}
			}
		}

		public String toString() {
			return buf.toString();
		}

	}

	//
	private static final int INIT = 1000;
	private static final int H_I  = 1010;
	private static final int H_U  = 1020;
	private static final int H_O  = 1030;
	private static final int H_KI = 1040;
	private static final int H_KU = 1045;
	private static final int H_SI = 1050;
	private static final int H_TI = 1060;
	private static final int H_TU = 1070;
	private static final int H_TE = 1080;
	private static final int H_TO = 1090;
	private static final int H_NI = 1100;
	private static final int H_HI = 1110;
	private static final int H_HU = 1120;
	private static final int H_MI = 1130;
	private static final int H_RI = 1140;
	private static final int H_WA = 1150;
	private static final int H_GI = 1160;
	private static final int H_GU = 1165;
	private static final int H_ZI = 1170;
	private static final int H_DI = 1180;
	private static final int H_DE = 1190;
	private static final int H_BI = 1200;
	private static final int H_PI = 1210;
	private static final int H_VU = 1220;

	//
	private static Pattern PTN1 = Pattern.compile(
			".*\\p{InHiragana}[ァィゥェォャュョッ].*");
	private static Pattern PTN2 = Pattern.compile(
			".*\\p{InKatakana}[ぁぃぅぇぉゃゅょっ].*");

	//
	private static boolean isAiueo(char c) {
		return (c == 'a' || c == 'e' ||
				c == 'i' || c == 'o' || c == 'u');
	}

	//
	private static int rew(int i, String s) {
		int r = i;

		while(s.charAt(--r) == 'ッ');
		return r;
	}

	/**
	 * 
	 */
	public static final int KUNREI = 1;

	/**
	 * 
	 */
	public static final int DZU = 2;

	/**
	 * 
	 */
	public static final int OH = 4;

	/**
	 * 
	 */
	public static final int MACRON = 8;

	/**
	 * 
	 */
	public static final int CIRCUMFLEX = 16;

	/**
	 * 
	 */
	public static final int USE_QUOTE = 32;

	/**
	 * 
	 */
	public static final int FORCE_N = 64;

	/**
	 * 
	 */
	public static final int USE_HYPHEN = 128;

	/**
	 * 
	 * @param str
	 * @param flags
	 * @return
	 */
	public static String toRomaji(String str, int flags) {
		HatsuonBuffer b;
		boolean kun, dzu, oh, mcr, cir, qu1, nnn, hyp;
		String s = str;
		int stat = INIT, c;

		if(PTN1.matcher(s).matches() || PTN2.matcher(s).matches()) {
			throw new IllegalArgumentException();
		}
		s = _katakana(s);

		kun = (flags & 1)   != 0;
		dzu = (flags & 2)   != 0;
		oh  = (flags & 4)   != 0;
		mcr = (flags & 8)   != 0;
		cir = (flags & 16)  != 0 || kun;
		qu1 = (flags & 32)  != 0 || kun;
		nnn = (flags & 64)  != 0 || kun;
		hyp = (flags & 128) != 0;
		b = new HatsuonBuffer(oh, mcr, cir, qu1, nnn, hyp);
		for(int i = 0; true; i++) {
			c = (i < s.length()) ? s.charAt(i) : -1;
			if(c == 'ッ') {
				if(i == 0)  throw new IllegalArgumentException();
				continue;
			} else if(i > 0 && s.charAt(i - 1) == 'ッ') {
				if(stat == INIT)  b.setHatsuon();
			}

			switch(stat) {
			case INIT:
//				if(spc && i > 0)  b.append(" ");
				switch(c) {
				case 'ア':  case 'ァ':  b.app("a");  break;
				case 'イ':  stat = H_I;  break;
				case 'ウ':  case 'ぅ':  stat = H_U;  break;
				case 'エ':  case 'ぇ':  b.app("e");  break;
				case 'オ':  stat = H_O;  break;
				case 'カ':  b.app("ka");  break;
				case 'キ':  stat = H_KI;  break;
				case 'ク':  stat = H_KU;  break;
				case 'ケ':  b.app("ke");  break;
				case 'コ':  b.app("ko");  break;
				case 'サ':  b.app("sa");  break;
				case 'シ':  stat = H_SI;  break;
				case 'ス':  b.app("su");  break;
				case 'セ':  b.app("se");  break;
				case 'ソ':  b.app("so");  break;
				case 'タ':  b.app("ta");  break;
				case 'チ':  stat = H_TI;  break;
				case 'ツ':  stat = H_TU;  break;
				case 'テ':  stat = H_TE;  break;
				case 'ト':  stat = H_TO;  break;
				case 'ナ':  b.app("na");  break;
				case 'ニ':  stat = H_NI;  break;
				case 'ヌ':  b.app("nu");  break;
				case 'ネ':  b.app("ne");  break;
				case 'ノ':  b.app("no");  break;
				case 'ハ':  b.app("ha");  break;
				case 'ヒ':  stat = H_HI;  break;
				case 'フ':  stat = H_HU;  break;
				case 'ヘ':  b.app("he");  break;
				case 'ホ':  b.app("ho");  break;
				case 'マ':  b.app("ma");  break;
				case 'ミ':  stat = H_MI;  break;
				case 'ム':  b.app("mu");  break;
				case 'メ':  b.app("me");  break;
				case 'モ':  b.app("mo");  break;
				case 'ヤ':  case 'ャ':  b.app("ya");  break;
				case 'ユ':  case 'ュ':  b.app("yu");  break;
				case 'ヨ':  case 'ョ':  b.app("yo");  break;
				case 'ラ':  b.app("ra");  break;
				case 'リ':  stat = H_RI;  break;
				case 'ル':  b.app("ru");  break;
				case 'レ':  b.app("re");  break;
				case 'ロ':  b.app("ro");  break;
				case 'ワ':  stat = H_WA;  break;
				case 'ヰ':  b.app(kun ? "i" : "wi");  break;
				case 'ヱ':  b.app(kun ? "e" : "we");  break;
				case 'ヲ':  b.app(kun ? "o" : "wo");  break;
				case 'ン':  b.app("n");  break;
				case 'ガ':  b.app("ga");  break;
				case 'ギ':  stat = H_GI;  break;
				case 'グ':  stat = H_GU;  break;
				case 'ゲ':  b.app("ge");  break;
				case 'ゴ':  b.app("go");  break;
				case 'ザ':  b.app("za");  break;
				case 'ジ':  stat = H_ZI;  break;
				case 'ズ':  b.app("zu");  break;
				case 'ゼ':  b.app("ze");  break;
				case 'ゾ':  b.app("zo");  break;
				case 'ダ':  b.app("da");  break;
				case 'ヂ':  stat = H_DI;  break;
				case 'ヅ':  b.app(dzu ? "dzu" : "zu");  break;
				case 'デ':  stat = H_DE;  break;
				case 'ド':  b.app("do");  break;
				case 'バ':  b.app("ba");  break;
				case 'ビ':  stat = H_BI;  break;
				case 'ブ':  b.app("bu");  break;
				case 'ベ':  b.app("be");  break;
				case 'ボ':  b.app("bo");  break;
				case 'パ':  b.app("pa");  break;
				case 'ピ':  stat = H_PI;  break;
				case 'プ':  b.app("pu");  break;
				case 'ペ':  b.app("pe");  break;
				case 'ポ':  b.app("po");  break;
				case 'ー':  b.accentize();  break;
				case 'ィ':  b.app("i");  break;
				case 'ォ':  b.app("o");  break;
				case 'ヮ':  b.app("wa");  break;
				case 'ヴ':  stat = H_VU;  break;
				case '。':  b.append(".");  break;
				case '、':  b.append(",");  break;
				case -1:  break;
				default:  b.append((char)c);  break;
				}
				break;
			case H_I:
				switch(c) {
				case 'ェ':  b.app("ye");  break;
				default:  b.app("i");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_U:
				switch(c) {
				case 'ァ':  b.app("wa");  break;
				case 'ィ':  b.app("wi");  break;
				case 'ェ':  b.app("we");  break;
				case 'ォ':  b.app("wo");  break;
				default:
					if(b.charAt(b.length() - 1) != 'o') {
						b.app("u");
					} else {
						b.accentize();
					}
					i = rew(i, s);
					break;
				}
				stat = INIT;
				break;
			case H_O:
				switch(c) {
				case 'オ':
					b.app(mcr ? "ō" : cir ? "ô" : oh ? "oh" : "o");
					break;
				default:  b.app("o");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_KI:
				switch(c) {
				case 'ャ':  b.app("kya");  break;
				case 'ュ':  b.app("kyu");  break;
				case 'ェ':  b.app("kye");  break;
				case 'ョ':  b.app("kyo");  break;
				default:  b.app("ki");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_KU:
				switch(c) {
				case 'ヮ':
				case 'ァ':  b.app(kun ? "kwa" : "qua");  break;
				case 'ィ':  b.app(kun ? "kwi" : "qui");  break;
				case 'ェ':  b.app(kun ? "kwe" : "que");  break;
				case 'ォ':  b.app(kun ? "kwo" : "quo");  break;
				default:  b.app("ku");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_SI:
				switch(c) {
				case 'ャ':  b.app(kun ? "sya" : "sha");  break;
				case 'ュ':  b.app(kun ? "syu" : "shu");  break;
				case 'ェ':  b.app(kun ? "sye" : "she");  break;
				case 'ョ':  b.app(kun ? "syo" : "sho");  break;
				default:
					b.app(kun ? "si" : "shi");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_TI:
				switch(c) {
				case 'ャ':  b.app(kun ? "tya" : "cha");  break;
				case 'ュ':  b.app(kun ? "tyu" : "chu");  break;
				case 'ェ':  b.app(kun ? "tye" : "che");  break;
				case 'ョ':  b.app(kun ? "tyo" : "cho");  break;
				default:
					b.app(kun ? "ti" : "chi");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_TU:
				switch(c) {
				case 'ァ':  b.app("tsa");  break;
				case 'ィ':  b.app("tsi");  break;
				case 'ェ':  b.app("tse");  break;
				case 'ォ':  b.app("tso");  break;
				default:
					b.app(kun ? "tu" : "tsu");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_TE:
				switch(c) {
				case 'ィ':  b.app("ti");  break;
				default:  b.app("te");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_TO:
				switch(c) {
				case 'ゥ':  b.app("tu");  break;
				default:  b.app("to");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_HI:
				switch(c) {
				case 'ャ':  b.app("hya");  break;
				case 'ュ':  b.app("hyu");  break;
				case 'ェ':  b.app("hye");  break;
				case 'ョ':  b.app("hyo");  break;
				default:  b.app("hi");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_HU:
				switch(c) {
				case 'ァ':  b.app("fa");  break;
				case 'ィ':  b.app("fi");  break;
				case 'ェ':  b.app("fe");  break;
				case 'ォ':  b.app("fo");  break;
				default:
					b.app(kun ? "hu" : "fu");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_MI:
				switch(c) {
				case 'ャ':  b.app("mya");  break;
				case 'ュ':  b.app("myu");  break;
				case 'ェ':  b.app("mye");  break;
				case 'ョ':  b.app("myo");  break;
				default:  b.app("mi");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_NI:
				switch(c) {
				case 'ャ':  b.app("nya");  break;
				case 'ュ':  b.app("nyu");  break;
				case 'ェ':  b.app("nye");  break;
				case 'ョ':  b.app("nyo");  break;
				default:  b.app("ni");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_RI:
				switch(c) {
				case 'ャ':  b.app("rya");  break;
				case 'ュ':  b.app("ryu");  break;
				case 'ェ':  b.app("rye");  break;
				case 'ョ':  b.app("ryo");  break;
				default:  b.app("ri");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_WA:
				switch(c) {
				case 'ァ':  b.app("wa");  break;
				default:  b.app("wa");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_GI:
				switch(c) {
				case 'ャ':  b.app("gya");  break;
				case 'ュ':  b.app("gyu");  break;
				case 'ェ':  b.app("gye");  break;
				case 'ョ':  b.app("gyo");  break;
				default:  b.app("gi");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_GU:
				switch(c) {
				case 'ヮ':  case 'ァ':  b.app("gwa");  break;
				case 'ィ':  b.app("gwi");  break;
				case 'ェ':  b.app("gwe");  break;
				case 'ォ':  b.app("gwo");  break;
				default:  b.app("gu");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_ZI:
				switch(c) {
				case 'ャ':  b.app(kun ? "zya" : "ja");  break;
				case 'ュ':  b.app(kun ? "zyu" : "ju");  break;
				case 'ェ':  b.app(kun ? "zye" : "je");  break;
				case 'ョ':  b.app(kun ? "zyo" : "jo");  break;
				default:
					b.app(kun ? "zi" : "ji");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_DI:
				switch(c) {
				case 'ャ':  b.app(kun ? "dya" : "ja");  break;
				case 'ュ':  b.app(kun ? "dyu" : "ju");  break;
				case 'ェ':  b.app(kun ? "dye" : "je");  break;
				case 'ョ':  b.app(kun ? "dyo" : "jo");  break;
				default:
					b.app(kun ? "zi" : "ji");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_DE:
				switch(c) {
				case 'ィ':  b.app("di");  break;
				default:  b.app("de");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_BI:
				switch(c) {
				case 'ャ':  b.app("bya");  break;
				case 'ュ':  b.app("byu");  break;
				case 'ェ':  b.app("bye");  break;
				case 'ョ':  b.app("byo");  break;
				default:  b.app("bi");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_PI:
				switch(c) {
				case 'ャ':  b.app("pya");  break;
				case 'ュ':  b.app("pyu");  break;
				case 'ェ':  b.app("pye");  break;
				case 'ョ':  b.app("pyo");  break;
				default:  b.app("pi");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			case H_VU:
				switch(c) {
				case 'ァ':  b.app("va");  break;
				case 'ィ':  b.app("vi");  break;
				case 'ェ':  b.app("ve");  break;
				case 'ォ':  b.app("vo");  break;
				default:  b.app("vu");  i = rew(i, s);  break;
				}
				stat = INIT;
				break;
			}
			if(c < 0)  return b.toString();
		}
	}

	//
	static String _katakana(String s) {
		StringBuffer b = new StringBuffer();
		char c;

		for(int i = 0; i < s.length(); i++) {
			c = s.charAt(i);
			if(Character.UnicodeBlock.of(c).equals(
					Character.UnicodeBlock.HIRAGANA)) {
				b.append((char)(c + 0x60));
			} else {
				b.append(c);
			}
		}
		return b.toString();
	}

	//
	private static final int K_INIT = 1000;
	private static final int K_K = 1010;
	private static final int K_S = 1020;
	private static final int K_T = 1030;
	private static final int K_N = 1040;
	private static final int K_H = 1050;
	private static final int K_M = 1060;
	private static final int K_Y = 1070;
	private static final int K_R = 1080;
	private static final int K_W = 1090;
	private static final int K_G = 1100;
	private static final int K_Z = 1110;
	private static final int K_B = 1120;
	private static final int K_D = 1130;
	private static final int K_J = 1140;
	private static final int K_C = 1150;
	private static final int K_F = 1160;
	private static final int K_L = 1170;
	private static final int K_P = 1180;
	private static final int K_Q = 1190;
	private static final int K_X = 1200;
	private static final int K_V = 1210;
	private static final int K_BY = 1220;
	private static final int K_CH = 1230;
	private static final int K_GY = 1240;
	private static final int K_GW = 1245;
	private static final int K_HY = 1250;
	private static final int K_KY = 1260;
	private static final int K_MY = 1270;
	private static final int K_NY = 1280;
	private static final int K_PY = 1290;
	private static final int K_QU = 1300;
	private static final int K_RY = 1310;
	private static final int K_SH = 1320;
	private static final int K_TS = 1330;
	private static final int K_ZY = 1340;

	/**
	 * 
	 * @param s
	 * @param kun
	 * @return
	 */
	public static String toKatakana(String s, boolean kun) {
		StringBuffer b = new StringBuffer();
		int stat = K_INIT;
		char c, a = 0, x = 0;

		for(int i = 0; i < s.length(); i++, x = a, a = c) {
			c = s.charAt(i);
			switch(stat) {
			case K_INIT:
				switch(c) {
				case 'a':  b.append("ア");  break;
				case 'b':  stat = K_B;  break;
				case 'c':  stat = K_C;  break;
				case 'd':  stat = K_D;  break;
				case 'e':  b.append("エ");  break;
				case 'f':  stat = K_F;  break;
				case 'g':  stat = K_G;  break;
				case 'h':  stat = K_H;  break;
				case 'i':  b.append("イ");  break;
				case 'j':  stat = K_J;  break;
				case 'k':  stat = K_K;  break;
				case 'l':  stat = K_L;  break;
				case 'm':  stat = K_M;  break;
				case 'n':  stat = K_N;  break;
				case 'o':  b.append("オ");  break;
				case 'p':  stat = K_P;  break;
				case 'q':  stat = K_Q;  break;
				case 'r':  stat = K_R;  break;
				case 's':  stat = K_S;  break;
				case 't':  stat = K_T;  break;
				case 'u':  b.append("ウ");  break;
				case 'v':  stat = K_V;  break;
				case 'w':  stat = K_W;  break;
				case 'x':  stat = K_X;  break;
				case 'y':  stat = K_Y;  break;
				case 'z':  stat = K_Z;  break;
				default:  b.append(c);  break;
				case 'ā':  case 'â':  b.append("アー");  break;
				case 'ē':  case 'ê':  b.append("エー");  break;
				case 'ī':  case 'î':  b.append("イー");  break;
				case 'ō':  case 'ô':  b.append("オー");  break;
				case 'ū':  case 'û':  b.append("ウー");  break;
				}
				break;
			case K_B:
				switch(c) {
				case 'a':  b.append("バ");  stat = INIT;  break;
				case 'e':  b.append("ベ");  stat = INIT;  break;
				case 'i':  b.append("ビ");  stat = INIT;  break;
				case 'o':  b.append("ボ");  stat = INIT;  break;
				case 'u':  b.append("ブ");  stat = INIT;  break;
				case 'b':  b.append("ッ");  break;
				case 'y':  stat = K_BY;  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_C:
				switch(c) {
				case 'h':  stat = K_CH;  break;
				case 'c':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_D:
				switch(c) {
				case 'a':  b.append("ダ");  stat = INIT;  break;
				case 'e':  b.append("デ");  stat = INIT;  break;
				case 'i':  b.append("ディ");  stat = INIT;  break;
				case 'o':  b.append("ド");  stat = INIT;  break;
				case 'u':  b.append("ドゥ");  stat = INIT;  break;
				case 'd':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_F:
				switch(c) {
				case 'a':  b.append("ファ");  stat = INIT;  break;
				case 'e':  b.append("フェ");  stat = INIT;  break;
				case 'i':  b.append("フィ");  stat = INIT;  break;
				case 'o':  b.append("フォ");  stat = INIT;  break;
				case 'u':  b.append("フ");  stat = INIT;  break;
				case 'f':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_G:
				switch(c) {
				case 'a':  b.append("ガ");  stat = INIT;  break;
				case 'e':  b.append("ゲ");  stat = INIT;  break;
				case 'i':  b.append("ギ");  stat = INIT;  break;
				case 'o':  b.append("ゴ");  stat = INIT;  break;
				case 'u':  b.append("グ");  stat = INIT;  break;
				case 'y':  stat = K_GY;  break;
				case 'w':  stat = K_GW;  break;
				case 'g':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_H:
				switch(c) {
				case 'a':  b.append("ハ");  stat = INIT;  break;
				case 'e':  b.append("ヘ");  stat = INIT;  break;
				case 'i':  b.append("ヒ");  stat = INIT;  break;
				case 'o':  b.append("ホ");  stat = INIT;  break;
				case 'u':  b.append("フ");  stat = INIT;  break;
				case 'y':  stat = K_HY;  break;
				case 'h':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_J:
				switch(c) {
				case 'a':  b.append("ジャ");  stat = INIT;  break;
				case 'e':  b.append("ジェ");  stat = INIT;  break;
				case 'i':  b.append("ジ");  stat = INIT;  break;
				case 'o':  b.append("ジョ");  stat = INIT;  break;
				case 'u':  b.append("ジュ");  stat = INIT;  break;
				case 'y':  break;
				case 'j':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_K:
				switch(c) {
				case 'a':  b.append("カ");  stat = INIT;  break;
				case 'e':  b.append("ケ");  stat = INIT;  break;
				case 'i':  b.append("キ");  stat = INIT;  break;
				case 'o':  b.append("コ");  stat = INIT;  break;
				case 'u':  b.append("ク");  stat = INIT;  break;
				case 'y':  stat = K_KY;  break;
				case 'w':  stat = K_QU;  break;
				case 'k':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_L:
				switch(c) {
				case 'a':  b.append("ラ");  stat = INIT;  break;
				case 'e':  b.append("レ");  stat = INIT;  break;
				case 'i':  b.append("リ");  stat = INIT;  break;
				case 'o':  b.append("ロ");  stat = INIT;  break;
				case 'u':  b.append("ル");  stat = INIT;  break;
				case 'l':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_M:
				switch(c) {
				case 'a':  b.append("マ");  stat = INIT;  break;
				case 'e':  b.append("メ");  stat = INIT;  break;
				case 'i':  b.append("ミ");  stat = INIT;  break;
				case 'o':  b.append("モ");  stat = INIT;  break;
				case 'u':  b.append("ム");  stat = INIT;  break;
				case 'y':  stat = K_MY;  break;
				case 'm':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_N:
				switch(c) {
				case 'a':  b.append("ナ");  stat = INIT;  break;
				case 'e':  b.append("ネ");  stat = INIT;  break;
				case 'i':  b.append("ニ");  stat = INIT;  break;
				case 'o':  b.append("ノ");  stat = INIT;  break;
				case 'u':  b.append("ヌ");  stat = INIT;  break;
				case 'y':  stat = K_NY;  break;
				case 'n':  b.append("ン");  break;
				default:
					b.append("ン");  i--;  stat = INIT;  break;
				}
				break;
			case K_P:
				switch(c) {
				case 'a':  b.append("パ");  stat = INIT;  break;
				case 'e':  b.append("ペ");  stat = INIT;  break;
				case 'i':  b.append("ピ");  stat = INIT;  break;
				case 'o':  b.append("ポ");  stat = INIT;  break;
				case 'u':  b.append("プ");  stat = INIT;  break;
				case 'y':  stat = K_PY;  break;
				case 'p':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_Q:
				switch(c) {
				case 'u':  stat = K_QU;  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_R:
				switch(c) {
				case 'a':  b.append("ラ");  stat = INIT;  break;
				case 'e':  b.append("レ");  stat = INIT;  break;
				case 'i':  b.append("リ");  stat = INIT;  break;
				case 'o':  b.append("ロ");  stat = INIT;  break;
				case 'u':  b.append("ル");  stat = INIT;  break;
				case 'y':  stat = K_RY;  break;
				case 'r':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_S:
				switch(c) {
				case 'a':  b.append("サ");  stat = INIT;  break;
				case 'e':  b.append("セ");  stat = INIT;  break;
				case 'i':
					b.append(kun ? "シ" : "スィ");  stat = INIT;  break;
				case 'o':  b.append("ソ");  stat = INIT;  break;
				case 'u':  b.append("ス");  stat = INIT;  break;
				case 'h':  stat = K_SH;  break;
				case 'y':  stat = K_SH;  break;
				case 's':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_T:
				switch(c) {
				case 'a':  b.append("タ");  stat = INIT;  break;
				case 'e':  b.append("テ");  stat = INIT;  break;
				case 'i':
					b.append(kun ? "チ" : "ティ");  stat = INIT;  break;
				case 'o':  b.append("ト");  stat = INIT;  break;
				case 'u':
					b.append(kun ? "ツ" : "トゥ");  stat = INIT;  break;
				case 's':  stat = K_TS;  break;
				case 'y':  stat = K_CH;  break;
				case 't':  b.append("ッ");  break;
				case 'c':  b.append("ッ");  stat = K_CH;  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_V:
				switch(c) {
				case 'a':  b.append("ヴァ");  stat = INIT;  break;
				case 'e':  b.append("ヴェ");  stat = INIT;  break;
				case 'i':  b.append("ヴィ");  stat = INIT;  break;
				case 'o':  b.append("ヴォ");  stat = INIT;  break;
				case 'u':  b.append("ヴ");  stat = INIT;  break;
				case 'v':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_W:
				switch(c) {
				case 'a':  b.append("ワ");  stat = INIT;  break;
				case 'e':  b.append("ウェ");  stat = INIT;  break;
				case 'i':  b.append("ウィ");  stat = INIT;  break;
				case 'o':  b.append("ウォ");  stat = INIT;  break;
				case 'u':  b.append("ウ");  stat = INIT;  break;
				case 'w':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_X:
				switch(c) {
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_Y:
				switch(c) {
				case 'a':  b.append("ヤ");  stat = INIT;  break;
				case 'e':  b.append("イェ");  stat = INIT;  break;
				case 'i':  b.append("イ");  stat = INIT;  break;
				case 'o':  b.append("ヨ");  stat = INIT;  break;
				case 'u':  b.append("ユ");  stat = INIT;  break;
				case 'y':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_Z:
				switch(c) {
				case 'a':  b.append("ザ");  stat = INIT;  break;
				case 'e':  b.append("ゼ");  stat = INIT;  break;
				case 'i':
					b.append(kun ? "ジ" : "ズィ");
					stat = INIT;  break;
				case 'o':  b.append("ゾ");  stat = INIT;  break;
				case 'u':  b.append("ズ");  stat = INIT;  break;
				case 'y':  stat = K_ZY;  break;
				case 'z':  b.append("ッ");  break;
				default:
					b.append(a);  b.append(c);  stat = INIT;  break;
				}
				break;
			case K_BY:
				switch(c) {
				case 'a':  b.append("ビャ");  stat = INIT;  break;
				case 'e':  b.append("ビェ");  stat = INIT;  break;
				case 'o':  b.append("ビョ");  stat = INIT;  break;
				case 'u':  b.append("ビュ");  stat = INIT;  break;
				default:
					b.append(x);  b.append(a);  b.append(c);
					stat = INIT;  break;
				}
				break;
			case K_CH:
				switch(c) {
				case 'a':  b.append("チャ");  break;
				case 'e':  b.append("チェ");  break;
				case 'i':  b.append("チ");  break;
				case 'o':  b.append("チョ");  break;
				case 'u':  b.append("チュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_GY:
				switch(c) {
				case 'a':  b.append("ギャ");  break;
				case 'e':  b.append("ギェ");  break;
				case 'o':  b.append("ギョ");  break;
				case 'u':  b.append("ギュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_GW:
				switch(c) {
				case 'a':  b.append("グァ");  break;
				case 'e':  b.append("グェ");  break;
				case 'i':  b.append("グィ");  break;
				case 'o':  b.append("グォ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_HY:
				switch(c) {
				case 'a':  b.append("ヒャ");  break;
				case 'e':  b.append("ヒェ");  break;
				case 'o':  b.append("ヒョ");  break;
				case 'u':  b.append("ヒュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_KY:
				switch(c) {
				case 'a':  b.append("キャ");  break;
				case 'e':  b.append("キェ");  break;
				case 'o':  b.append("キョ");  break;
				case 'u':  b.append("キュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_MY:
				switch(c) {
				case 'a':  b.append("ミャ");  break;
				case 'e':  b.append("ミェ");  break;
				case 'o':  b.append("ミョ");  break;
				case 'u':  b.append("ミュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_NY:
				switch(c) {
				case 'a':  b.append("ニャ");  break;
				case 'e':  b.append("ニェ");  break;
				case 'o':  b.append("ニョ");  break;
				case 'u':  b.append("ニュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_PY:
				switch(c) {
				case 'a':  b.append("ピャ");  break;
				case 'e':  b.append("ピェ");  break;
				case 'o':  b.append("ピョ");  break;
				case 'u':  b.append("ピュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_QU:
				switch(c) {
				case 'a':  b.append("クァ");  break;
				case 'e':  b.append("クェ");  break;
				case 'i':  b.append("クィ");  break;
				case 'o':  b.append("クォ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_RY:
				switch(c) {
				case 'a':  b.append("リャ");  break;
				case 'e':  b.append("リェ");  break;
				case 'o':  b.append("リョ");  break;
				case 'u':  b.append("リュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_SH:
				switch(c) {
				case 'a':  b.append("シャ");  break;
				case 'e':  b.append("シェ");  break;
				case 'i':  b.append("シ");  break;
				case 'o':  b.append("ショ");  break;
				case 'u':  b.append("シュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_TS:
				switch(c) {
				case 'a':  b.append("ツァ");  break;
				case 'e':  b.append("ツェ");  break;
				case 'i':  b.append("ツィ");  break;
				case 'o':  b.append("ツォ");  break;
				case 'u':  b.append("ツ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			case K_ZY:
				switch(c) {
				case 'a':  b.append("ジャ");  break;
				case 'e':  b.append("ジェ");  break;
				case 'o':  b.append("ジョ");  break;
				case 'u':  b.append("ジュ");  break;
				default:
					b.append(x);  b.append(a);  b.append(c);  break;
				}
				stat = INIT;
				break;
			}
		}

		if(stat == K_N)  b.append("ン");
		return b.toString();
	}

}
