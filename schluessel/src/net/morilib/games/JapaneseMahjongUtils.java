package net.morilib.games;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/08
 */
public final class JapaneseMahjongUtils {

	/**
	 * 
	 */
	public static final int UNLIMITED = 2;

	/**
	 * 
	 */
	public static final int OYA = 1;

	//
	private JapaneseMahjongUtils() {}

	//
	private static int scoreBase(int f, int h, boolean ul) {
		int sc = (f + 9) / 10 * 10;

		if(ul) {
			sc = sc * (1 << (h + 2));
		} else if(h > 12) {
			sc = 8000;
		} else if(h > 10) {
			sc = 6000;
		} else if(h > 7) {
			sc = 4000;
		} else if(h > 5) {
			sc = 3000;
		} else {
			sc = sc * (1 << (h + 2));
			if(sc >= 2000)  sc = 2000;
		}
		return sc;
	}

	/**
	 * 
	 * @param fu
	 * @param fan
	 * @param flags
	 * @return
	 */
	public static int scoreRong(int fu, int fan, int flags) {
		boolean ul = (flags & 2) != 0;
		int sc;

		sc = scoreBase(fu, fan, ul);
		sc = sc * (((flags & 1) == 0) ? 4 : 6);
		sc = (sc + 99) / 100 * 100;
		return sc;
	}

	/**
	 * 
	 * @param fu
	 * @param fan
	 * @param flags
	 * @return
	 */
	public static int[] scoreZimo(int fu, int fan, int flags) {
		boolean ul = (flags & 2) != 0;
		int sc, s1, s2;

		sc = scoreBase(fu, fan, ul);
		s2 = sc * 2;
		s2 = (s2 + 99) / 100 * 100;
		s1 = (sc + 99) / 100 * 100;
		if((flags & 1) == 0) {
			return new int[] { s2, s1, s1 };
		} else {
			return new int[] { s2, s2, s2 };
		}
	}

}
