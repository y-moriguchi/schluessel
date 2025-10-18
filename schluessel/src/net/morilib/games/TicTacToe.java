package net.morilib.games;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/09
 */
public class TicTacToe {

	/**
	 * 
	 */
	public static final int O_WIN = -1;

	/**
	 * 
	 */
	public static final int X_WIN = -2;

	/**
	 * 
	 */
	public static final int EVEN = -3;

	//
	private static boolean isok(int x, int p, boolean o) {
		int m = (p << (o ? 0 : 1));

		return (x & m) == m;
	}

	/**
	 * 
	 * @param x
	 * @param o
	 * @return
	 */
	public static boolean isWin(int x, boolean o) {
		return (isok(x, 0111000000, o) ||
				isok(x, 0000111000, o) ||
				isok(x, 0000000111, o) ||
				isok(x, 0100100100, o) ||
				isok(x, 0010010010, o) ||
				isok(x, 0001001001, o) ||
				isok(x, 0100010001, o) ||
				isok(x, 0001010100, o));
	}

	/**
	 * 
	 * @param x
	 * @param o
	 * @return
	 */
	public static int think(int x, boolean o) {
		Map<Integer, Integer> m = new HashMap<Integer, Integer>();
		int[] z = new int[512];
		int[] q = new int[512];
		int p = 1, c, n = -500000, r = -1, s;
		boolean t = o;

		if(isWin(x, true)) {
			return O_WIN;
		} else if(isWin(x, false)) {
			return X_WIN;
		} else if(x == 0) {
			return 0100000000;
		}

		z[0] = x;  q[1] = 0;
		while(p > 0) {
			if(q[p] >= 9) {
				q[--p]++;  t = !t;  continue;
			} else if(p == 2 && isWin(z[1], o)) {
				return z[1];
			} else if(p == 3 && isWin(z[2], !o)) {
				m.put(z[1], -10000000);
				q[--p]++;  t = !t;  continue;
			} else if(isWin(z[p - 1], o)) {
				c = m.get(z[1]) == null ? 0 : m.get(z[1]);
				m.put(z[1], c + 1);
				q[--p]++;  t = !t;  continue;
			} else if(isWin(z[p - 1], !o)) {
				c = m.get(z[1]) == null ? 0 : m.get(z[1]);
				m.put(z[1], c - 1);
				q[--p]++;  t = !t;  continue;
			}

			if((z[p - 1] & (7 << (q[p] * 3))) == 0) {
				z[p] = z[p - 1] | ((t ? 1 : 2) << (q[p] * 3));
				q[++p] = 0;  t = !t;
			} else {
				q[p]++;
			}
		}

		for(int i = 0; i < 27; i += 3) {
			if((x & (7 << i)) == 0) {
				s = x | ((o ? 1 : 2) << i);
				if(!m.containsKey(s))  m.put(s, -100000);
			}
		}

		for(int w : m.keySet()) {
			if(m.get(w) > n)  n = m.get(r = w);
		}
		return (r > 0) ? r : m.isEmpty() ? EVEN : o ? X_WIN : O_WIN;
	}

	/**
	 * 
	 * @param board
	 * @return
	 */
	public static String toString(int board) {
		StringBuffer b = new StringBuffer();

		for(int i = 24; i >= 0; i -= 3) {
			switch((board >> i) & 7) {
			case 0:  b.append('.');  break;
			case 1:  b.append('o');  break;
			case 2:  b.append('x');  break;
			default:  throw new IllegalArgumentException();
			}
			if(i % 9 == 0)  b.append('\n');
		}
		return b.toString();
	}

	/**
	 * 
	 * @param board
	 * @return
	 */
	public static int think(int board) {
		return think(board, whichTurn(board));
	}

	/**
	 * 
	 * @param board
	 * @return
	 */
	public static boolean whichTurn(int board) {
		int o = 0, x = 0;

		for(int i = 24; i >= 0; i -= 3) {
			switch((board >> i) & 7) {
			case 0:  break;
			case 1:  o++;  break;
			case 2:  x++;  break;
			default:  throw new IllegalArgumentException();
			}
		}

		switch(o - x) {
		case 0:  return true;
		case 1:  return false;
		default:  throw new IllegalArgumentException();
		}
	}

	/**
	 * 
	 * @param board
	 * @param x
	 * @param y
	 * @param turn
	 * @return
	 */
	public static int put(int board, int x, int y, boolean turn) {
		int xy = 24 - (y * 3 + x) * 3;

		if(x < 0 || x > 2 || y < 0 || y > 2) {
			throw new IndexOutOfBoundsException();
		} else if((board & (7 << xy)) != 0) {
			return -1;
		} else {
			return board | ((turn ? 1 : 2) << xy);
		}
	}

	/**
	 * 
	 * @param board
	 * @param x
	 * @param y
	 * @return
	 */
	public static int put(int board, int x, int y) {
		return put(board, x, y, whichTurn(board));
	}

	/**
	 * 
	 * @param board
	 * @return
	 */
	public static boolean isEven(int board) {
		int o = 0, x = 0;

		for(int i = 24; i >= 0; i -= 3) {
			switch((board >> i) & 7) {
			case 0:  break;
			case 1:  o++;  break;
			case 2:  x++;  break;
			default:  throw new IllegalArgumentException();
			}
		}
		return o + x == 9;
	}

}
