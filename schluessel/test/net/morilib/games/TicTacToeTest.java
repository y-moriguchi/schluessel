package net.morilib.games;

import junit.framework.TestCase;

public class TicTacToeTest extends TestCase {

	public void testWin1() {
		assertEquals(TicTacToe.think(0220110000, true), 0220111000);
		assertEquals(TicTacToe.think(0210210000, true), 0210210010);
		assertEquals(TicTacToe.think(0120120000, false), 0120120020);
	}

	public void testWin2() {
		assertEquals(TicTacToe.think(0100000000, false), 0100020000);
		assertEquals(TicTacToe.think(0021000000, true),  0021010000);
		assertEquals(TicTacToe.think(0021010000, false), 0021010200);
		assertEquals(TicTacToe.think(0021010200, true),  0021010201);
		assertEquals(TicTacToe.think(0021010201, false), TicTacToe.O_WIN);
	}

	private static int play(int x, boolean t) {
		while(x >= 0) {
//			System.out.println(Integer.toOctalString(x) + " " + t);
			x = TicTacToe.think(x, t);  t = !t;
		}
		return x;
	}

	public void testCannotWin1() {
		assertEquals(play(0, true), TicTacToe.EVEN);
	}

	public void testCannotWin2() {
		assertEquals(play(0200010000, true), TicTacToe.EVEN);
	}

	public void testCannotWin3() {
		assertEquals(play(0210000000, true), TicTacToe.EVEN);
	}

	public void testWin3() {
		assertEquals(play(0021000000, true), TicTacToe.O_WIN);
	}

	public void testWin4() {
		assertEquals(play(0000120000, true), TicTacToe.O_WIN);
	}

}
