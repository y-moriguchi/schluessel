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
package net.morilib.lisp.games;

import net.morilib.games.TicTacToe;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.InputPort;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/09
 */
public class PlayTicTacToe extends Subr {

	//
	private Datum play(boolean yourTurn, LispMessage mesg) {
		InputPort ip = InputPort.getStandard(mesg);
		boolean turn = true;
		int board = 0, x, y;
		String[] t;
		String s;

		while(!TicTacToe.isEven(board)) {
			if(yourTurn == turn) {
				System.out.print(TicTacToe.toString(board));
				while(true) {
					System.out.print("x,y> ");
					s = ip.readLine();
					t = s.split(",");
					if(t.length != 2)  continue;
					try {
						x = Integer.parseInt(t[0].trim());
						y = Integer.parseInt(t[1].trim());
						if(x >= 0 && x < 3 && y >= 0 && y < 3) {
							break;
						}
					} catch(NumberFormatException e) {
						// ignore
					}
				}
				board = TicTacToe.put(board, x, y);
			} else {
				board = TicTacToe.think(board);
			}

			if(TicTacToe.isWin(board, yourTurn)) {
				return Symbol.getSymbol("you-win");
			} else if(TicTacToe.isWin(board, !yourTurn)) {
				return Symbol.getSymbol("you-lose");
			}
			turn = !turn;
		}
		return Symbol.getSymbol("even");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d = Iterators.nextIf(itr);
		boolean b;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(d == null) {
			b = Math.random() >= 0.5;
		} else {
			b = d.isTrue();
		}
		return play(b, mesg);
	}

}
