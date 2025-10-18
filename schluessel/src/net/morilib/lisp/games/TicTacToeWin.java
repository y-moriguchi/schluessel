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
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/09
 */
public class TicTacToeWin extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		int b;

		if(!(c1a instanceof LispTicTacToeBoard)) {
			throw mesg.getError("err.games.require.tictactoeboard",
					c1a);
		} else if(TicTacToe.isWin(
				b = ((LispTicTacToeBoard)c1a).board, true)) {
			return Symbol.getSymbol("o-win");
		} else if(TicTacToe.isWin(b, false)) {
			return Symbol.getSymbol("x-win");
		} else if(TicTacToe.isEven(b)) {
			return Symbol.getSymbol("even");
		} else {
			return LispBoolean.FALSE;
		}
	}

}
