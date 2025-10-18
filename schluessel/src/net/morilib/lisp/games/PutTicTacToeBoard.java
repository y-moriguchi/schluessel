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
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/09
 */
public class PutTicTacToeBoard extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		int x = SubrUtils.getSmallInt(c2a, mesg);
		int y = SubrUtils.getSmallInt(c3a, mesg);
		int b;

		if(!(c1a instanceof LispTicTacToeBoard)) {
			throw mesg.getError("err.games.require.tictactoeboard",
					c1a);
		} else if(x < 0 || x > 2) {
			throw mesg.getError("err.games.board.outofrange", c2a);
		} else if(y < 0 || y > 2) {
			throw mesg.getError("err.games.board.outofrange", c3a);
		} else if((b = TicTacToe.put(
				((LispTicTacToeBoard)c1a).board, x, y)) >= 0) {
			return new LispTicTacToeBoard(b);
		} else {
			throw mesg.getError("err.games.board.put");
		}
	}

}
