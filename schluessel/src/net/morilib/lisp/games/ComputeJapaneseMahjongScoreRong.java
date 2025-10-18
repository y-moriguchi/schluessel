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

import net.morilib.games.JapaneseMahjongUtils;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/08
 */
public class ComputeJapaneseMahjongScoreRong extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		int fu  = SubrUtils.nextNonnegativeSmallInt(itr, mesg, body);
		int fan = SubrUtils.nextNonnegativeSmallInt(itr, mesg, body);
		Datum oya = Iterators.nextIf(itr, LispBoolean.FALSE);
		Datum unl = Iterators.nextIf(itr, LispBoolean.FALSE);
		int flags = 0;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(oya.isTrue())  flags |= 1;
		if(unl.isTrue())  flags |= 2;
		return LispInteger.valueOf(
				JapaneseMahjongUtils.scoreRong(fu, fan, flags));
	}

}
