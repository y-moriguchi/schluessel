/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp;

import java.util.List;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class SubrReadBase extends Subr {

	/**
	 * 
	 * @param inp
	 * @return
	 */
	protected abstract Datum action(InputPort inp);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		InputPort inp;

		if(lst.size() > 1) {
			throw mesg.getError("err.argument", symbolName);
		}

		if(lst.size() == 1) {
			if(lst.get(0) instanceof InputPort) {
				inp = (InputPort)lst.get(0);
			} else {
				throw mesg.getError("err.require.iport", lst.get(0));
			}
		} else {
			inp = InputPort.getStandard(mesg);
		}
		return action(inp);
	}

}
