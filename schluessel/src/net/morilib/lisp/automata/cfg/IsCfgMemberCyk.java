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
package net.morilib.lisp.automata.cfg;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/12
 */
public class IsCfgMemberCyk extends Subr {

	//
	private static boolean cont(Set<?> s, Object o) {
		return s != null && s.contains(o);
	}

	/**
	 * 
	 * @param cfg
	 * @param d
	 * @return
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static boolean isMemberByCYK(LispCFG cfg, Datum... d) {
		LispCFG cnf = cfg.toCNF();
		Set[][] tbl = new Set[d.length][d.length];
		List<Datum> l;

		if(d.length == 0)  return cfg.isNullable();
		outer: for(int i = 0; i < d.length; i++) {
			for(LispCFGRule x : cnf.getRules()) {
				l = x.getRightValues();
				if(l.size() == 1 && d[i].equals(l.get(0))) {
					tbl[i][0] =
							Collections.singleton(x.getLeftValue());
					continue outer;
				}
			}
			return false;
		}

		for(int i = 1; i < d.length; i++) {
			for(int j = 0; j < d.length - i; j++) {
				for(int k = 0; k < i; k++) {
					for(LispCFGRule x : cnf.getRules()) {
						l = x.getRightValues();
						if(l.size() == 2 &&
								cont(tbl[j][k], l.get(0)) &&
								cont(tbl[j + k + 1][i - k - 1], l.get(1))) {
							if(tbl[j][i] == null) {
								tbl[j][i] = new HashSet();
							}
							tbl[j][i].add(x.getLeftValue());
						}
					}
				}
			}
		}
		return (tbl[0][d.length - 1] != null &&
				tbl[0][d.length - 1].contains(cnf.getStartVariable()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c = SubrUtils.nextIf(itr, mesg, body);
		List<Datum> l = new ArrayList<Datum>();

		while(itr.hasNext())  l.add(itr.next());
		SubrUtils.checkTerminated(itr, body, mesg);
		if(c instanceof LispCFG) {
			return LispBoolean.getInstance(isMemberByCYK(
					(LispCFG)c, l.toArray(new Datum[0])));
		} else {
			throw mesg.getError("err.automata.require.cfg", c);
		}
	}

}
