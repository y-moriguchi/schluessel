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

import net.morilib.util.Builder;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class ConsListBuilder
implements Builder<ConsListBuilder, Datum> {

	//
	private SExpressionDatum res = null;
	private Cons  rp  = null;

	/**
	 * 
	 */
	public ConsListBuilder() {
		// default
	}

	//
	/*package*/ ConsListBuilder(ConsListBuilder src, LispMessage mesg) {
		if(src.res != null) {
			appendAll(src.res, mesg);
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public ConsListBuilder appendCons(Cons n) {
		if(res == null) {
			res = rp = n;
		} else {
			rp.setCdr(n);
			rp = n;
		}
		return this;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public ConsListBuilder append(Datum d) {
		return appendCons(new Cons(d, Nil.NIL));
	}

	//
	/*package*/ ConsListBuilder appendAll(Datum n, LispMessage mesg) {
		Datum p = n;

		while(true) {
			if(p instanceof Cons) {
				Cons c = (Cons)p;

				append(c.getCar());
				p = c.getCdr();
			} else if(p == Nil.NIL) {
				return this;
			} else {
				throw mesg.getError("err.list");
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public SExpressionDatum get() {
		return (res == null) ? Nil.NIL : res;
	}

	/**
	 * 
	 * @param cdr
	 * @return
	 */
	public Datum get(Datum cdr) {
		if(res != null) {
			rp.setCdr(cdr);
			return res;
		} else {
			return cdr;
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return LispUtils.print(get());
	}

}
