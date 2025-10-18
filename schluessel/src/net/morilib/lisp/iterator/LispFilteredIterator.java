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
package net.morilib.lisp.iterator;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/29
 */
public class LispFilteredIterator extends Datum2
implements ILispIterator {

	//
	private Procedure proc;
	private Environment env;
	private LispMessage mesg;
	private ILispIterator iter;

	/**
	 * 
	 * @param iter
	 * @param proc
	 * @param env
	 * @param mesg
	 */
	public LispFilteredIterator(ILispIterator iter, Procedure proc,
			Environment env, LispMessage mesg) {
		this.iter = iter;
		this.proc = proc;
		this.env  = env;
		this.mesg = mesg;
		_g();
	}

	//
	private void _g() {
		while(!iter.isTerminated() && !Scheme.callva(
				proc, env, mesg, iter.getCurrentDatum()).isTrue()) {
			iter = iter.next();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#isTerminated()
	 */
	public boolean isTerminated() {
		return iter.isTerminated();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#next()
	 */
	public ILispIterator next() {
		iter = iter.next();
		_g();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#getCurrentDatum()
	 */
	public Datum getCurrentDatum() {
		return iter.getCurrentDatum();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<filtered-iterator>");
	}

}
