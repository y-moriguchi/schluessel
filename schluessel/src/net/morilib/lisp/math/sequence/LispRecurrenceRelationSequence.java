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
package net.morilib.lisp.math.sequence;

import java.util.Map;
import java.util.WeakHashMap;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/12
 */
public class LispRecurrenceRelationSequence
extends AbstractLispRealSequence {

	/**
	 * 
	 */
	public static final int INFINITE = -1;

	//
	private Procedure recur;
	private Environment env;
	private LispMessage mesg;
	private LispReal[] init;
	private int size;
	private transient Map<Integer, LispReal> memo =
		new WeakHashMap<Integer, LispReal>();

	/**
	 * 
	 * @param recur
	 * @param env
	 * @param mesg
	 * @param size
	 * @param reals
	 */
	public LispRecurrenceRelationSequence(Procedure recur,
			Environment env, LispMessage mesg, int size,
			LispReal... reals) {
		this.recur = recur;
		this.env   = env;
		this.mesg  = mesg;
		this.size  = size;
		this.init  = new LispReal[reals.length];
		System.arraycopy(reals, 0, init, 0, reals.length);
	}

	//
	private LispReal _get(int i) {
		if(i < 1) {
			throw new IndexOutOfBoundsException();
		} else if(i <= init.length) {
			return init[i - 1];
		} else {
			return memo.get(i);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#get(int)
	 */
	public LispReal get(int i) {
		LispReal[] args = new LispReal[init.length];
		LispReal r;
		Datum d;

		if(i > size) {
			return LispInteger.ZERO;
		} else if((r = _get(i)) == null) {
			for(int j = init.length + 1; j <= i; j++) {
				if((r = _get(j)) == null) {
					for(int k = 1; k <= args.length; k++) {
						args[k - 1] = _get(j - k);
					}
					d = Scheme.callva(recur, env, mesg, args);
					r = SubrUtils.getReal(d, mesg);
					memo.put(j, r);
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#limit()
	 */
	public LispReal limit() {
		return (size < 0) ? null : LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispSequence#isFinite()
	 */
	public boolean isFinite() {
		return size >= 1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispSequence#size()
	 */
	public int size() {
		return size;
	}

}
