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

import java.util.List;

import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispReal;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/11
 */
public class LispSimpleRealSequence extends AbstractLispRealSequence {

	//
	private List<LispReal> seq;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#get(int)
	 */
	public LispReal get(int i) {
		if(i < 1) {
			throw new IndexOutOfBoundsException();
		} else if(i > seq.size()) {
			return LispInteger.ZERO;
		} else {
			return seq.get(i - 1);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#limit()
	 */
	public LispReal limit() {
		return LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispSequence#isFinite()
	 */
	public boolean isFinite() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispSequence#size()
	 */
	public int size() {
		return seq.size();
	}

}
