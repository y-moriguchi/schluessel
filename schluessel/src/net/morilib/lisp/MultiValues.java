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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import net.morilib.lisp.accessor.ILispRef;
import net.morilib.lisp.subr.SubrUtils;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class MultiValues extends Datum implements ILispRef {

	//
	private List<? extends Datum> values;

	//
	private MultiValues(List<? extends Datum> values) {
		if(values == null) {
			throw new NullPointerException();
		}
		this.values = values;
	}

	/**
	 * 
	 * @param values
	 * @return
	 */
	public static Datum newValues(List<? extends Datum> values) {
		if(values.size() == 0) {
			return Undef.UNDEF;
		} else if(values.size() == 1) {
			return values.get(0);
		} else {
			return new MultiValues(values);
		}
	}

	/**
	 * 
	 * @param values
	 * @return
	 */
	public static Datum newValues(Datum... values) {
		if(values.length == 0) {
			return Undef.UNDEF;
		} else if(values.length == 1) {
			return values[0];
		} else {
			return new MultiValues(Arrays.asList(values));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTrue()
	 */
	@Override
	public boolean isTrue() {
		return values.isEmpty() || values.get(0).isTrue();
	}

	/**
	 * 
	 * @return
	 */
	public List<Datum> getValues() {
		return Collections.unmodifiableList(values);
	}

	/**
	 * 
	 * @return
	 */
	public int howManyValues() {
		return values.size();
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Datum getValueOf(int x) {
		if(x >= 0 && x < values.size()) {
			return values.get(x);
		} else {
			throw new IndexOutOfBoundsException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum ref(Datum arg, LispMessage mesg) {
		int k = SubrUtils.getSmallInt(arg, mesg);

		if(k < 0 || k >= values.size()) {
			throw mesg.getError("err.accessor.ref.outofrange", arg);
		}
		return values.get(k);
	}

}
