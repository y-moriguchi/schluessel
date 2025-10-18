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
package net.morilib.lisp.r6rs.record;

import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/13
 */
public class RecordAccessor extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		int n = SubrUtils.getSmallInt(c2a, mesg);
		final RecordTypeDescriptor rtd;
		Set<String> fld;
		final String rn;

		if(c1a instanceof RecordTypeDescriptor) {
			rtd = (RecordTypeDescriptor)c1a;
			fld = rtd.getFieldNames();
			if(n < 0 || n >= fld.size()) {
				throw mesg.getError("err.r6rsrecord.field.outofrange",
						c2a);
			}
			rn = Iterators.get(fld, n);
			return new UnaryArgs("accessor " + rtd.getId()) {

				@Override
				protected Datum execute(Datum x, Environment env,
						LispMessage mesg) {
					if(!(x instanceof R6RSRecord)) {
						throw mesg.getError(
								"err.r6rsrecord.require.record", x);
					} else if(!rtd.equals(((R6RSRecord)x).getRtd())) {
						throw mesg.getError(
								"err.r6rsrecord.rtdmismatch");
					} else {
						return ((R6RSRecord)x).getField(rn);
					}
				}

			};
		} else {
			throw mesg.getError("err.r6rsrecord.require.rtd", c1a);
		}
	}

}
