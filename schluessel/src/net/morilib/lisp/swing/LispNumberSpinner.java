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
package net.morilib.lisp.swing;

import java.math.BigInteger;
import java.util.List;

import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/26
 */
public class LispNumberSpinner extends LispSpinner {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/26
	 */
	public static class MakeNumberSpinner extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			String fmt = null;

			if(l.size() < 4 || l.size() > 5) {
				throw mesg.getError("err.argument");
			} else if(!(l.get(0) instanceof LispReal)) {
				throw mesg.getError("err.require.real", l.get(0));
			} else if(!(l.get(1) instanceof LispReal)) {
				throw mesg.getError("err.require.real", l.get(1));
			} else if(!(l.get(2) instanceof LispReal)) {
				throw mesg.getError("err.require.real", l.get(2));
			} else if(!(l.get(3) instanceof LispReal)) {
				throw mesg.getError("err.require.real", l.get(3));
			} else if(l.size() == 4) {
				// go next
			} else {
				fmt = SubrUtils.getString(l.get(4), mesg);
			}

			//
			SpinnerNumberModel model;
			JSpinner spinner;
			JSpinner.NumberEditor editor;

			if(l.get(0) instanceof LispSmallInt &&
					issmallintorfalse(l.get(1)) &&
					issmallintorfalse(l.get(2)) &&
					l.get(3) instanceof LispSmallInt) {
				model = new SpinnerNumberModel(
						l.get(0).getInt(),
						smallintorfalse(l.get(1)),
						smallintorfalse(l.get(2)),
						l.get(3).getInt());
			} else {
				model = new SpinnerNumberModel(
						l.get(0).getRealDouble(),
						floatorfalse(l.get(1)),
						floatorfalse(l.get(2)),
						l.get(3).getRealDouble());
			}
			spinner = new JSpinner(model);
			if(fmt == null) {
				editor = new JSpinner.NumberEditor(spinner);
			} else {
				editor = new JSpinner.NumberEditor(spinner, fmt);
			}
			spinner.setEditor(editor);
			return new LispNumberSpinner(spinner);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/07/14
	 */
	public static class GetNumberFromSpinner extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispNumberSpinner) {
				LispNumberSpinner s = (LispNumberSpinner)c1a;
				SpinnerNumberModel m =
					(SpinnerNumberModel)s.spinner.getModel();
				Number n = m.getNumber();

				if(n == null) {
					return LispBoolean.FALSE;
				} else if(n instanceof BigInteger) {
					return LispInteger.valueOf((BigInteger)n);
				} else if(n instanceof Long) {
					return LispInteger.valueOf(n.longValue());
				} else if(n instanceof Integer ||
						n instanceof Short ||
						n instanceof Byte) {
					return LispInteger.valueOf(n.intValue());
				} else {
					return new LispDouble(n.doubleValue());
				}
			} else {
				throw mesg.getError("err.swing.require.spinner.number",
						c1a);
			}
		}

	}

	//
//	private SpinnerLispRealModel model;
//	private JSpinner.NumberEditor editor;

	//
	private LispNumberSpinner(JSpinner spinner) {
		super(spinner);
//		this.model  = model;
//		this.editor = editor;
	}

	//
	private static boolean issmallintorfalse(Datum d) {
		return (d instanceof LispSmallInt) || !d.isTrue();
	}

	//
	@SuppressWarnings("rawtypes")
	private static Comparable smallintorfalse(Datum d) {
		return d.isTrue() ? d.getInt() : null;
	}

	//
	@SuppressWarnings("rawtypes")
	private static Comparable floatorfalse(Datum d) {
		return d.isTrue() ? ((LispSmallInt)d).floatValue() : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<number-spinner>");
	}

}
