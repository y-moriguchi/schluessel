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
package net.morilib.lisp.exlib;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/09
 */
public class LispStringBuilder extends Datum2 {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/09
	 */
	public static class MakeStringBuilder extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			if(body.isNil()) {
				return new LispStringBuilder();
			} else {
				throw mesg.getError("err.argument", body);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/09
	 */
	public static class AppendBuilderS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d0;

			if(!itr.hasNext()) {
				throw mesg.getError("err.argument", body);
			} else if(!((d0 = itr.next())
					instanceof LispStringBuilder)) {
				throw mesg.getError("err.require.stringbuilder", d0);
			}

			while(itr.hasNext()) {
				String s = SubrUtils.getString(itr.next(), mesg);

				((LispStringBuilder)d0).builder.append(s);
			}

			if(!itr.getTerminal().isNil()) {
				throw mesg.getError("err.list", body);
			}
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/09
	 */
	public static class AppendCharBuilderS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d0;

			if(!itr.hasNext()) {
				throw mesg.getError("err.argument", body);
			} else if(!((d0 = itr.next())
					instanceof LispStringBuilder)) {
				throw mesg.getError("err.require.stringbuilder", d0);
			}

			while(itr.hasNext()) {
				char c = SubrUtils.getCharacter(itr.next(), mesg);

				((LispStringBuilder)d0).builder.append(c);
			}

			if(!itr.getTerminal().isNil()) {
				throw mesg.getError("err.list", body);
			}
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/12
	 */
	public static class BuilderToString extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispStringBuilder) {
				return new LispString(
						((LispStringBuilder)c1a).builder.toString());
			} else {
				throw mesg.getError("err.require.stringbuilder", c1a);
			}
		}

	}

	//
	private StringBuilder builder;

	/**
	 * 
	 */
	public LispStringBuilder() {
		builder = new StringBuilder();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<string-builder ").append(builder).append(">");
	}

}
