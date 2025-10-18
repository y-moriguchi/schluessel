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
package net.morilib.lisp.compare;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/24
 */
public class DebugCompare extends UnaryArgs {

	//
	private static class Proc extends BinaryArgs {

		//
		private Datum proc;
		private Datum z = null;

		//
		private Proc(Datum proc) {
			this.proc = proc;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum x, Datum y, Environment env,
				LispMessage mesg) {
			int cxx, cyy, cxy, cyx;
			int cxz = 0, cyz = 0, czx = 0, czy = 0;

			cxx = SRFI67.callCompare(proc, x, x, env, mesg).getInt();
			cyy = SRFI67.callCompare(proc, y, y, env, mesg).getInt();
			cxy = SRFI67.callCompare(proc, x, y, env, mesg).getInt();
			cyx = SRFI67.callCompare(proc, y, x, env, mesg).getInt();
			if(z != null) {
				cxz = SRFI67.callCompare(proc, x, z, env,
						mesg).getInt();
				cyz = SRFI67.callCompare(proc, y, z, env,
						mesg).getInt();
				czx = SRFI67.callCompare(proc, z, x, env,
						mesg).getInt();
				czy = SRFI67.callCompare(proc, z, y, env,
						mesg).getInt();
			}

			if(cxx != 0) {
				throw mesg.getError("err.srfi67.notreflective", proc);
			} else if(cyy != 0) {
				throw mesg.getError("err.srfi67.notreflective", proc);
			} else if(!ANTISYMMETRY[cxy + 1][cyx + 1]) {
				throw mesg.getError("err.srfi67.notantisymmetric",
						proc);
			} else if(!ANTISYMMETRY[cxz + 1][czx + 1]) {
				throw mesg.getError("err.srfi67.notantisymmetric",
						proc);
			} else if(!ANTISYMMETRY[cyz + 1][czy + 1]) {
				throw mesg.getError("err.srfi67.notantisymmetric",
						proc);
			} else if(z != null &&
					TRANSITIVITY[cyz + 1][cxy + 1][cxz + 1] == FA) {
				throw mesg.getError("err.srfi67.nottransitive", proc);
			}
			z = ((int)(Math.random() * 1000) & 1) == 0 ? x : y;
			return LispInteger.valueOf(cxy);
		}

	}

	//
	private static final int TR = 1;
	private static final int FA = 0;
	private static final int UN = -1;

	//
	private static final boolean[][] ANTISYMMETRY = new boolean[][] {
		// -1    0      1            xy
		{ false, false, true  },  // yx=-1
		{ false, true,  false },  // yx=0
		{ true,  false, false }   // yx=1
	};

	//
	private static final int[] UNKNOWN = new int[] {
		UN, UN, UN
	};

	//
	private static final int[][][] TRANSITIVITY = new int[][][] {
		// -1           0             1                   xy
		// -1  0   1     -1  0   1     -1  0   1          xz
		{ {TR, FA, FA}, {TR, FA, FA}, UNKNOWN      },  // yz=-1
		{ {TR, FA, FA}, {FA, TR, FA}, {FA, FA, TR} },  // yz=0
		{ UNKNOWN,      {FA, FA, TR}, {FA, FA, TR} }   // yz=1
	};

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum cmp, Environment env,
			LispMessage mesg) {
		if(cmp instanceof Procedure) {
			return new Proc(cmp);
		} else {
			throw mesg.getError("err.require.procedure", cmp);
		}
	}

}
