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

import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

public class ExSubrSetter extends UnaryArgs {
	
	private static class Setter extends BinaryArgs {
		{
			symbolName = "setter$setter";
		}
		
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof Settable)) {
				throw mesg.getError(
						"err.srfi17.require.settable", c1a);
			} else if(!(c2a instanceof Procedure)) {
				throw mesg.getError(
						"err.srfi17.require.procedure", c2a);
			}
			
			((Settable)c1a).setSetter(c2a);
			return Undef.UNDEF;
		}
	}
	
	private static final Setter INSTANCE = new Setter();
	
	
	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		if(!(c1a instanceof Settable)) {
			throw mesg.getError(
					"err.srfi17.require.settable", c1a);
		}
		
		return ((Settable)c1a).getSetter();
	}
	
	
	public Datum getSetter() {
		Datum ss = super.getSetter();
		
		return (ss == Undef.UNDEF) ? INSTANCE : ss;
	}
	
}
