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
package net.morilib.lisp.sos;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/16
 */
public interface ISlotDatum {

	/**
	 * 
	 * @param sym
	 * @return
	 */
	public Datum getSlot(Symbol sym);

	/**
	 * 
	 * @param sym
	 * @param val
	 * @return
	 */
	public boolean setSlot(
			Symbol sym, Datum val, LispMessage mesg);

}
