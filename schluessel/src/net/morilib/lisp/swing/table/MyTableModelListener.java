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
package net.morilib.lisp.swing.table;

import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Symbol;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/18
 */
public class MyTableModelListener implements TableModelListener {

	//
	static final Symbol INSERT = Symbol.getSymbol("insert");
	static final Symbol UPDATE = Symbol.getSymbol("update");
	static final Symbol DELETE = Symbol.getSymbol("delete");

	//
	private Environment env;
	private Procedure proc;
	private LispMessage mesg;

	/**
	 * 
	 * @param env
	 * @param proc
	 * @param mesg
	 */
	public MyTableModelListener(Environment env, Procedure proc,
			LispMessage mesg) {
		this.env = env;
		this.proc = proc;
		this.mesg = mesg;
	}

	/* (non-Javadoc)
	 * @see javax.swing.event.TableModelListener#tableChanged(javax.swing.event.TableModelEvent)
	 */
	@Override
	public void tableChanged(TableModelEvent e) {
		Datum bg, ed, cl;

		if(e.getFirstRow() == TableModelEvent.HEADER_ROW) {
			Scheme.callva(proc, env, mesg,
					LispBoolean.FALSE,
					LispBoolean.FALSE,
					LispBoolean.FALSE,
					LispBoolean.FALSE);
		} else {
			bg = LispInteger.valueOf(e.getFirstRow());
			ed = LispInteger.valueOf(e.getLastRow());
			cl = (e.getColumn() == TableModelEvent.ALL_COLUMNS) ?
					LispBoolean.FALSE :
						LispInteger.valueOf(e.getColumn());
			switch(e.getType()) {
			case TableModelEvent.UPDATE:
				Scheme.callva(proc, env, mesg, bg, ed, cl, UPDATE);
				return;
			case TableModelEvent.INSERT:
				Scheme.callva(proc, env, mesg, bg, ed, cl, INSERT);
				return;
			case TableModelEvent.DELETE:
				Scheme.callva(proc, env, mesg, bg, ed, cl, DELETE);
				return;
			}
		}
	}

}
