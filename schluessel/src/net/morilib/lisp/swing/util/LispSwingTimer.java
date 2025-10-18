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
package net.morilib.lisp.swing.util;

import java.awt.event.ActionListener;

import javax.swing.Timer;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.swing.listener.ActionListenable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/24
 */
public class LispSwingTimer extends Datum2
implements ActionListenable {

	//
	Timer timer;

	/**
	 * 
	 * @param timer
	 */
	public LispSwingTimer(Timer timer) {
		this.timer = timer;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ActionListenable#addActionListener(java.awt.event.ActionListener)
	 */
	@Override
	public void addActionListener(ActionListener listener) {
		timer.addActionListener(listener);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<swing-timer>");
	}

}
