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
package net.morilib.lisp.awt.tray;

import java.awt.Image;
import java.awt.Menu;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.TrayIcon;
import java.io.IOException;
import java.util.List;

import javax.imageio.ImageIO;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.painter.ILispImage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.swing.LispSwing;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/01
 */
public class MakeTrayIcon extends Subr {

	//
	/*package*/ static MenuItem parseMenu(
			Datum d, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(d, mesg);

		if(l.size() == 2) {
			String t = SubrUtils.getString(l.get(0), mesg);
			ConsIterator itr;

			if(l.get(1) instanceof Procedure) {
				MenuItem itm = new MenuItem(t);

				itm.addActionListener(LispSwing.createActionListener(
						l.get(1), env, mesg));
				return itm;
			} else if(l.get(1) instanceof Cons) {
				Menu men = new Menu(t);

				itr = new ConsIterator(l.get(1));
				while(itr.hasNext()) {
					Datum z = itr.next();

					if(z.equals(Symbol.getSymbol("separator"))) {
						men.addSeparator();
					} else {
						men.add(parseMenu(z, env, mesg));
					}
				}

				if(!itr.getTerminal().isNil()) {
					throw mesg.getError("err.list", l.get(1));
				}
				return men;
			} else if(!l.get(1).isTrue()) {
				return new MenuItem(t);
			} else {
				throw mesg.getError("err.awt.invalidmenu", d);
			}
		} else {
			throw mesg.getError("err.awt.invalidmenu", d);
		}
	}

	//
	/*package*/ static PopupMenu parsePopupMenu(
			Datum d, Environment env, LispMessage mesg) {
		PopupMenu    mb  = new PopupMenu();
		ConsIterator itr = new ConsIterator(d);

		while(itr.hasNext()) {
			mb.add(parseMenu(itr.next(), env, mesg));
		}
		SubrUtils.checkProper(itr, d, mesg);
		return mb;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum c2a = SubrUtils.nextIf(itr, mesg, body);
		Datum c3a = SubrUtils.nextIf(itr, mesg, body);
		Datum c4a = Iterators.nextIf(itr);
		Datum c5a = Iterators.nextIf(itr);
		Datum c6a = Iterators.nextIf(itr);
		PopupMenu men;
		TrayIcon ico;
		String tt;
		Image img;

		SubrUtils.checkTerminated(itr, body, mesg);
		try {
			if(c1a instanceof LispString) {
				img = ImageIO.read(LispFiles.getFile(env, c1a, mesg));
			} else if(c1a instanceof ILispImage) {
				img = ((ILispImage)c1a).getImage();
			} else {
				throw mesg.getError("err.awt.require.image", c1a);
			}
			tt  = c2a.isTrue() ? SubrUtils.getString(c2a, mesg) : null;
			men = c3a.isTrue() ? parsePopupMenu(c3a, env, mesg) : null;

			ico = new TrayIcon(img, tt, men);
			if(c4a != null) {
				ico.addActionListener(LispSwing.createActionListener(
						c4a, env, mesg));
			}

			if(c5a != null) {
				ico.addMouseListener(LispSwing.createMouseListener(
						c5a, body, env, mesg));
			}

			if(c6a != null) {
				ico.addMouseMotionListener(
						LispSwing.createMouseMotionListener(
								c6a, body, env, mesg));
			}
			ico.setImageAutoSize(true);
			return new LispTrayIcon(ico);
		} catch (IOException e) {
			throw mesg.getError("err.io");
		}
	}

}
