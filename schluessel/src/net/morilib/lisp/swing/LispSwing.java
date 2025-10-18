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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.Keyword;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Parser;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.painter.SchlushColor;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.swing.event.LispActionEvent;
import net.morilib.lisp.swing.event.LispAdjustmentEvent;
import net.morilib.lisp.swing.event.LispChangeEvent;
import net.morilib.lisp.swing.event.LispItemEvent;
import net.morilib.lisp.swing.event.LispMouseEvent;
import net.morilib.lisp.swing.event.LispWindowEvent;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public final class LispSwing {

	//
	private static final Symbol VERTICAL =
			Symbol.getSymbol("vertical");
	private static final Symbol HORIZONTAL =
			Symbol.getSymbol("horizontal");
	private static final Symbol UNKNOWN = Symbol.getSymbol("unknown");

	//
	private static final OneToOneSet<Datum, Integer> _KEYS;
	private static final OneToOneSet<Datum, Integer>
	_MODS = new HashOneToOneSet<Datum, Integer>(new Object[][] {
			new Object[] {
					Symbol.getSymbol("shift"),
					InputEvent.SHIFT_MASK
			},
			new Object[] {
					Symbol.getSymbol("ctrl"),
					InputEvent.CTRL_MASK
			},
			new Object[] {
					Symbol.getSymbol("meta"),
					InputEvent.META_MASK
			},
			new Object[] {
					Symbol.getSymbol("alt"),
					InputEvent.ALT_MASK
			}
	});

	//
	static {
		InputStream ins = null;
		List<Datum> l;

		_KEYS = new HashOneToOneSet<Datum, Integer>();
		try {
			ConsIterator itr;

			ins = LispSwing.class.getResourceAsStream("keyboard.s");
			l   = Parser.readSExpression(
					new InputStreamReader(ins, "UTF-8"));
			itr = new ConsIterator(l.get(0));
			while(itr.hasNext()) {
				Cons c = (Cons)itr.next();

				_KEYS.put(c.getCar(), c.getCdr().getInt());
			}
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeException(e);
		} finally {
			if(ins != null) {
				try {
					ins.close();
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
			}
		}
	}

	//
	private LispSwing() {
		// do nothing
	}

	//
	/*package*/ static Selectable.Item[] toItem(Datum data) {
		ConsIterator it = new ConsIterator(data);
		List<Selectable.Item> l =
			new ArrayList<Selectable.Item>();

		while(it.hasNext()) {
			Datum d = it.next();

			if(d instanceof Cons) {
				l.add(new Selectable.Item(
						((Cons)d).getCdr(), ((Cons)d).getCar()));
			} else {
				l.add(new Selectable.Item(d));
			}
		}

		if(!it.getTerminal().equals(Nil.NIL)) {
			return null;
		}
		return l.toArray(new Selectable.Item[0]);
	}

	//
	/*package*/ static int toKeyEvent(Datum key, LispMessage mesg) {
		if(!_KEYS.containsKey(key)) {
			throw mesg.getError("err.swing.invalidkeytype", key);
		}
		return _KEYS.getValue(key);
	}

	//
	/*package*/ static KeyStroke toKeyStroke(
			Datum key, Datum mod, LispMessage mesg) {
		int m = 0;

		if(mod instanceof Cons) {
			ConsIterator itr = new ConsIterator(mod);

			while(itr.hasNext()) {
				Datum   d = itr.next();
				Integer i;

				if((i = _MODS.getValue(d)) == null) {
					throw mesg.getError(
							"err.swing.invalidkeymodifier", d);
				}
				m |= i;
			}

			if(!itr.getTerminal().isNil()) {
				throw mesg.getError("err.list", mod);
			}
		} else if(mod.isTrue()) {
			throw mesg.getError("err.swing.invalidkeymodifier", mod);
		} else if(!_KEYS.containsKey(key)) {
			throw mesg.getError("err.swing.invalidkeytype", key);
		}
		return KeyStroke.getKeyStroke(_KEYS.getValue(key), m);
	}

	/**
	 * 
	 * @param key
	 * @param mesg
	 * @return
	 */
	public static int toKeyCode(Datum key, LispMessage mesg) {
		if(!_KEYS.containsKey(key)) {
			throw mesg.getError("err.swing.invalidkeytype", key);
		}
		return _KEYS.getValue(key);
	}

	//
	private static Datum call(
			Datum p, Datum e,
			Scheme sch, Environment ev, LispMessage m) {
		if(p == null || !p.isTrue() || p.isNil()) {
			return Undef.UNDEF;
		} else if(p instanceof Subr) {
			return ((Subr)p).eval(LispUtils.list(e), ev, m);
		} else if(p instanceof Procedure) {
			return sch.call(p, e);
		} else {
			throw m.getError("err.require.procedure", p);
		}
	}

	/**
	 * 
	 * @param proc
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static ActionListener createActionListener(
			Datum proc, Environment env, LispMessage mesg) {
		final Datum p = proc;

		if(p instanceof Subr) {
			final Environment ev = env;
			final LispMessage m  = mesg;

			return new ActionListener() {

				public void actionPerformed(ActionEvent e) {
					((Subr)p).eval(
							LispUtils.list(new LispActionEvent(e)),
							ev, m);
				}
				
			};
		} else if(p instanceof Procedure) {
			final Scheme sch = new Scheme(env, mesg);

			return new ActionListener() {

				public void actionPerformed(ActionEvent e) {
					sch.call(p, new LispActionEvent(e));
				}

			};
		} else {
			throw mesg.getError("err.require.procedure", proc);
		}
	}

	/**
	 * 
	 * @param d
	 * @param body
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static MouseListener createMouseListener(
			Datum d, Datum body, Environment env, LispMessage mesg) {
		final Map<Keyword, Datum> mp =
			SubrUtils.keywordArgsToMap(d, body, mesg);
		final Environment ev = env;
		final LispMessage m  = mesg;
		final Scheme sch = new Scheme(env, mesg);

		return new MouseListener() {

			public void mouseClicked(MouseEvent e) {
				call(mp.get(Keyword.getKeyword("clicked")),
						new LispMouseEvent(e), sch, ev, m);
			}

			public void mousePressed(MouseEvent e) {
				call(mp.get(Keyword.getKeyword("pressed")),
						new LispMouseEvent(e), sch, ev, m);
			}

			public void mouseReleased(MouseEvent e) {
				call(mp.get(Keyword.getKeyword("released")),
						new LispMouseEvent(e), sch, ev, m);
			}

			public void mouseEntered(MouseEvent e) {
				call(mp.get(Keyword.getKeyword("entered")),
						new LispMouseEvent(e), sch, ev, m);
			}

			public void mouseExited(MouseEvent e) {
				call(mp.get(Keyword.getKeyword("exited")),
						new LispMouseEvent(e), sch, ev, m);
			}
			
		};
	}

	/**
	 * 
	 * @param d
	 * @param body
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static MouseMotionListener createMouseMotionListener(
			Datum d, Datum body, Environment env, LispMessage mesg) {
		final Map<Keyword, Datum> mp =
			SubrUtils.keywordArgsToMap(d, body, mesg);
		final Environment ev = env;
		final LispMessage m  = mesg;
		final Scheme sch = new Scheme(env, mesg);

		return new MouseMotionListener() {

			@Override
			public void mouseDragged(MouseEvent e) {
				call(mp.get(Keyword.getKeyword("dragged")),
						new LispMouseEvent(e), sch, ev, m);
			}

			@Override
			public void mouseMoved(MouseEvent e) {
				call(mp.get(Keyword.getKeyword("moved")),
						new LispMouseEvent(e), sch, ev, m);
			}
			
		};
	}

	/**
	 * 
	 * @param proc
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static ItemListener createItemListener(
			Datum proc, Environment env, LispMessage mesg) {
		final Datum p = proc;
		final Environment ev = env;
		final LispMessage m  = mesg;
		final Scheme sch = new Scheme(env, mesg);

		return new ItemListener() {

			public void itemStateChanged(ItemEvent e) {
				call(p, new LispItemEvent(e), sch, ev, m);
			}

		};
	}

	/**
	 * 
	 * @param proc
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static AdjustmentListener createAdjustmentListener(
			Datum proc, Environment env, LispMessage mesg) {
		final Datum p = proc;
		final Environment ev = env;
		final LispMessage m  = mesg;
		final Scheme sch = new Scheme(env, mesg);

		return new AdjustmentListener() {

			public void adjustmentValueChanged(AdjustmentEvent e) {
				call(p, new LispAdjustmentEvent(e), sch, ev, m);
			}

		};
	}

	/**
	 * 
	 * @param proc
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static ChangeListener createChangeListener(
			Datum proc, Environment env, LispMessage mesg) {
		final Datum p = proc;
		final Environment ev = env;
		final LispMessage m  = mesg;
		final Scheme sch = new Scheme(env, mesg);

		return new ChangeListener() {

			public void stateChanged(ChangeEvent e) {
				call(p, new LispChangeEvent(e), sch, ev, m);
			}

		};
	}

	//
	/*package*/ static Action createAction(
			Datum d, Datum body, Environment env, LispMessage mesg) {
		final Map<Keyword, Datum> mp =
			SubrUtils.keywordArgsToMap(d, body, mesg);
		final Environment ev = env;
		final LispMessage m  = mesg;
		final Scheme sch = new Scheme(env, mesg);
		Action res = new AbstractAction() {

			//
			private static final long
			serialVersionUID = -354094442585077659L;

			public void actionPerformed(ActionEvent e) {
				call(mp.get(Keyword.getKeyword("action")),
						new LispActionEvent(e), sch, ev, m);
			}

		};

		for(Map.Entry<Keyword, Datum> e : mp.entrySet()) {
			String k = e.getKey().getName();
			List<Datum> l;

			if(k.equals("accelerator-key")) {
				l = LispUtils.consToList(e.getValue(), mesg);
				if(l.size() != 2) {
					throw mesg.getError(
							"err.swing.invalidkeystroke",
							e.getValue());
				}
				res.putValue(Action.ACCELERATOR_KEY,
						toKeyStroke(l.get(0), l.get(1), mesg));
			} else if(k.equals("long-description")) {
				res.putValue(Action.LONG_DESCRIPTION,
						SubrUtils.getString(e.getValue(), mesg));
			} else if(k.equals("mnemonic-key")) {
				res.putValue(Action.MNEMONIC_KEY,
						toKeyEvent(e.getValue(), mesg));
			} else if(k.equals("name")) {
				res.putValue(Action.NAME,
						SubrUtils.getString(e.getValue(), mesg));
			} else if(k.equals("short-description")) {
				res.putValue(Action.SHORT_DESCRIPTION,
						SubrUtils.getString(e.getValue(), mesg));
			} else if(k.equals("small-icon")) {
				if(e.getValue() instanceof ILispIcon) {
					res.putValue(Action.SMALL_ICON,
							((ILispIcon)e.getValue()).getIcon());
				} else {
					throw mesg.getError(
							"err.swing.require.icon",
							e.getValue());
				}
			}
		}
		return res;
	}

	/**
	 * 
	 * @param d
	 * @param body
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static WindowListener createWindowListener(
			Datum d, Datum body, Environment env, LispMessage mesg) {
		final Map<Keyword, Datum> mp =
			SubrUtils.keywordArgsToMap(d, body, mesg);
		final Environment ev = env;
		final LispMessage m  = mesg;
		final Scheme sch = new Scheme(env, mesg);

		return new WindowListener() {

			public void windowOpened(WindowEvent e) {
				call(mp.get(Keyword.getKeyword("opened")),
						new LispWindowEvent(e), sch, ev, m);
			}

			public void windowClosing(WindowEvent e) {
				call(mp.get(Keyword.getKeyword("closing")),
						new LispWindowEvent(e), sch, ev, m);
			}

			public void windowClosed(WindowEvent e) {
				call(mp.get(Keyword.getKeyword("closed")),
						new LispWindowEvent(e), sch, ev, m);
			}

			public void windowIconified(WindowEvent e) {
				call(mp.get(Keyword.getKeyword("iconified")),
						new LispWindowEvent(e), sch, ev, m);
			}

			public void windowDeiconified(WindowEvent e) {
				call(mp.get(Keyword.getKeyword("deiconified")),
						new LispWindowEvent(e), sch, ev, m);
			}

			public void windowActivated(WindowEvent e) {
				call(mp.get(Keyword.getKeyword("activated")),
						new LispWindowEvent(e), sch, ev, m);
			}

			public void windowDeactivated(WindowEvent e) {
				call(mp.get(Keyword.getKeyword("deactivated")),
						new LispWindowEvent(e), sch, ev, m);
			}
			
		};
	}

	//
	/*package*/ static AttributeSet createAttributeSet(
			Datum d, Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(d);
		SimpleAttributeSet a = new SimpleAttributeSet();

		while(itr.hasNext()) {
			String k = SubrUtils.nextKeywordName(itr, mesg, body);

			if(k.equals("font-family")) {
				String s = SubrUtils.nextString(itr, mesg, body);

				StyleConstants.setFontFamily(a, s);
			} else if(k.equals("font-size")) {
				int s = SubrUtils.nextSmallInt(itr, mesg, body);

				StyleConstants.setFontSize(a, s);
			} else if(k.equals("bold")) {
				StyleConstants.setBold(a, true);
			} else if(k.equals("italic")) {
				StyleConstants.setItalic(a, true);
			} else if(k.equals("underline")) {
				StyleConstants.setUnderline(a, true);
			} else if(k.equals("strike")) {
				StyleConstants.setStrikeThrough(a, true);
			} else if(k.equals("superscript")) {
				StyleConstants.setSuperscript(a, true);
			} else if(k.equals("subscript")) {
				StyleConstants.setSubscript(a, true);
			} else if(k.equals("color")) {
				Datum c = SubrUtils.nextIf(itr, mesg, body);

				if(c instanceof SchlushColor) {
					StyleConstants.setForeground(
							a, ((SchlushColor)c).getColor());
				} else {
					throw mesg.getError("err.require.color", c);
				}
			} else if(k.equals("background-color")) {
				Datum c = SubrUtils.nextIf(itr, mesg, body);

				if(c instanceof SchlushColor) {
					StyleConstants.setBackground(
							a, ((SchlushColor)c).getColor());
				} else {
					throw mesg.getError("err.require.color", c);
				}
			}
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return a;
	}

	/**
	 * 
	 * @param d1
	 * @param mesg
	 * @return
	 */
	public static int getOrientation(Datum d1, LispMessage mesg) {
		if(d1.equals(VERTICAL)) {
			return SwingConstants.VERTICAL;
		} else if(d1.equals(HORIZONTAL)) {
			return SwingConstants.HORIZONTAL;
		} else {
			throw mesg.getError("err.swing.orientation.invalid", d1);
		}
	}

	
	public static Symbol orientationSymbol(int orient) {
		switch(orient) {
		case SwingConstants.VERTICAL:    return VERTICAL;
		case SwingConstants.HORIZONTAL:  return HORIZONTAL;
		default:  return UNKNOWN;
		}
	}

}
