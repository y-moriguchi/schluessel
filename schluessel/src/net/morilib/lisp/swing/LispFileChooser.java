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

import java.io.File;

import javax.swing.JFileChooser;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;
import net.morilib.util.swing.ExtensionFileFilter;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/19
 */
public class LispFileChooser extends Datum2 {

	//
	private static final OneToOneSet<Datum, Integer>
	_MODES = new HashOneToOneSet<Datum, Integer>(new Object[][] {
			new Object[] {
					Symbol.getSymbol("files"),
					JFileChooser.FILES_ONLY
			},
			new Object[] {
					Symbol.getSymbol("directories"),
					JFileChooser.DIRECTORIES_ONLY
			},
			new Object[] {
					Symbol.getSymbol("files-and-directories"),
					JFileChooser.FILES_AND_DIRECTORIES
			},
	});

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class MakeFileChooser extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(c1a);
			JFileChooser fc  = new JFileChooser();

			while(itr.hasNext()) {
				Datum d = itr.next();

				if(d instanceof Cons) {
					Cons c = (Cons)d;

					if(!(c.getCar() instanceof LispString)) {
						throw mesg.getError(
								"err.require.string", c.getCar());
					} else if(!(c.getCdr() instanceof LispString)) {
						throw mesg.getError(
								"err.require.string", c.getCdr());
					}
					fc.addChoosableFileFilter(new ExtensionFileFilter(
							c.getCar().getString(),
							c.getCdr().getString()));
				}
			}

			if(!itr.getTerminal().isNil()) {
				throw mesg.getError("err.list", c1a);
			}
			return new LispFileChooser(fc);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class SetFileSelectionModeS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispFileChooser) {
				Integer i = _MODES.getValue(c2a);

				if(i == null) {
					throw mesg.getError(
							"err.swing.invalidfileselectionmode", c2a);
				}
				((LispFileChooser)c1a)
				.filechooser.setFileSelectionMode(i);
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.swing.require.filechooser", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class SetAcceptAllFileFilterUsedS
	extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispFileChooser) {
				((LispFileChooser)c1a).filechooser
				.setAcceptAllFileFilterUsed(c2a.isTrue());
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.swing.require.filechooser", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class ShowOpenFileChooser extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispFileChooser) {
				JFileChooser fc;

				fc = ((LispFileChooser)c1a).filechooser;
				if(c2a instanceof GUIElement) {
					return _result(fc, fc.showOpenDialog(
							((GUIElement)c2a).getAWTComponent()));
				} else if(!c2a.isTrue()) {
					return _result(fc, fc.showOpenDialog(null));
				} else {
					throw mesg.getError(
							"err.swing.require.guielement", c2a);
				}
			} else {
				throw mesg.getError(
						"err.swing.require.filechooser", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class ShowSaveFileChooser extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispFileChooser) {
				JFileChooser fc;

				fc = ((LispFileChooser)c1a).filechooser;
				if(c2a instanceof GUIElement) {
					return _result(fc, fc.showSaveDialog(
							((GUIElement)c2a).getAWTComponent()));
				} else if(!c2a.isTrue()) {
					return _result(fc, fc.showSaveDialog(null));
				} else {
					throw mesg.getError(
							"err.swing.require.guielement", c2a);
				}
			} else {
				throw mesg.getError(
						"err.swing.require.filechooser", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class ShowFileChooser extends TernaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Datum c3a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispFileChooser) {
				JFileChooser fc;
				String t = SubrUtils.getString(c3a, mesg);

				fc = ((LispFileChooser)c1a).filechooser;
				if(c2a instanceof GUIElement) {
					return _result(fc, fc.showDialog(
							((GUIElement)c2a).getAWTComponent(), t));
				} else if(!c2a.isTrue()) {
					return _result(fc, fc.showDialog(null, t));
				} else {
					throw mesg.getError(
							"err.swing.require.guielement", c2a);
				}
			} else {
				throw mesg.getError(
						"err.swing.require.filechooser", c1a);
			}
		}

	}

	//
	private JFileChooser filechooser;

	/**
	 * 
	 * @param chooser
	 */
	public LispFileChooser(JFileChooser chooser) {
		this.filechooser = chooser;
	}

	//
	private static Datum _result(JFileChooser fc, int r) {
		switch(r) {
		case JFileChooser.APPROVE_OPTION:
			if(fc.isMultiSelectionEnabled()) {
				File[] fs = fc.getSelectedFiles();
				ConsListBuilder b = new ConsListBuilder();

				for(int i = 0; i < fs.length; i++) {
					b.append(new LispString(fs[i].getPath()));
				}
				return b.get();
			} else {
				return new LispString(fc.getSelectedFile().getPath());
			}
		case JFileChooser.CANCEL_OPTION:
			return LispBoolean.FALSE;
		default:
			throw new RuntimeException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<file-chooser>");
	}

}
