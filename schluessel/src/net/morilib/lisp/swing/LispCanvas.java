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

import java.awt.Component;
import java.awt.Image;
import java.awt.MediaTracker;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JPanel;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.painter.SchlushPanelFrame;
import net.morilib.lisp.painter.drawer.CoordinateMap;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public class LispCanvas extends SchlushPanelFrame
implements ILispComponent {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class MakeCanvas extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);

			if(l.size() == 0) {
				return new LispCanvas();
			} else if(l.size() == 4) {
				double x1 = l.get(0).getRealDouble();
				double y1 = l.get(1).getRealDouble();
				double x2 = l.get(2).getRealDouble();
				double y2 = l.get(3).getRealDouble();

				return new LispCanvas(
						new CoordinateMap(x1, y1, x2, y2));
			} else {
				throw mesg.getError("err.argument", body);
			}
		}

	}

	//
	private JPanel panel;

	//
	private LispCanvas() {
		panel = super.createPanel();
	}

	//
	private LispCanvas(CoordinateMap coordinate) {
		super(coordinate);
		panel = super.createPanel();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushJPanelFrame#createPanel()
	 */
	@Override
	public JPanel createPanel() {
		throw new RuntimeException("panels cannot be created");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getImageWidth(java.awt.Image)
	 */
	public int getImageWidth(Image img) {
		return img.getWidth(panel);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getImageHeight(java.awt.Image)
	 */
	public int getImageHeight(Image img) {
		return img.getHeight(panel);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getHeight()
	 */
	public int getHeight() {
		return panel.getHeight();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getWidth()
	 */
	public int getWidth() {
		return panel.getWidth();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#repaint()
	 */
	public void repaint() {
		panel.repaint();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JComponent getComponent() {
		return panel;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushJPanelFrame#loadImage(java.awt.Image)
	 */
	@Override
	protected void loadImage(Image img) throws InterruptedException {
		MediaTracker mt = new MediaTracker(panel);

		mt.addImage(img, 1);
		mt.waitForAll();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return getComponent();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<canvas>");
	}

}
