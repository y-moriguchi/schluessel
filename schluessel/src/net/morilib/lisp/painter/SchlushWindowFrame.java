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
package net.morilib.lisp.painter;

import java.awt.Component;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JFrame;

import net.morilib.lisp.painter.drawer.CoordinateMap;
import net.morilib.lisp.painter.drawer.Drawer;
import net.morilib.lisp.swing.ILispWindow;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/12
 */
public class SchlushWindowFrame extends SchlushPanelFrame
implements ILispWindow {

	//
	/*package*/ JFrame frame;
	private boolean disposed = false;

	/**
	 * 
	 * @param x
	 * @param y
	 * @param coordinate
	 */
	public SchlushWindowFrame(
			int x, int y, CoordinateMap coordinate) {
		super(coordinate);
		frame = new JFrame();
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		frame.setSize(x, y);
		frame.add(createPanel());
		frame.setVisible(true);
		frame.addWindowListener(new WindowListener() {

			public void windowOpened(WindowEvent e) {
			}

			public void windowClosing(WindowEvent e) {
			}

			public void windowClosed(WindowEvent e) {
				disposed = true;
			}

			public void windowIconified(WindowEvent e) {
			}

			public void windowDeiconified(WindowEvent e) {
			}

			public void windowActivated(WindowEvent e) {
			}

			public void windowDeactivated(WindowEvent e) {
			}

		});
	}

	/**
	 * 
	 * @param x
	 * @param y
	 */
	public SchlushWindowFrame(int x, int y) {
		this(x, y, null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushJPanelFrame#loadImage(java.awt.Image)
	 */
	protected void loadImage(Image img) throws InterruptedException {
		MediaTracker mt = new MediaTracker(frame);

		mt.addImage(img, 1);
		mt.waitForAll();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getHeight()
	 */
	public int getHeight() {
		return frame.getHeight();
	}


	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getWidth()
	 */
	public int getWidth() {
		return frame.getWidth();
	}


	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getImageWidth(java.awt.Image)
	 */
	public int getImageWidth(Image img) {
		return img.getWidth(frame);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getImageHeight(java.awt.Image)
	 */
	public int getImageHeight(Image img) {
		return img.getHeight(frame);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#addImage(net.morilib.lisp.painter.SchlushFrame.ImageInfo)
	 */
	public void addImage(ImageInfo img) throws InterruptedException {
		if(!disposed) {
			super.addImage(img);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#addDrawer(net.morilib.lisp.painter.drawer.Drawer)
	 */
	public void addDrawer(Drawer drawer) {
		if(!disposed) {
			super.addDrawer(drawer);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#clearScreen()
	 */
	@Override
	public void clearScreen() {
		if(!disposed) {
			super.clearScreen();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#repaint()
	 */
	public void repaint() {
		frame.repaint();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return frame;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.WindowListenable#addWindowListener(java.awt.event.WindowListener)
	 */
	@Override
	public void addWindowListener(WindowListener l) {
		frame.addWindowListener(l);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.WindowListenable#getAWTWindow()
	 */
	@Override
	public Window getAWTWindow() {
		return frame;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispWindow#getFrame()
	 */
	@Override
	public JFrame getFrame() {
		return frame;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<window-frame>");
	}

}
