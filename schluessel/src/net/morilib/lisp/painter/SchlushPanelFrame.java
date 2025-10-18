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

import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.font.TextLayout;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JPanel;

import net.morilib.lisp.painter.drawer.CoordinateMap;
import net.morilib.lisp.painter.drawer.Drawer;
import net.morilib.lisp.painter.geom.MutablePoint2D;
import net.morilib.lisp.swing.GUIElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/12
 */
public abstract class SchlushPanelFrame
extends GUIElement implements SchlushFrame {

	//
	private class Ipt {
		private String str;
		private Font fnt;
	}

	//
	private class SPanel extends JPanel {

		//
		private static final long serialVersionUID =
			8388728689806126886L;

		//
		private void paintImage(Graphics g, ImageInfo info) {
			MutablePoint2D p0, ps;

			if(coordinate != null) {
				p0 = coordinate.transform(
						getWidth(),
						getHeight(),
						info.x, info.y);
				ps = coordinate.transformSize(
						getWidth(),
						getHeight(),
						info.sx, info.sy);
				g.drawImage(info.img,
						p0.getXInt(),
						p0.getYInt() - ps.getYInt(),
						this);
			} else {
				g.drawImage(info.img, (int)info.x, (int)info.y, this);
			}
		}

		/* (non-Javadoc)
		 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
		 */
		@Override
		protected void paintComponent(Graphics g2) {
			Graphics g;

			g = g2;
			super.paintComponent(g);
			synchronized(bgdrawers) {
				for(Drawer drawer : bgdrawers) {
					drawer.draw(
							g,
							getWidth(),
							getHeight(),
							coordinate);
				}
			}
			synchronized(images) {
				for(ImageInfo info : images) {
					paintImage(g, info);
				}
			}
			synchronized(drawers) {
				for(Drawer drawer : drawers) {
					drawer.draw(
							g,
							getWidth(),
							getHeight(),
							coordinate);
				}
			}

			synchronized(queue) {
				Graphics2D g2d = (Graphics2D)g;
				for(Map.Entry<Ipt, Rectangle2D> e : queue.entrySet()) {
					TextLayout tl = new TextLayout(
							e.getKey().str, e.getKey().fnt,
							g2d.getFontRenderContext());
	
					e.setValue(tl.getBounds());
				}
				queue.notifyAll();
			}
		}

	}

	/**
	 * 
	 */
	protected CoordinateMap coordinate;

	//
	private List<ImageInfo> images    = new ArrayList<ImageInfo>();
	private List<Drawer>    drawers   = new ArrayList<Drawer>();
	private List<Drawer>    bgdrawers = new ArrayList<Drawer>();
	private Map<Ipt, Rectangle2D> queue =
		new HashMap<Ipt, Rectangle2D>();

	/**
	 * 
	 * @param coordinate
	 */
	public SchlushPanelFrame(CoordinateMap coordinate) {
		this.coordinate = coordinate;
	}

	/**
	 * 
	 */
	public SchlushPanelFrame() {
		this(null);
	}

	/**
	 * 
	 * @param img
	 * @throws InterruptedException
	 */
	protected abstract void loadImage(
			Image img) throws InterruptedException;

	/**
	 * 
	 * @return
	 */
	public JPanel createPanel() {
		return new SPanel();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#addImage(net.morilib.lisp.painter.SchlushFrame.ImageInfo)
	 */
	public void addImage(ImageInfo img) throws InterruptedException {
		Image img2 = null;
		MutablePoint2D ps;

		if(coordinate != null) {
			ps = coordinate.transformSize(
					getWidth(),
					getHeight(),
					img.sx, img.sy);

			if(ps.getXInt() > 0 && ps.getYInt() > 0) {
				img2 = img.img.getScaledInstance(
						ps.getXInt(), ps.getYInt(),
						Image.SCALE_SMOOTH);
			}
		} else {
			if((int)img.sx > 0 && (int)img.sy > 0) {
				img2 = img.img.getScaledInstance(
						(int)img.sx, (int)img.sy,
						Image.SCALE_SMOOTH);
			}
		}

		if(img2 != null) {
			loadImage(img.img);
			synchronized(images) {
				images.add(new ImageInfo(
						img2, img.x, img.y, img.sx, img.sy));
			}
		}
		repaint();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#addDrawer(net.morilib.lisp.painter.drawer.Drawer)
	 */
	public void addDrawer(Drawer drawer) {
		synchronized(drawers) {
			drawers.add(drawer);
		}
		repaint();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#addBackgroundDrawer(net.morilib.lisp.painter.drawer.Drawer)
	 */
	public void addBackgroundDrawer(Drawer drawer) {
		synchronized(bgdrawers) {
			bgdrawers.add(drawer);
		}
		repaint();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#clearScreen()
	 */
	public void clearScreen() {
		synchronized(images) {
			images.clear();
		}
		synchronized(drawers) {
			drawers.clear();
		}
		repaint();
	}

	/**
	 * @return the coordinate
	 */
	public CoordinateMap getCoordinate() {
		return coordinate;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#calculateFontBounds(java.lang.String, net.morilib.lisp.painter.SchlushFont)
	 */
	public Rectangle2D calculateFontBounds(
			String str, SchlushFont font) {
		Ipt ipt = new Ipt();

		ipt.str = str;
		ipt.fnt = font.font;
		synchronized(queue) {
			queue.put(ipt, null);
			repaint();
			try {
				if(queue.get(ipt) == null) {
					queue.wait();
				}
			} catch (InterruptedException e) {
				throw new RuntimeException(e);
			}
		}
		return queue.remove(ipt);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double transformX(double x) {
		return coordinate.transform(
				getWidth(), getHeight(), x, 0).getX();
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double transformY(double y) {
		return coordinate.transform(
				getWidth(), getHeight(), 0, y).getY();
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double invertX(int x) {
		return coordinate.invert(
				getWidth(), getHeight(), x, 0).getX();
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public double invertY(int y) {
		return coordinate.invert(
				getWidth(), getHeight(), 0, y).getY();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<swing-panel>");
	}

}
