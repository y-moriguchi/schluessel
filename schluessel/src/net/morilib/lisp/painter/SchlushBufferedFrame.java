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

import net.morilib.lisp.Datum2;
import net.morilib.lisp.painter.drawer.CoordinateMap;
import net.morilib.lisp.painter.drawer.Drawer;
import net.morilib.lisp.painter.geom.MutablePoint2D;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/12
 */
public abstract class SchlushBufferedFrame
extends Datum2 implements SchlushFrame {

	//
	private class Ipt {
		private String str;
		private Font fnt;
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
	public SchlushBufferedFrame(CoordinateMap coordinate) {
		this.coordinate = coordinate;
	}

	/**
	 * 
	 */
	public SchlushBufferedFrame() {
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
	 * @param g
	 * @param info
	 * @param c
	 */
	protected void paintImage(
			Graphics g, ImageInfo info, Component c) {
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
					c);
		} else {
			g.drawImage(info.img, (int)info.x, (int)info.y, c);
		}
	}

	/**
	 * 
	 * @param g2
	 * @param c
	 */
	protected void paintDrawers(Graphics g2, Component c) {
		Graphics g;

		g = g2;
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
				paintImage(g, info, c);
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

}
