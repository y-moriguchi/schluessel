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
package net.morilib.lisp.painter.turtle;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;

import net.morilib.lisp.painter.drawer.CompositeDrawer;
import net.morilib.lisp.painter.drawer.CoordinateMap;
import net.morilib.lisp.painter.drawer.Drawer;
import net.morilib.lisp.painter.geom.MutablePoint2D;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/28
 */
public class TurtleDrawer extends CompositeDrawer {

	//
	private double tx, ty, tt = 0, stx, sty;
	private BufferedImage image;
	private ImageObserver obs;
	boolean show = true;

	/**
	 * @param drawers
	 */
	public TurtleDrawer(double x, double y,
			BufferedImage img, ImageObserver obs) {
		super();
		stx = tx = x;
		sty = ty = y;
		image = img;
		this.obs = obs;
	}

	/**
	 * @return the x
	 */
	public double getX() {
		return tx;
	}

	/**
	 * @return the y
	 */
	public double getY() {
		return ty;
	}

	/**
	 * @return the angle
	 */
	public double getAngle() {
		return tt;
	}

	/**
	 * 
	 * @param t
	 */
	public void addTurnLeftByDegree(final double t) {
		add(new Drawer() {

			@Override
			public void draw(Graphics g, int framex, int framey,
					CoordinateMap coordinate) {
				tt = Math.IEEEremainder(tt + t, 360);
			}

		});
	}

	/**
	 * 
	 * @param t
	 */
	public void addTurnLeftByRadian(final double t) {
		add(new Drawer() {

			@Override
			public void draw(Graphics g, int framex, int framey,
					CoordinateMap coordinate) {
				tt = Math.IEEEremainder(tt + t * 180 / Math.PI, 360);
			}

		});
	}

	/**
	 * 
	 * @param r
	 */
	public void addDrawLine(final double r) {
		add(new Drawer() {

			@Override
			public void draw(Graphics g, int framex, int framey,
					CoordinateMap coordinate) {
				if(coordinate != null) {
					double nx = tx - r * Math.sin(tt / 180 * Math.PI);
					double ny = ty + r * Math.cos(tt / 180 * Math.PI);
					MutablePoint2D p1 =
						coordinate.transform(framex, framey, tx, ty);
					MutablePoint2D p2 =
						coordinate.transform(framex, framey, nx, ny);

					g.drawLine(p1.getXInt(), p1.getYInt(),
							p2.getXInt(), p2.getYInt());
					tx = nx;  ty = ny;
				} else {
					double nx = tx - r * Math.sin(tt / 180 * Math.PI);
					double ny = ty + r * Math.cos(tt / 180 * Math.PI);

					g.drawLine((int)tx, (int)ty, (int)nx, (int)ny);
					tx = nx;  ty = ny;
				}
			}

		});
	}

	/**
	 * 
	 * @param r
	 */
	public void addMove(final double r) {
		add(new Drawer() {

			@Override
			public void draw(Graphics g, int framex, int framey,
					CoordinateMap coordinate) {
				tx = tx - r * Math.sin(tt / 180 * Math.PI);
				ty = ty + r * Math.cos(tt / 180 * Math.PI);
			}

		});
	}

	/**
	 * 
	 * @param r
	 */
	public void addColor(final Color c) {
		add(new Drawer() {

			@Override
			public void draw(Graphics g, int framex, int framey,
					CoordinateMap coordinate) {
				g.setColor(c);
			}

		});
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.drawer.Drawer#draw(java.awt.Graphics, int, int, net.morilib.lisp.painter.drawer.CoordinateMap)
	 */
	@Override
	public void draw(Graphics g, int framex, int framey,
			CoordinateMap coordinate) {
		MutablePoint2D p1 =
			coordinate.transform(framex, framey, tx, ty);
		AffineTransform rt, rr;
//		Point2D pa, pb;

		tx = stx;  ty = sty;  tt = 0;
		synchronized(this) {
			for(Drawer d : drawers) {
				d.draw(g, framex, framey, coordinate);
			}
		}

		// draw the image
		if(show) {
			rt = ((Graphics2D)g).getTransform();
			rr = AffineTransform.getTranslateInstance(
					p1.getX(), p1.getY());
			rr.rotate(-tt / 180 * Math.PI);
			rr.translate(-p1.getX(), -p1.getY());
			((Graphics2D)g).setTransform(rr);
	//		pa = new Point2D.Double(
	//				p1.getX() - image.getWidth() / 2.0,
	//				p1.getY() - image.getHeight() / 2.0);
	//		pb = new Point2D.Double();
	//		rr.transform(pa, pb);
			g.drawImage(image,
					p1.getXInt() - image.getWidth() / 2,
					p1.getYInt() - image.getHeight() / 2,
					obs);
			((Graphics2D)g).setTransform(rt);
		}
	}

}
