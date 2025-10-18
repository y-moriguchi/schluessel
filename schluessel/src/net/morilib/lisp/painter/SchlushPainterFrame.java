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

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.font.TextLayout;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

import net.morilib.lisp.painter.drawer.CoordinateMap;
import net.morilib.lisp.painter.drawer.Drawer;
import net.morilib.lisp.painter.geom.MutablePoint2D;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/19
 */
public class SchlushPainterFrame extends ImagePainter
implements SchlushFrame, ILispImage {

	//
	/*package*/ BufferedImage image;
	private CoordinateMap coordinate;
	private final ImageFactory factory = new ImageFactory() {

		public Image getImage() {
			return image;
		}

	};

	/**
	 * 
	 * @param x
	 * @param y
	 * @param coordinate
	 */
	public SchlushPainterFrame(
			int x, int y, CoordinateMap coordinate) {
		image = new BufferedImage(x, y, BufferedImage.TYPE_INT_ARGB);
		this.coordinate = coordinate;
	}

	/**
	 * 
	 * @param img
	 * @param coordinate
	 */
	public SchlushPainterFrame(BufferedImage img,
			CoordinateMap coordinate) {
		this.image = img;
		this.coordinate = coordinate;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#addImage(net.morilib.lisp.painter.SchlushFrame.ImageInfo)
	 */
	public void addImage(ImageInfo img) throws InterruptedException {
		Graphics g = image.createGraphics();
		MutablePoint2D p0, ps;

		if(coordinate != null) {
			p0 = coordinate.transform(
					image.getWidth(), image.getHeight(),
					img.x, img.y);
			ps = coordinate.transformSize(
					image.getWidth(),
					image.getHeight(),
					img.sx, img.sy);
			g.drawImage(img.img,
					p0.getXInt(), p0.getYInt() - ps.getYInt(),
					ps.getXInt(), ps.getYInt(),
					null);
		} else {
			g.drawImage(img.img,
					(int)img.x, (int)img.y,
					(int)img.sx, (int)img.sy,
					null);
		}
		g.dispose();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#addDrawer(net.morilib.lisp.painter.drawer.Drawer)
	 */
	public void addDrawer(Drawer drawer) {
		Graphics g = image.createGraphics();

		drawer.draw(
				g,
				image.getWidth(),
				image.getHeight(),
				coordinate);
		g.dispose();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#addBackgroundDrawer(net.morilib.lisp.painter.drawer.Drawer)
	 */
	public void addBackgroundDrawer(Drawer drawer) {
		addDrawer(drawer);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getImageWidth(java.awt.Image)
	 */
	public int getImageWidth(Image img) {
		return img.getWidth(null);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getImageHeight(java.awt.Image)
	 */
	public int getImageHeight(Image img) {
		return img.getHeight(null);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.ImagePainter#getImageFactory()
	 */
	@Override
	public ImageFactory getImageFactory() {
		return factory;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#clearScreen()
	 */
	public void clearScreen() {
		Graphics g = image.createGraphics();

		g.clearRect(0, 0, image.getWidth(), image.getHeight());
		g.dispose();
	}

	/**
	 * @return the coordinate
	 */
	public CoordinateMap getCoordinate() {
		return coordinate;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getHeight()
	 */
	public int getHeight() {
		return image.getHeight();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#getWidth()
	 */
	public int getWidth() {
		return image.getWidth();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#calculateFontBounds(java.lang.String, net.morilib.lisp.painter.SchlushFont)
	 */
	public Rectangle2D calculateFontBounds(
			String str, SchlushFont font) {
		Graphics2D g2d = image.createGraphics();
		TextLayout tl = new TextLayout(
				str, font.font,
				g2d.getFontRenderContext());

		return tl.getBounds();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.SchlushFrame#repaint()
	 */
	public void repaint() {
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.ILispImage#getImage()
	 */
	public Image getImage() {
		return image;
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
		buf.append("#<painter-frame>");
	}

}
