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
package net.morilib.lisp.math.geometry.g2d;

import java.awt.geom.AffineTransform;

import net.morilib.geometry.g2d.VectorTransformer2D;
import net.morilib.lang.Hashes;
import net.morilib.lang.number.NumericalField;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispReal;

/**
 *
 *
 * @deprecated
 * @author MORIGUCHI, Yuichiro 2011/01/29
 */
public class LispAffineTransformer2D extends Datum
implements VectorTransformer2D<LispVector2D, LispReal>,
java.io.Serializable {

	//
	private LispReal scaleX, scaleY;
	private LispReal shearX, shearY;
	private LispReal transformX, transformY;

	/**
	 * 
	 * @param scaleX
	 * @param scaleY
	 * @param shearX
	 * @param shearY
	 * @param transformX
	 * @param transformY
	 */
	public LispAffineTransformer2D(
			LispReal scaleX, LispReal scaleY,
			LispReal shearX, LispReal shearY,
			LispReal transformX, LispReal transformY) {
		if(scaleX == null || scaleY == null) {
			throw new NullPointerException();
		} else if(shearX == null || shearY == null) {
			throw new NullPointerException();
		} else if(transformX == null || transformY == null) {
			throw new NullPointerException();
		}
		this.scaleX = scaleX;
		this.shearX = shearX;
		this.transformX = transformX;
		this.scaleY = scaleY;
		this.shearY = shearY;
		this.transformY = transformY;
	}

	/**
	 * 
	 * @param <P>
	 * @param <LispReal>
	 * @param ff
	 * @param theta
	 * @return
	 */
	public static LispAffineTransformer2D getRotator(double theta) {
		NumericalField<LispReal> ff = LispDouble.FIELD;
		double sin = Math.sin(theta);
		double cos = Math.cos(theta);

		return new LispAffineTransformer2D(
				ff.valueOf(cos),  ff.valueOf(cos),
				ff.valueOf(-sin), ff.valueOf(sin),
				ff.getZero(),     ff.getZero());
	}

	/**
	 * 
	 * @param <P>
	 * @param <LispReal>
	 * @param ff
	 * @param anchor
	 * @param theta
	 * @return
	 */
	public static LispAffineTransformer2D getRotator(
			LispVector2D anchor, double theta) {
		NumericalField<LispReal> ff = LispDouble.FIELD;
		double x = anchor.getX().doubleValue();
		double y = anchor.getY().doubleValue();
		double sin = Math.sin(theta);
		double cos = Math.cos(theta);

		return new LispAffineTransformer2D(
				ff.valueOf(cos),  ff.valueOf(cos),
				ff.valueOf(-sin), ff.valueOf(sin),
				ff.valueOf(x - x * cos + y * sin),
				ff.valueOf(y - x * sin - y * cos));
	}

	/**
	 * @return the scaleX
	 */
	public LispReal getScaleX() {
		return scaleX;
	}

	/**
	 * @return the scaleY
	 */
	public LispReal getScaleY() {
		return scaleY;
	}

	/**
	 * @return the shearX
	 */
	public LispReal getShearX() {
		return shearX;
	}

	/**
	 * @return the shearY
	 */
	public LispReal getShearY() {
		return shearY;
	}

	/**
	 * @return the transformX
	 */
	public LispReal getTransformX() {
		return transformX;
	}

	/**
	 * @return the transformY
	 */
	public LispReal getTransformY() {
		return transformY;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.geometry.g2d.VectorTransformer2D#transform(net.morilib.geometry.g2d.Vector2D)
	 */
	public LispVector2D transform(LispVector2D p) {
		LispReal x, y;

		x = scaleX.multiply(p.getX());
		x = x.add(shearX).multiply(p.getY());
		x = x.add(transformX);
		y = scaleY.multiply(p.getY());
		y = y.add(shearY).multiply(p.getX());
		y = y.add(transformY);
		return p.getFactory().create(x, y);
	}

	/**
	 * 
	 * @return
	 */
	public AffineTransform toAWTTransform() {
		return new AffineTransform(
				scaleX.doubleValue(), shearY.doubleValue(),
				scaleX.doubleValue(), scaleY.doubleValue(),
				transformX.doubleValue(), transformY.doubleValue());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("\n");
		buf.append("[ ").append(scaleX.toString());
		buf.append(" ").append(shearX.toString());
		buf.append(" ").append(transformX.toString()).append("]\n");
		buf.append("[ ").append(shearY.toString());
		buf.append(" ").append(scaleY.toString());
		buf.append(" ").append(transformY.toString()).append("]\n");
		buf.append("[0 0 1]\n");
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;

		r = (r + scaleX.hashCode()) * Hashes.A;
		r = (r + scaleY.hashCode()) * Hashes.A;
		r = (r + shearX.hashCode()) * Hashes.A;
		r = (r + shearY.hashCode()) * Hashes.A;
		r = (r + transformX.hashCode()) * Hashes.A;
		r = (r + transformY.hashCode()) * Hashes.A;
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispAffineTransformer2D) {
			LispAffineTransformer2D t;

			t = (LispAffineTransformer2D)obj;
			return (scaleX.equals(t.scaleX) &&
					scaleY.equals(t.scaleY) &&
					shearX.equals(t.shearX) &&
					shearY.equals(t.shearY) &&
					transformX.equals(t.transformX) &&
					transformY.equals(t.transformY));
		}
		return false;
	}

}
