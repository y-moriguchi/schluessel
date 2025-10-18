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

import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.matrix.AbstractImmutableLispMatrix;
import net.morilib.lisp.math.matrix.ILispMatrix;
import net.morilib.lisp.math.matrix.LispMatrixException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/19
 */
public class LispDoubleLinearTransform2D
extends AbstractImmutableLispMatrix
implements ILispAffineTransform2D {

	/**
	 * 
	 */
	public static final LispDoubleLinearTransform2D IDENTITY =
		new LispDoubleLinearTransform2D(1, 1, 0, 0);

	/**
	 * 
	 */
	public static final LispDoubleLinearTransform2D REFLECTION_X =
		new LispDoubleLinearTransform2D(1, -1, 0, 0);

	/**
	 * 
	 */
	public static final LispDoubleLinearTransform2D REFLECTION_Y =
		new LispDoubleLinearTransform2D(-1, 1, 0, 0);

	//
	private double scaleX, scaleY;
	private double shearX, shearY;

	/**
	 * 
	 * @param scaleX
	 * @param scaleY
	 * @param shearX
	 * @param shearY
	 */
	public LispDoubleLinearTransform2D(
			double scaleX, double scaleY,
			double shearX, double shearY) {
		this.scaleX = scaleX;
		this.scaleY = scaleY;
		this.shearX = shearX;
		this.shearY = shearY;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#rotateByRadian(double)
	 */
	public static LispDoubleLinearTransform2D createRotateByRadian(
			double rad) {
		return new LispDoubleLinearTransform2D(
				Math.cos(rad), Math.cos(rad),
				Math.sin(rad), -Math.sin(rad));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#rotateByIntDegree(int)
	 */
	public static LispDoubleLinearTransform2D createRotateByIntDegree(
			int deg) {
		switch(deg % 360) {
		case 0:  return IDENTITY;
		case 90:  case -270:
			return new LispDoubleLinearTransform2D(0, 0, 1, -1);
		case 180:  case -180:
			return new LispDoubleLinearTransform2D(-1, -1, 0, 0);
		case 270: case -90:
			return new LispDoubleLinearTransform2D(0, 0, -1, 1);
		default:
			return createRotateByRadian((double)deg * Math.PI / 180.0);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#scale(double, double)
	 */
	public static LispDoubleLinearTransform2D createScale(
			double sx, double sy) {
		return new LispDoubleLinearTransform2D(
				1 * sx, 1 * sy, 0, 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#shear(double, double)
	 */
	public static LispDoubleLinearTransform2D createShear(
			double sx, double sy) {
		return new LispDoubleLinearTransform2D(1, 1, sx, sy);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#shear(double, double)
	 */
	public static LispDoubleLinearTransform2D createSqueeze(
			double k) {
		return new LispDoubleLinearTransform2D(
				k, 1 / k, 0, 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispMatrix#get(int, int)
	 */
	@Override
	public LispNumber get(int row, int column) {
		switch(row) {
		case 0:
			switch(column) {
			case 0:  return new LispDouble(scaleX);
			case 1:  return new LispDouble(shearX);
			default:  throw new IndexOutOfBoundsException();
			}
		case 1:
			switch(column) {
			case 0:  return new LispDouble(shearY);
			case 1:  return new LispDouble(scaleY);
			default:  throw new IndexOutOfBoundsException();
			}
		default:  throw new IndexOutOfBoundsException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispDatumMatrix#rowSize()
	 */
	@Override
	public int rowSize() {
		return 2;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispDatumMatrix#columnSize()
	 */
	@Override
	public int columnSize() {
		return 2;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getScaleX()
	 */
	@Override
	public LispReal getScaleX() {
		return new LispDouble(scaleX);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getScaleY()
	 */
	@Override
	public LispReal getScaleY() {
		return new LispDouble(scaleY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getShearX()
	 */
	@Override
	public LispReal getShearX() {
		return new LispDouble(shearX);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getShearY()
	 */
	@Override
	public LispReal getShearY() {
		return new LispDouble(shearY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getTransformX()
	 */
	@Override
	public LispReal getTranslateX() {
		return LispDouble.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getTransformY()
	 */
	@Override
	public LispReal getTranslateY() {
		return LispDouble.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getScaleXDouble()
	 */
	@Override
	public double getScaleXDouble() {
		return scaleX;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getScaleYDouble()
	 */
	@Override
	public double getScaleYDouble() {
		return scaleY;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getShearXDouble()
	 */
	@Override
	public double getShearXDouble() {
		return shearX;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getShearYDouble()
	 */
	@Override
	public double getShearYDouble() {
		return shearY;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getTransformXDouble()
	 */
	@Override
	public double getTranslateXDouble() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#getTransformYDouble()
	 */
	@Override
	public double getTranslateYDouble() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#toAWTTransform()
	 */
	@Override
	public AffineTransform toAWTTransform() {
		return new AffineTransform(
				scaleX, shearY, shearX, scaleY, 0, 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.AbstractLispMatrix#determinant()
	 */
	@Override
	public LispNumber determinant() {
		return new LispDouble(scaleX * scaleY - shearX * shearY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.AbstractLispMatrix#mul(net.morilib.lisp.math.matrix.ILispMatrix)
	 */
	@Override
	public ILispMatrix mul(ILispMatrix a) {
		if(a instanceof LispDoubleLinearTransform2D) {
			return concatenate((ILispAffineTransform2D)a);
		} else {
			return super.mul(a);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.AbstractLispMatrix#transpose()
	 */
	@Override
	public ILispMatrix transpose() {
		return new LispDoubleLinearTransform2D(
				scaleX, scaleY, shearY, shearX);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.AbstractLispMatrix#adjoint()
	 */
	@Override
	public ILispMatrix adjoint() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.AbstractLispMatrix#add(net.morilib.lisp.math.matrix.ILispMatrix)
	 */
	@Override
	public ILispMatrix add(ILispMatrix a) {
		if(a instanceof LispDoubleLinearTransform2D) {
			LispDoubleLinearTransform2D z;

			z = (LispDoubleLinearTransform2D)a;
			return new LispDoubleLinearTransform2D(
					scaleX + z.scaleX, scaleY + z.scaleY,
					shearX + z.shearX, shearY + z.shearY);
		} else {
			return super.add(a);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.AbstractLispMatrix#sub(net.morilib.lisp.math.matrix.ILispMatrix)
	 */
	@Override
	public ILispMatrix sub(ILispMatrix a) {
		if(a instanceof LispDoubleLinearTransform2D) {
			LispDoubleLinearTransform2D z;

			z = (LispDoubleLinearTransform2D)a;
			return new LispDoubleLinearTransform2D(
					scaleX - z.scaleX, scaleY - z.scaleY,
					shearX - z.shearX, shearY - z.shearY);
		} else {
			return super.sub(a);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.AbstractLispMatrix#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public ILispMatrix mul(LispNumber a) {
		if(a.isReal()) {
			return new LispDoubleLinearTransform2D(
					scaleX * a.getRealDouble(),
					scaleY * a.getRealDouble(),
					shearX * a.getRealDouble(),
					shearY * a.getRealDouble());
		} else {
			return super.mul(a);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.AbstractLispMatrix#uminus()
	 */
	@Override
	public ILispMatrix uminus() {
		return new LispDoubleLinearTransform2D(
				-scaleX, -scaleY, -shearX, -shearY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.AbstractLispMatrix#inv()
	 */
	@Override
	public ILispMatrix inv() {
		final double D;

		if((D = scaleX * scaleY - shearX * shearY) == 0) {
			throw new LispMatrixException();
		} else {
			return new LispDoubleLinearTransform2D(
					scaleY  / D, scaleX  / D,
					-shearX / D, -shearY / D);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#rotateByRadian(double)
	 */
	@Override
	public ILispAffineTransform2D rotateByRadian(double rad) {
		return new LispDoubleLinearTransform2D(
				scaleX * Math.cos(rad) + shearX * -Math.sin(rad),
				shearY * Math.sin(rad) + scaleY * Math.cos(rad),
				scaleX * Math.sin(rad) + shearX * Math.cos(rad),
				shearY * Math.cos(rad) + scaleY * -Math.sin(rad));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#rotateByIntDegree(int)
	 */
	@Override
	public ILispAffineTransform2D rotateByIntDegree(int deg) {
		switch(deg % 360) {
		case 0:
			return this;
		case 90:  case -270:
			return new LispDoubleLinearTransform2D(
					-shearX, shearY, scaleX, -scaleY);
		case 180:  case -180:
			return new LispDoubleLinearTransform2D(
					-scaleX, -scaleY, -shearX, -shearY);
		case 270: case -90:
			return new LispDoubleLinearTransform2D(
					shearX, -shearY, -scaleX, scaleY);
		default:
			return rotateByRadian((double)deg * Math.PI / 180.0);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#scale(double, double)
	 */
	@Override
	public ILispAffineTransform2D scale(double sx, double sy) {
		return new LispDoubleLinearTransform2D(
				scaleX * sx, scaleY * sy, shearX, shearY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#shear(double, double)
	 */
	@Override
	public ILispAffineTransform2D shear(double sx, double sy) {
		return new LispDoubleLinearTransform2D(
				scaleX, scaleY, shearX * sx, shearY * sy);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#translate(double, double)
	 */
	@Override
	public ILispAffineTransform2D translate(double tx, double ty) {
		return new LispDoubleAffineTransform2D(
				scaleX, scaleY, shearX, shearY, tx, ty);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#reflectX()
	 */
	@Override
	public ILispAffineTransform2D reflectX() {
		return new LispDoubleLinearTransform2D(
				scaleX, -scaleY, shearX, shearY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#reflectY()
	 */
	@Override
	public ILispAffineTransform2D reflectY() {
		return new LispDoubleLinearTransform2D(
				-scaleX, scaleY, shearX, shearY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#squeeze(double)
	 */
	@Override
	public ILispAffineTransform2D squeeze(double k) {
		return new LispDoubleLinearTransform2D(
				scaleX * k, scaleY / k, shearX, shearY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D#concatenate(net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D)
	 */
	@Override
	public ILispAffineTransform2D concatenate(
			ILispAffineTransform2D t) {
		if(t instanceof LispDoubleLinearTransform2D) {
			LispDoubleLinearTransform2D z;

			z = (LispDoubleLinearTransform2D)t;
			return new LispDoubleLinearTransform2D(
					scaleX * z.scaleX + shearX * z.shearY,
					shearY * z.shearX + scaleY * z.scaleY,
					scaleX * z.shearX + shearX * z.scaleY,
					shearY * z.scaleX + scaleY * z.shearY);
		} else {
			LispDoubleAffineTransform2D z;

			z = (LispDoubleAffineTransform2D)t;
			return new LispDoubleAffineTransform2D(
					scaleX * z.getScaleXDouble() +
					shearX * z.getShearYDouble(),
					shearY * z.getShearXDouble() +
					scaleY * z.getScaleYDouble(),
					scaleX * z.getShearXDouble() +
					shearX * z.getScaleYDouble(),
					shearY * z.getScaleXDouble() +
					scaleY * z.getShearYDouble(),
					scaleX * z.getTranslateXDouble() +
					shearX * z.getTranslateYDouble(),
					shearY * z.getTranslateXDouble() +
					scaleY * z.getTranslateYDouble());
		}
	}

}
