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
package net.morilib.lisp.math.matrix;

import net.morilib.lisp.LispNumber;
import net.morilib.lisp.math.LispPermutation;
import net.morilib.lisp.math.algebra.ILispNumberEqual;
import net.morilib.lisp.math.algebra.ILispRing;
import net.morilib.lisp.math.algebra.ILispScalarMultipliable;
import net.morilib.lisp.matrix.ILispDatumMatrix;
import net.morilib.lisp.uvector.HomogeneousUnsignedArray;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public interface ILispMatrix
extends ILispDatumMatrix, ILispRing<ILispMatrix>,
ILispScalarMultipliable<ILispMatrix>, ILispNumberEqual<ILispMatrix> {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#get(int, int)
	 */
	public LispNumber get(int row, int column);

	/**
	 * 
	 * @param column
	 * @return
	 */
	public ILispNumberVector getRowVector(int row);

	/**
	 * 
	 * @param row
	 * @return
	 */
	public ILispNumberVector getColumnVector(int column);

	/**
	 * 
	 * @param n
	 * @return
	 */
	public ILispMatrix mul(LispNumber a);

	/**
	 * 
	 * @param a
	 * @return
	 */
	public ILispMatrix add(ILispMatrix a);

	/**
	 * 
	 * @param a
	 * @return
	 */
	public ILispMatrix sub(ILispMatrix a);

	/**
	 * 
	 * @return
	 */
	public ILispMatrix uminus();

	/**
	 * 
	 * @param a
	 * @return
	 * @throws LispMatrixException
	 */
	public ILispMatrix mul(ILispMatrix a);

	/**
	 * 
	 * @param a
	 * @return
	 * @throws LispMatrixException
	 */
	public ILispNumberVector mul(ILispNumberVector a);

	/**
	 * 
	 * @return
	 * @throws LispMatrixException
	 */
	public LispNumber determinant();

	/**
	 * 
	 * @return
	 * @throws LispMatrixException
	 */
	public ILispMatrix inv();

	/**
	 * 
	 * @return
	 */
	public boolean isHermite();

	/**
	 * 
	 * @return
	 */
	public ILispMatrix adjoint();

	/**
	 * 
	 * @param rowp
	 * @param colp
	 * @return
	 */
	public ILispMatrix changeRow(LispPermutation rowp);

	/**
	 * 
	 * @param rowp
	 * @param colp
	 * @return
	 */
	public ILispMatrix changeColumn(LispPermutation colp);

	/**
	 * 
	 * @param rowb
	 * @param rowe
	 * @param colb
	 * @param cole
	 * @return
	 */
	public ILispMatrix submatrix(int rowb, int rowe, int colb,
			int cole);

	/**
	 * 
	 * @param row
	 * @param col
	 * @return
	 */
	public ILispMatrix submatrix(HomogeneousUnsignedArray row,
			HomogeneousUnsignedArray col);

	/**
	 * 
	 * @param y
	 * @return
	 */
	public boolean isEqualTo(ILispMatrix y);

}
