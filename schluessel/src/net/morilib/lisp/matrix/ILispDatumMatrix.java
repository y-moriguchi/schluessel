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
package net.morilib.lisp.matrix;

import net.morilib.lisp.Datum;
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.math.LispPermutation;
import net.morilib.lisp.math.matrix.LispMatrixException;
import net.morilib.lisp.uvector.HomogeneousUnsignedArray;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public interface ILispDatumMatrix {

	/**
	 * 
	 * @param row
	 * @param column
	 * @return
	 */
	public Datum get(int row, int column);

	/**
	 * 
	 * @param row
	 * @param column
	 * @param x
	 */
	public void set(int row, int column,
			Datum x) throws LispMatrixException;

	/**
	 * 
	 * @param column
	 * @return
	 */
	public ILispVector getRowVector(int row);

	/**
	 * 
	 * @param row
	 * @return
	 */
	public ILispVector getColumnVector(int column);

	/**
	 * 
	 * @return
	 */
	public ILispDatumMatrix transpose();

	/**
	 * 
	 * @return
	 */
	public int rowSize();

	/**
	 * 
	 * @return
	 */
	public int columnSize();

	/**
	 * 
	 * @param rowp
	 * @param colp
	 * @return
	 */
	public ILispDatumMatrix changeRow(LispPermutation rowp);

	/**
	 * 
	 * @param rowp
	 * @param colp
	 * @return
	 */
	public ILispDatumMatrix changeColumn(LispPermutation colp);

	/**
	 * 
	 * @param rowb
	 * @param rowe
	 * @param colb
	 * @param cole
	 * @return
	 */
	public ILispDatumMatrix submatrix(int rowb, int rowe, int colb,
			int cole);

	/**
	 * 
	 * @param row
	 * @param col
	 * @return
	 */
	public ILispDatumMatrix submatrix(HomogeneousUnsignedArray row,
			HomogeneousUnsignedArray col) throws LispMatrixException;

	/**
	 * 
	 * @return
	 */
	public boolean isSquare();

}
