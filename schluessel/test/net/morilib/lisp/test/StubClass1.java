/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class StubClass1 {
	
	private boolean bboolean;
	private char    bchar;
	private int     bint;
	private long    blong;
	private double  bdouble;
	private BigInteger bBigInteger;
	private BigDecimal bBigDecimal;
	private String bString;
	
	private Map<Integer, Boolean>    biboolean =
		new HashMap<Integer, Boolean>();
	private Map<Integer, Character>  bichar =
		new HashMap<Integer, Character>();
	private Map<Integer, Integer>    biint =
		new HashMap<Integer, Integer>();
	private Map<Integer, Long>       bilong =
		new HashMap<Integer, Long>();
	private Map<Integer, Double>     bidouble =
		new HashMap<Integer, Double>();
	private Map<Integer, BigInteger> biBigInteger =
		new HashMap<Integer, BigInteger>();
	private Map<Integer, BigDecimal> biBigDecimal =
		new HashMap<Integer, BigDecimal>();
	private Map<Integer, String>     biString =
		new HashMap<Integer, String>();
	
	private String[] strlist;
	private int[][]  matrix;
	private String[] init0;
	
	public StubClass1(String... strs) {
		init0 = strs;
	}
	
	public String[] getInit0() {
		return init0;
	}
	
	public void setInit0(String... strs) {
		init0 = strs;
	}
	
	/**
	 * @return the bboolean
	 */
	public boolean isBboolean() {
		return bboolean;
	}
	
	/**
	 * @param bboolean the bboolean to set
	 */
	public void setBboolean(boolean bboolean) {
		this.bboolean = bboolean;
	}
	
	/**
	 * @return the bchar
	 */
	public char getBchar() {
		return bchar;
	}
	
	/**
	 * @param bchar the bchar to set
	 */
	public void setBchar(char bchar) {
		this.bchar = bchar;
	}
	
	/**
	 * @return the blong
	 */
	public int getBint() {
		return bint;
	}
	
	/**
	 * @param blong the blong to set
	 */
	public void setBint(int bint) {
		this.bint = bint;
	}
	
	/**
	 * @return the blong
	 */
	public long getBlong() {
		return blong;
	}
	
	/**
	 * @param blong the blong to set
	 */
	public void setBlong(long blong) {
		this.blong = blong;
	}
	
	/**
	 * @return the bdouble
	 */
	public double getBdouble() {
		return bdouble;
	}
	
	/**
	 * @param bdouble the bdouble to set
	 */
	public void setBdouble(double bdouble) {
		this.bdouble = bdouble;
	}
	
	/**
	 * @return the bBigInteger
	 */
	public BigInteger getBBigInteger() {
		return bBigInteger;
	}
	
	/**
	 * @param bigInteger the bBigInteger to set
	 */
	public void setBBigInteger(BigInteger bigInteger) {
		bBigInteger = bigInteger;
	}
	
	/**
	 * @return the bBigDecimal
	 */
	public BigDecimal getBBigDecimal() {
		return bBigDecimal;
	}
	
	/**
	 * @param bigDecimal the bBigDecimal to set
	 */
	public void setBBigDecimal(BigDecimal bigDecimal) {
		bBigDecimal = bigDecimal;
	}
	
	/**
	 * @return the bString
	 */
	public String getBString() {
		return bString;
	}
	
	/**
	 * @param string the bString to set
	 */
	public void setBString(String string) {
		bString = string;
	}
	
	/**
	 * @return the bboolean
	 */
	public boolean getBiboolean(int index) {
		return biboolean.get(Integer.valueOf(index));
	}
	
	/**
	 * @param bboolean the bboolean to set
	 */
	public void setBiboolean(int index, boolean bboolean) {
		this.biboolean.put(Integer.valueOf(index), bboolean);
	}
	
	/**
	 * @return the bchar
	 */
	public char getBichar(int index) {
		return bichar.get(Integer.valueOf(index));
	}
	
	/**
	 * @param bchar the bchar to set
	 */
	public void setBichar(int index, char bchar) {
		this.bichar.put(Integer.valueOf(index), bchar);
	}
	
	/**
	 * @return the blong
	 */
	public int getBiint(int index) {
		return biint.get(Integer.valueOf(index));
	}
	
	/**
	 * @param blong the blong to set
	 */
	public void setBiint(int index, int bint) {
		this.biint.put(Integer.valueOf(index), bint);
	}
	
	/**
	 * @return the blong
	 */
	public long getBilong(int index) {
		return bilong.get(Integer.valueOf(index));
	}
	
	/**
	 * @param blong the blong to set
	 */
	public void setBilong(int index, long blong) {
		this.bilong.put(Integer.valueOf(index), blong);
	}
	
	/**
	 * @return the bdouble
	 */
	public double getBidouble(int index) {
		return bidouble.get(Integer.valueOf(index));
	}
	
	/**
	 * @param bdouble the bdouble to set
	 */
	public void setBidouble(int index, double bdouble) {
		this.bidouble.put(Integer.valueOf(index), bdouble);
	}
	
	/**
	 * @return the bBigInteger
	 */
	public BigInteger getBiBigInteger(int index) {
		return biBigInteger.get(Integer.valueOf(index));
	}
	
	/**
	 * @param bigInteger the bBigInteger to set
	 */
	public void setBiBigInteger(int index, BigInteger bigInteger) {
		biBigInteger.put(Integer.valueOf(index), bigInteger);
	}
	
	/**
	 * @return the bBigDecimal
	 */
	public BigDecimal getBiBigDecimal(int index) {
		return biBigDecimal.get(Integer.valueOf(index));
	}
	
	/**
	 * @param bigDecimal the bBigDecimal to set
	 */
	public void setBiBigDecimal(int index, BigDecimal bigDecimal) {
		biBigDecimal.put(Integer.valueOf(index), bigDecimal);
	}
	
	/**
	 * @return the bString
	 */
	public String getBiString(int index) {
		return biString.get(Integer.valueOf(index));
	}
	
	/**
	 * @param string the bString to set
	 */
	public void setBiString(int index, String string) {
		biString.put(Integer.valueOf(index), string);
	}
	
	/**
	 * @return the strlist
	 */
	public String[] getStrlist() {
		return strlist;
	}
	
	/**
	 * @param strlist the strlist to set
	 */
	public void setStrlist(String[] strlist) {
		this.strlist = strlist;
	}
	
	public String getStr(int index) {
		if(strlist == null) {
			throw new NullPointerException();
		} else if(index < 0 || index >= strlist.length) {
			throw new IndexOutOfBoundsException("" + index);
		}
		return strlist[index];
	}
	
	/**
	 * @return the matrix
	 */
	public int[][] getMatrix() {
		return matrix;
	}
	
	/**
	 * @param matrix the matrix to set
	 */
	public void setMatrix(int[][] matrix) {
		this.matrix = matrix;
	}
	
}
