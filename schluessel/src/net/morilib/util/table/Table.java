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
package net.morilib.util.table;

import java.util.Iterator;
import java.util.List;

/**
 * The interface of the table.
 * <p>2次元の表をあらわすインターフェースである.
 * 
 * @author MORIGUCHI, Yuichiro 2005/04/30
 */
public interface Table<E> {

	/**
	 * gets the element at which the row is r and the column is c.
	 * <p>r行c列にある要素を取得する.
	 * 
	 * @param r  取得したい行
	 * @param c  取得したい列
	 * @return  取得した要素
	 */
	public E get(int r, int c);

	/**
	 * sets the specified object at which the row is r and the column is c.
	 * <p>オブジェクトをr行c列にセットする.
	 * 
	 * @param r  セットする行
	 * @param c  セットする列
	 * @param o  セットするオブジェクト
	 * @return  前にセットされていたオブジェクト
	 */
	public E set(int r, int c, E o);

	/**
	 * returns the row size of this table.
	 * <p>この表の行のサイズを返します.
	 * 
	 * @return  行のサイズ
	 */
	public int rowSize();

	/**
	 * returns the column size of this table.
	 * <p>この表の列のサイズを返します.
	 * 
	 * @return  列のサイズ
	 */
	public int columnSize();

	/**
	 * returns the iterator which iterates the all of elements in this table.
	 * <p>全部の要素について反復するIteratorを返す.
	 * 反復は第1行の全ての列、第2行の全ての列、...の順で反復される.
	 * 
	 * @return  全部の要素について反復するIterator
	 */
	public Iterator<E> wholeIterator();

	/**
	 * returns the iterator which iterates the all of elements in the specified row.
	 * <p>指定された行にある全ての要素について反復するIteratorを返す.
	 * 
	 * @param row  反復したい行
	 * @return   目的のIterator
	 */
	public Iterator<E> rowIterator(int row);

	/**
	 * returns the iterator which iterates the all of elements in the specified column.
	 * <p>指定された列にある全ての要素について反復するIteratorを返す.
	 * 
	 * @param column  反復したい列
	 * @return   目的のIterator
	 */
	public Iterator<E> columnIterator(int column);

	/**
	 * returns the iterator which iterates the all of elements in this table.
	 * <p>全部の要素について反復するIteratorを返す.
	 * 反復は第1行の全ての列、第2行の全ての列、...の順で反復される.
	 * nullの項目はスキップされる.
	 * 
	 * @return  全部の要素について反復するIterator
	 */
	public Iterator<E> wholeIteratorNotNull();

	/**
	 * returns the iterator which iterates the all of elements in the specified row.
	 * <p>指定された行にある全ての要素について反復するIteratorを返す.
	 * nullの項目はスキップされる.
	 * 
	 * @param row  反復したい行
	 * @return   目的のIterator
	 */
	public Iterator<E> rowIteratorNotNull(int row);

	/**
	 * returns the iterator which iterates the all of elements in the specified column.
	 * <p>指定された列にある全ての要素について反復するIteratorを返す.
	 * nullの項目はスキップされる.
	 * 
	 * @param column  反復したい列
	 * @return   目的のIterator
	 */
	public Iterator<E> columnIteratorNotNull(int column);

	/**
	 * returns the table view of this table.
	 * <p>このテーブルの一部分を参照するビューを取得する.
	 * 
	 * @param r1  行の開始地点(この行は含む)
	 * @param r2  行の終了地点(この行は含まない)
	 * @param c1  列の開始地点(この列は含む)
	 * @param c2  列の終了地点(この列は含まない)
	 * @return  テーブルの一部分を参照するビュー
	 */
	public Table<E> subTable(int r1, int r2, int c1, int c2);


	public List<E> getRow(int row);


	public List<E> getColumn(int col);

	/**
	 * sets the specified list to the specified row.
	 * <p>指定した行に指定したリストをセットする.
	 * 
	 * @param row   セットする行
	 * @param list  セットするリスト
	 * @return  セットしたリストの要素数
	 */
	public int setRow(int row, List<E> list);

	/**
	 * sets the specified list to the specified row.
	 * <p>指定した行に指定したリストをセットする.
	 * 
	 * @param row   セットする行
	 * @param list  セットするリスト
	 * @param cbgn  行の開始地点(この点は含む)
	 * @param cend  行の終了地点(この点は含まない)
	 * @return  セットしたリストの要素数
	 */
	public int setRow(int row, List<E> list, int cbgn, int cend);

	/**
	 * adds the specified list to the specified row.
	 * <p>指定した行に指定したリストを追加する.
	 * 
	 * @param row   追加する行
	 * @param list  追加するリスト
	 * @return  追加したリストの要素数
	 */
	public int addRow(int row, List<E> list);

	/**
	 * adds the specified list to the end row of this table.
	 * <p>この表の末尾の行に指定したリストを追加する.
	 * 
	 * @param list  追加するリスト
	 * @return  追加したリストの要素数
	 */
	public int addRow(List<E> list);

	/**
	 * deletes the specified row.
	 * <p>指定した行を削除する.
	 * 
	 * @param row   削除する列
	 */
	public void deleteRow(int row);

	/**
	 * sets the specified list to the specified column.
	 * <p>指定した列に指定したリストをセットする.
	 * 
	 * @param col   セットする列
	 * @param list  セットするリスト
	 * @return  セットしたリストの要素数
	 */
	public int setColumn(int col, List<E> list);

	/**
	 * sets the specified list to the specified column.
	 * <p>指定した列に指定したリストをセットする.
	 * 
	 * @param col   セットする列
	 * @param list  セットするリスト
	 * @param rbgn  列の開始地点(この点は含む)
	 * @param rend  列の終了地点(この点は含まない)
	 * @return  セットしたリストの要素数
	 */
	public int setColumn(int col, List<E> list, int rbgn, int rend);

	/**
	 * adds the specified list to the specified column.
	 * <p>指定した列に指定したリストを追加する.
	 * 
	 * @param col   追加する列
	 * @param list  追加するリスト
	 * @return  追加したリストの要素数
	 */
	public int addColumn(int col, List<E> list);

	/**
	 * adds the specified list to the end column of this table.
	 * <p>この表の末尾の列に指定したリストを追加する.
	 * 
	 * @param list  追加するリスト
	 * @return  追加したリストの要素数
	 */
	public int addColumn(List<E> list);

	/**
	 * deletes the specified row.
	 * <p>指定した列を削除する.
	 * 
	 * @param col   削除する列
	 */
	public void deleteColumn(int col);

	/**
	 * clears the table.
	 * 表をクリアする.
	 */
	public void clear();

}
