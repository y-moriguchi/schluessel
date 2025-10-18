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
package net.morilib.diff;

import java.util.ArrayList;
import java.util.List;

import net.morilib.lang.EqualPredicate;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/10
 */
public final class Diff {

	//
	static final EqualPredicate DEF_EQ = new EqualPredicate() {

		public boolean isEqual(Object a, Object b) {
			return a != null ? a.equals(b) : b == null;
		}

	};

	//
	static<T> EditScript<T> _diff0(List<T> a, List<T> b,
			EqualPredicate eq) {
		EditMatrix<T> s = new EditMatrix<T>(a.size(), b.size());
		T at, bt;

		for(int m = 1; m <= a.size() + b.size(); m++) {
			for(int i = 0; i <= a.size(); i++) {
				for(int j = 0; j <= b.size(); j++) {
					at = i > 0 ? a.get(i - 1) : null;
					bt = j > 0 ? b.get(j - 1) : null;
					if(i == 0 && j == 0) {
						// do nothing
					} else if(s.canApplyRule3(m, i, j, at, bt, eq)) {
						s.setDistance(i, j,
								s.getDistance(i - 1, j - 1));
						s.setScripts(i, j, s.getScripts(i - 1, j - 1));
					} else if(s.canApplyRule1(m, i, j)) {
						s.setDistance(i, j,
								s.getDistance(i, j - 1) + 1);
						s.setScripts(i, j, new InsertScript<T>(
								i, bt, s.getScripts(i, j - 1)));
					} else if(s.canApplyRule2(m, i, j)) {
						s.setDistance(i, j,
								s.getDistance(i - 1, j) + 1);
						s.setScripts(i, j, new DeleteScript<T>(
								i, at, s.getScripts(i - 1, j)));
					}
				}
			}
		}
		return s.getScripts(a.size(), b.size());
	}

	//
	static<T> List<EditScript<T>> _diff(List<T> a, List<T> b,
			EqualPredicate eq) {
		List<EditScript<T>> l = new ArrayList<EditScript<T>>();
		List<EditScript<T>> f = new ArrayList<EditScript<T>>();
		EditScript<T> s = _diff0(a, b, eq);

		for(; s != null; s = s.getPrevious())  l.add(s);
		for(int i = l.size() - 2; i >= 0; i--) {
			f.add(l.get(i));
		}
		return f;
	}

	/**
	 * 
	 * @param <T>
	 * @param a
	 * @param b
	 * @return
	 */
	public static<T> List<Change<T>> diff(List<T> a, List<T> b) {
		return Change.makechange(_diff(a, b, DEF_EQ));
	}

	/**
	 * 
	 * @param <T>
	 * @param a
	 * @param b
	 * @return
	 */
	public static<T> Patch<T> patch(List<T> a, List<T> b) {
		return new DiffPatch<T>(_diff(a, b, DEF_EQ));
	}

	/**
	 * 
	 * @param <T>
	 * @param a
	 * @param b
	 * @return
	 */
	public static<T> String diffToString(List<T> a, List<T> b) {
		StringBuilder f = new StringBuilder();
		List<EditScript<T>> l = new ArrayList<EditScript<T>>();
		EditScript<T> s = _diff0(a, b, DEF_EQ);

		for(; s != null; s = s.getPrevious())  l.add(s);
		for(int i = l.size() - 2; i >= 0; i--) {
			f.append(l.get(i).toString()).append("\n");
		}
		return f.toString();
	}

	/**
	 * 
	 * @param <T>
	 * @param a
	 * @param b
	 * @return
	 */
	public static<T> List<Change<T>> diff(List<T> a, List<T> b,
			EqualPredicate eq) {
		return Change.makechange(_diff(a, b, eq));
	}

	/**
	 * 
	 * @param <T>
	 * @param a
	 * @param b
	 * @return
	 */
	public static<T> Patch<T> patch(List<T> a, List<T> b,
			EqualPredicate eq) {
		return new DiffPatch<T>(_diff(a, b, eq));
	}

	/**
	 * 
	 * @param <T>
	 * @param a
	 * @param b
	 * @return
	 */
	public static<T> String diffToString(List<T> a, List<T> b,
			EqualPredicate eq) {
		StringBuilder f = new StringBuilder();
		List<EditScript<T>> l = new ArrayList<EditScript<T>>();
		EditScript<T> s = _diff0(a, b, eq);

		for(; s != null; s = s.getPrevious())  l.add(s);
		for(int i = l.size() - 2; i >= 0; i--) {
			f.append(l.get(i).toString()).append("\n");
		}
		return f.toString();
	}

}
