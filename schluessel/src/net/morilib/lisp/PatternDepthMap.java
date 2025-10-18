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
package net.morilib.lisp;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/*package*/ class PatternDepthMap {
	
	private Map<Symbol, Integer> depthMap =
		new HashMap<Symbol, Integer>();
	
	private Map<Symbol, Map<PatternDepthIndex, Datum>> map =
		new HashMap<Symbol, Map<PatternDepthIndex, Datum>>();
	
	private Map<Symbol, Map<PatternDepthIndex, Integer>> repeats =
		new HashMap<Symbol, Map<PatternDepthIndex, Integer>>();
	
	private Set<Symbol> paramSet;
	
	
	/*package*/ PatternDepthMap(Set<Symbol> paramSet) {
		this.paramSet = paramSet;
	}
	
	/*package*/ PatternDepthMap() {
		this(new HashSet<Symbol>());
	}
	
	
	/*package*/ void put(Symbol sym, PatternDepthIndex index, Datum d) {
		Integer dp = depthMap.get(sym);
		Map<PatternDepthIndex, Datum> dm = map.get(sym);
		
		// 深さを保存する
		if(dp == null) {
			depthMap.put(sym, index.depth());
		} else if(dp.intValue() != index.depth()) {
			throw new LispException("not the same depth");
		}
		
		// データを保存する
		if(dm == null) {
			dm = new HashMap<PatternDepthIndex, Datum>();
			map.put(sym, dm);
		}
		
		//PatternMatch.markReplace(d);
		dm.put(index, d);
	}
	
	
	/*package*/ Datum get(
			Symbol sym,
			PatternDepthIndex index) throws PatternDepthException {
		Integer dp = depthMap.get(sym);
		Map<PatternDepthIndex, Datum> dm = map.get(sym);
		
		// 深さをチェックする
		if(dp == null) {
			return null;
		} else if(dp.intValue() != index.depth()) {
			throw new PatternDepthException(sym.getName());
		}
		
		//
		if(dm == null) {
			return null;
		}
		
		return dm.get(index);
		//// 置き換えからの保護のためにラッパーにくるむ
		//return new ReplacementWrapper(dm.get(index));
	}
	
	
	/*package*/ boolean contains(Symbol sym) {
		// パラメータリストに含まれるか(0回マッチ含む)
		return paramSet.contains(sym);
	}
	
	
	/*package*/ boolean hasElement(Symbol sym) {
		// 1回以上マッチしたか
		return depthMap.containsKey(sym);
	}
	
	
	/*package*/ void setRepetaion(
			Set<Symbol> col, PatternDepthIndex index, int rep) {
		//System.out.println("saaa:" + col + ":" + index + ":" + rep);
		for(Symbol i : col) {
			Map<PatternDepthIndex, Integer> map;
			
			map = repeats.get(i);
			if(map == null) {
				map = new HashMap<PatternDepthIndex, Integer>();
				repeats.put(i, map);
			}
			
			map.put(index, rep);
		}
	}
	
	
	/*package*/ int getRepetaion(
			Set<Symbol> col, PatternDepthIndex index) {
		int res = Integer.MAX_VALUE;
		
		for(Symbol i : col) {
			Map<PatternDepthIndex, Integer> map;
			
			map = repeats.get(i);
			if(map != null) {
				Integer r = map.get(index);
				
				if(r != null) {
					res = (r.intValue() < res) ? r.intValue() : res;
				}
			}
		}
		return (res == Integer.MAX_VALUE) ? 0 : res;
	}
	
	
	public String toString() {
		return depthMap.toString() + "\n" + map.toString();
	}
	
}
