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
package net.morilib.automata.lr;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import net.morilib.lang.Hashes;
import net.morilib.util.Objects;

/**
 * 文脈自由言語の規則を規定するクラスである。
 * 
 * @author MORIGUCHI, Yuichiro 2006/06/23
 */
public class ContextFreeRule {
	
	//
	private Nonterminal left;
	private List<GrammarSymbol> derived;
	
	/**
	 * 文脈自由言語の規則を生成する。
	 * 
	 * @param left 左辺値の非終端記号
	 * @param derived 右辺値の記号列
	 */
	public ContextFreeRule(
			Nonterminal left, GrammarSymbol... derived) {
		if(derived == null) {
			throw new NullPointerException(
					"Argument 'derived' must not be null");
		}
		
		this.left    = left;
		this.derived = Arrays.asList(derived);
	}
	
	/**
	 * 規則の左辺値を取得する。
	 * 
	 * @return 規則の左辺値
	 */
	public Nonterminal getLeftSymbol() {
		return left;
	}
	
	/**
	 * 規則の右辺値の記号列を取得する。
	 * 
	 * @return 右辺値の記号列
	 */
	public List<GrammarSymbol> getDerivedSymbols() {
		return Collections.unmodifiableList(derived);
	}
	
	/**
	 * 規則の右辺値の記号列の長さを取得する。
	 * 
	 * @return 右辺値の記号列の長さ
	 */
	public int getDerivedSymbolLength() {
		return derived.size();
	}
	
	/**
	 * 右辺値の記号列において、指定されたインデックスの記号を取りだす。
	 * 
	 * @param no インデックス
	 * @return 指定されたインデックスの記号
	 */
	public GrammarSymbol getDerivedSymbol(int no) {
		return derived.get(no);
	}
	
	/**
	 * 右辺値の記号列が存在しない(epsilon遷移のとき)trueを得る。
	 * 
	 * @return 右辺値の記号列が存在しない(epsilon遷移のとき)true
	 */
	public boolean isEpsilon() {
		return derived.isEmpty();
	}
	
	/**
	 * 規則が等しいときにtrueを得る。
	 * 
	 * @param 調べるオブジェクト
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof ContextFreeRule) {
			ContextFreeRule rule = (ContextFreeRule)o;
			
			return (Objects.equals(left, rule.left) &&
					Objects.equals(derived, rule.derived));
		}
		return false;
	}
	
	/**
	 * ハッシュ値を得る。
	 * 
	 * @return ハッシュ値
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;
		r += Hashes.A * (Hashes.hashCode(left) + r);
		r += Hashes.A * (Hashes.hashCode(derived) + r);
		
		return r;
	}
	
	/**
	 * 文字列表現を得る。
	 * 
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer buf = new StringBuffer();
		
		buf.append(Objects.toString(left));
		buf.append(" -> ");
		for(int i = 0; i < derived.size(); i++) {
			buf.append(derived.get(i));
			buf.append(" ");
		}
		return buf.toString();
	}

}