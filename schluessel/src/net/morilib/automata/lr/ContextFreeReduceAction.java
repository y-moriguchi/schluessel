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

/**
 * 文脈自由言語の規則が還元されたときに実行されるActionの
 * インターフェースである。
 * 
 * @author MORIGUCHI, Yuichiro 2006/07/09
 */
public interface ContextFreeReduceAction<T> {
	
	/**
	 * 規則が還元されたときに実行されるActionである。
	 * 
	 * @param event 還元された規則
	 * @param attrs 還元されたときの意味属性
	 */
	public T action(
			ContextFreeRule event,
			SemanticAttributes<T> attrs);

}
