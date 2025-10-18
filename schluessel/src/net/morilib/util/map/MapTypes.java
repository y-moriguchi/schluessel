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
package net.morilib.util.map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/11/13
 */
public final class MapTypes {
	
	//
	private MapTypes() { }
	
	
	public static Class<?> getKeyType(Class<?> o) {
		KeyTyped a;
		
		if(o == null) {
			throw new NullPointerException();
		}
		a = o.getAnnotation(KeyTyped.class);
		return (a == null) ? null : a.value();
	}
	
	
	public static Class<?> getKeyType(Object o) {
		if(o == null) {
			throw new NullPointerException();
		}
		return getKeyType(o.getClass());
	}
	
	
	public static Class<?> getValueType(Class<?> o) {
		ValueTyped a;
		
		if(o == null) {
			throw new NullPointerException();
		}
		a = o.getAnnotation(ValueTyped.class);
		return (a == null) ? null : a.value();
	}
	
	
	public static Class<?> getValueType(Object o) {
		if(o == null) {
			throw new NullPointerException();
		}
		return getValueType(o.getClass());
	}
	
}
