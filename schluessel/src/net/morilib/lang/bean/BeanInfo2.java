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
package net.morilib.lang.bean;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/09
 */
public abstract class BeanInfo2<T> {
	
	
	/*package*/ BeanInfo2() { }
	
	
	public static<T> BeanInfo2<T> getInfoClass(Class<T> t) {
		return new StaticBeanInfo<T>(t);
	}
	
	
	@SuppressWarnings("unchecked")
	public static<T> BeanInfo2<T> getInfo(T t) {
		return new StaticBeanInfo<T>((Class<T>)t.getClass());
	}
	
	
	public abstract Property getPropertyOrField(String name);
	
	
	public abstract Property[] getPropertiesAndFields();
	
	
	public abstract Property getProperty(String name);
	
	
	public abstract Property[] getProperties();
	
	
	public abstract T newInstance(Object... args);
	
	
	public abstract T forceNewInstance(Object... args);

}
