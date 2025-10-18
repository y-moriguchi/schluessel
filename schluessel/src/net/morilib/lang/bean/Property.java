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

import java.lang.annotation.Annotation;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/09
 */
public abstract class Property {
	
	
	public abstract Object get(Object bean);
	
	
	public abstract void set(Object bean, Object value);
	
	
	public abstract void setAccessible(boolean b);
	
	
	public abstract boolean isAnnotationPresent(
			Class<? extends Annotation> annotationClass);
	
	
	public abstract <T extends Annotation> T getAnnotation(
			Class<T> annotationClass);
	
	
	public abstract Annotation[] getAnnotations();

	/**
	 * @return
	 */
	public abstract String getName();

	/**
	 * @return
	 */
	public abstract Class<?> getType();
	
}
