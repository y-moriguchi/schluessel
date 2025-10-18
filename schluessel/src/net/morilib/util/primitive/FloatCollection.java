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
package net.morilib.util.primitive;

import java.util.Collection;

import net.morilib.util.primitive.iterator.FloatIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public interface FloatCollection extends Collection<Float> {
	
	public boolean addFloat(float v);
	
	public boolean add(int v);
	
	public boolean addAllFloat(FloatCollection a);
	
	public boolean addAllFloat(FloatCollection... as);
	
	public boolean addAllFloat(
			Collection<? extends FloatCollection> as);
	
	public void clear();
	
	public boolean containsFloat(float v);
	
	public boolean contains(int v);
	
	public boolean containsAllFloat(FloatCollection a);
	
	public boolean isEmpty();
	
	public FloatIterator floatIterator();
	
	public boolean removeFloat(float v);
	
	public boolean removeElement(int v);
	
	public boolean removeAllFloat(FloatCollection a);
	
	public boolean retainAllFloat(FloatCollection a);
	
	public int size();
	
	public float[] toFloatArray();
	
	public float[] toFloatArray(float[] a);
	
	public boolean isInfinite();
	
	public FloatSet toSet();
	
}
