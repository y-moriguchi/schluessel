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
package net.morilib.lisp.painter.drawer;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/28
 */
public class CompositeDrawer implements Drawer {

	/**
	 * 
	 */
	protected List<Drawer> drawers;

	/**
	 * 
	 * @param drawers
	 */
	public CompositeDrawer(Collection<Drawer> drawers) {
		this.drawers = new ArrayList<Drawer>(drawers);
	}

	/**
	 * 
	 * @param drawers
	 */
	public CompositeDrawer(Drawer... drawers) {
		this(Arrays.asList(drawers));
	}

	/**
	 * 
	 * @param drawer
	 */
	public synchronized void add(Drawer drawer) {
		drawers.add(drawer);
	}

	/**
	 * 
	 */
	public synchronized void clear() {
		drawers.clear();
	}

	/**
	 * 
	 */
	public synchronized void undo() {
		drawers.remove(drawers.size() - 1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.drawer.Drawer#draw(java.awt.Graphics, int, int, net.morilib.lisp.painter.drawer.CoordinateMap)
	 */
	@Override
	public void draw(Graphics g, int framex, int framey,
			CoordinateMap coordinate) {
		synchronized(this) {
			for(Drawer d : drawers) {
				d.draw(g, framex, framey, coordinate);
			}
		}
	}

}
