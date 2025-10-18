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
package net.morilib.lisp.swing;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;

import javax.swing.JComboBox;
import javax.swing.JComponent;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.lisp.swing.listener.ActionListenable;
import net.morilib.lisp.swing.listener.ItemListenable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public class LispComboBox extends LightweightGUIElement
implements Selectable, ActionListenable, ItemListenable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class MakeComboBox extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			JComboBox c;

			c = new JComboBox(LispSwing.toItem(c1a));
			c.setEditable(true);
			return new LispComboBox(c);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class MakeListBox extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			JComboBox c;

			c = new JComboBox(LispSwing.toItem(c1a));
			c.setEditable(false);
			return new LispComboBox(c);
		}

	}

	//
	private JComboBox combo;

	/**
	 * 
	 * @param combo
	 */
	public LispComboBox(JComboBox combo) {
		this.combo = combo;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JComponent getComponent() {
		return combo;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.Selectable#getCurrentItem()
	 */
	public Item getCurrentItem() {
		return (Item)combo.getSelectedItem();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.Selectable#getItems()
	 */
	public Item[] getItems() {
		Item[] res = new Item[combo.getItemCount()];

		for(int i = 0; i < res.length; i++) {
			res[i] = (Item)combo.getItemAt(i);
		}
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.Selectable#setItems(net.morilib.lisp.Datum[])
	 */
	public void setItems(Item... data) {
		combo.removeAllItems();
		for(int i = 0; i < data.length; i++) {
			combo.addItem(data);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ActionListenable#addActionListener(java.awt.event.ActionListener)
	 */
	public void addActionListener(ActionListener listener) {
		combo.addActionListener(listener);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ItemListenable#addItemListener(java.awt.event.ItemListener)
	 */
	public void addItemListener(ItemListener l) {
		combo.addItemListener(l);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return getComponent();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<combobox>");
	}

}
