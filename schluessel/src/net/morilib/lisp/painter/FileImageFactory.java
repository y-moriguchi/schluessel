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
package net.morilib.lisp.painter;

import java.awt.Image;

import javax.swing.ImageIcon;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/12
 */
public class FileImageFactory implements ImageFactory {

	//
	private String filename;

	public FileImageFactory(String filename) {
		this.filename = filename;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.ImageFactory#getImage()
	 */
	public Image getImage() {
		return new ImageIcon(filename).getImage();
//		try {
//			return ImageIO.read(new File(filename));
//		} catch (IOException e) {
//			throw new LispIOException(e);
//		}
	}

}
