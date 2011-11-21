// This file is part of the Attempto Java Packages.
// Copyright 2008-2009, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Java Packages is free software: you can redistribute it and/or modify it under the
// terms of the GNU Lesser General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// The Attempto Java Packages is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Java Packages. If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.ape;

import org.jdom.Element;

/**
 * This exception is used by the ACE parser when used in solo mode. It contains the error
 * messages of the parser.
 * 
 * @author Tobias Kuhn
 */
public class ACEParserException extends Exception {
	
	private static final long serialVersionUID = 5759137566239371054L;
	
	private MessageContainer messageContainer;
	
	
	ACEParserException(Element xmlElement) {
		messageContainer = new MessageContainer(xmlElement);
	}
	
	/**
	 * Returns the message container with the error messages.
	 * 
	 * @return The message container.
	 */
	public MessageContainer getMessageContainer() {
		return messageContainer;
	}

}
