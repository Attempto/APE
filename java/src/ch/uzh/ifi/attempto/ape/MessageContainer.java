// This file is part of the Attempto Parsing Engine (APE).
// Copyright 2008-2012, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later version.
//
// The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.ape;

import java.util.ArrayList;
import java.util.List;

import org.jdom.Element;

/**
 * This class is a container for error and warning messages.
 * 
 * @author Tobias Kuhn
 */
public class MessageContainer {
	
	private List<Message> messages = new ArrayList<Message>();
	
	MessageContainer() {
	}
	
	MessageContainer(Element xmlElement) {
		if (xmlElement == null) return;
		for (Object o : xmlElement.getChildren("message")) {
			if (!(o instanceof Element)) continue;
			messages.add(new Message((Element) o));
		}
	}
	
	/**
	 * Returns a list that contains all messages.
	 * 
	 * @return A list of all messages.
	 */
	public List<Message> getMessages() {
		return new ArrayList<Message>(messages);
	}
	
	/**
	 * Returns a list that contains all error messages.
	 * 
	 * @return A list of all error messages.
	 */
	public List<Message> getErrorMessages() {
		List<Message> list = new ArrayList<Message>();
		for (Message m : messages) {
			if (m.isError()) list.add(m);
		}
		return list;
	}
	
	/**
	 * Returns a list that contains all warning messages.
	 * 
	 * @return A list of all warning messages.
	 */
	public List<Message> getWarningMessages() {
		List<Message> list = new ArrayList<Message>();
		for (Message m : messages) {
			if (!m.isError()) list.add(m);
		}
		return list;
	}
	
	/**
	 * Returns a list that contains all messages of a given type.
	 * 
	 * @param type The type of the messages to be returned.
	 * @return A list of all messages with the given type.
	 */
	public List<Message> getMessages(String type) {
		List<Message> list = new ArrayList<Message>();
		for (Message m : messages) {
			if (m.getType().equals(type)) list.add(m);
		}
		return list;
	}
	
	/**
	 * Returns a list that contains all error messages of a given type.
	 * 
	 * @param type The type of the error messages to be returned.
	 * @return A list of all error messages with the given type.
	 */
	public List<Message> getErrorMessages(String type) {
		List<Message> list = new ArrayList<Message>();
		for (Message m : messages) {
			if (m.isError() && m.getType().equals(type)) list.add(m);
		}
		return list;
	}
	
	/**
	 * Returns a list that contains all warning messages of a given type.
	 * 
	 * @param type The type of the warning message to be returned.
	 * @return A list of all warning messages with the given type.
	 */
	public List<Message> getWarningMessages(String type) {
		List<Message> list = new ArrayList<Message>();
		for (Message m : messages) {
			if (!m.isError() && m.getType().equals(type)) list.add(m);
		}
		return list;
	}
	
	public String toString() {
		String s = "";
		for (Message m : messages) {
			s += m;
		}
		return s;
	}

}
