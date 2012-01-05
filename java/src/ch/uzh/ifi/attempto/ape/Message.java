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

import org.jdom.Element;

/**
 * This class represents a warning or error message of the ACE parser.
 * 
 * @author Tobias Kuhn
 * @author Kaarel Kaljurand
 */
public class Message {

	private boolean isError;
	private String type, value, repair;
	private Integer sentenceId, tokenId;

	/**
	 * Creates a new Message object. All the relevant information for the
	 * message must be passed as arguments to this constructor.
	 * 
	 * @param importance The importance of the message, e.g. "error", "warning"
	 * @param type The type of the message, e.g. "sentence", "token"
	 * @param sentenceId The number of the problematic sentence, or <code>null</code> if unknown
	 * @param tokenId The number of the problematic token, or <code>null</code> if unknown
	 * @param value The value of the message (e.g. a misspelled word)
	 * @param repair The description that helps to resolve the error or warning
	 */
	public Message(String importance, String type, Integer sentenceId, Integer tokenId, String value, String repair) {
		this.isError = importance.equals("error");
		this.type = type;
		this.sentenceId = sentenceId;
		this.tokenId = tokenId;
		this.value = value;
		this.repair = repair;
	}


	Message(Element xmlElement) {
		isError = (xmlElement.getAttribute("importance").getValue().equals("error"));
		type = xmlElement.getAttribute("type").getValue();
		value = xmlElement.getAttribute("value").getValue();
		repair = xmlElement.getAttribute("repair").getValue();
		try {
			sentenceId = new Integer(xmlElement.getAttribute("sentence").getValue());
		} catch (NumberFormatException ex) {}
		try {
			tokenId = new Integer(xmlElement.getAttribute("token").getValue());
		} catch (NumberFormatException ex) {}
	}

	/**
	 * Returns true if this message is an error message, or false if it is only a warning
	 * message.
	 * 
	 * @return true if this is an error message.
	 */
	public boolean isError() {
		return isError;
	}

	/**
	 * Returns the type of the error/warning message.
	 * 
	 * @return The type of the message.
	 */
	public String getType() {
		return type;
	}

	/**
	 * Returns the sentence number that locates the reason for the error/warning message.
	 * In some cases, this can be null.
	 * 
	 * @return The sentence number.
	 */
	public Integer getSentenceId() {
		return sentenceId;
	}

	/**
	 * Returns the token number that locates the reason for the error/warning message.
	 * In some cases, this can be null.
	 * 
	 * @return The token number.
	 */
	public Integer getTokenId() {
		return tokenId;
	}

	/**
	 * Returns the value of the error/warning message. This is for example the word that
	 * caused the error or warning.
	 * 
	 * @return The value of the message.
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Returns an suggestion how to react on the error or warning.
	 * 
	 * @return Suggestion how to react on the error/warning.
	 */
	public String getRepair() {
		return repair;
	}

	/**
	 * Returns a pretty-printed error/warning message.
	 */
	public String toString() {
		return (isError ? "ERROR: [" : "WARNING: [") + type + "] at " + (sentenceId == null ? "?" : sentenceId) + "-" + (tokenId == null ? "?" : tokenId) + ": '" + value + "'. " + repair;
	}
}