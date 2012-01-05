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

import java.io.StringReader;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;

/**
 * This class represents the output that is returned by the ACEParser class.
 * 
 * @author Tobias Kuhn
 */
public class ACEParserResult {

	private Element result;
	private MessageContainer messageContainer = new MessageContainer();


	ACEParserResult(String xmlString) {
		try {
			SAXBuilder sb = new SAXBuilder();
			sb.setValidation(false);
			Document xml = sb.build(new StringReader(xmlString));
			result = xml.getRootElement();
			messageContainer = new MessageContainer(result.getChild("messages"));
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	/**
	 * Returns the specified output as a string. If the requested output is not available (because
	 * it has not been requested) then null is returned.
	 * 
	 * @param outputType The output to be returned.
	 * @return The output as a string.
	 */
	public String get(OutputType outputType) {
		try {
			Element element = result.getChild(outputType.toString().toLowerCase());
			return element.getText();
		} catch (Exception ex) {
			return null;
		}
	}

	/**
	 * Returns the message container that contains the error and warning messages of the parsing
	 * procedure.
	 * 
	 * @return The message container.
	 */
	public MessageContainer getMessageContainer() {
		return messageContainer;
	}

}
