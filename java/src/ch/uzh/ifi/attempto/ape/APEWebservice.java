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

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

/**
 * This class provides an interface to the Attempto Parsing Engine (APE) webservice
 * (i.e. HTTP server).
 * The HTTP server implementation is provided by <code>ape.exe</code>. To start
 * a server, execute for example:
 * <p/>
 * <pre>
 * ./ape.exe -httpserver -port 8000
 * </pre>
 *
 * @author Kaarel Kaljurand
 * @author Tobias Kuhn
 */
public class APEWebservice extends ACEParser {

    private static final int MAX_HTTP_GET_LENGTH = 1000;
    private static final String ERROR_MESSAGE = "Accessing APE webservice failed";

    private String wsUrl;
    private HttpClientBuilder httpClientBuilder = HttpClientBuilder.create();

    /**
     * Creates a new parser object based on the URL of a running APE webservice.
     *
     * @param wsUrl The URL of the APE webservice.
     */
    public APEWebservice(String wsUrl) {
        this.wsUrl = wsUrl;
    }

    /**
     * Sets the URL of the APE webservice to be used.
     *
     * @param wsUrl The URL of the APE webservice.
     */
    public void setWebserviceUrl(String wsUrl) {
        if (wsUrl == null) {
            throw new NullPointerException("APE webservice URL cannot be null");
        }
        this.wsUrl = wsUrl;
    }

    public String getSoloOutput(String aceText, Lexicon lexicon, OutputType outputType) throws ACEParserException {
        List<NameValuePair> nvps = prepareRequestParameters(aceText, lexicon);
        nvps.add(getSoloParam(outputType));
        return checkForErrors(getResponseAsString(nvps));
    }


    public ACEParserResult getMultiOutput(String aceText, Lexicon lexicon, OutputType... outputTypes) {
        List<NameValuePair> nvps = prepareRequestParameters(aceText, lexicon);
        for (OutputType t : outputTypes) {
            nvps.add(new BasicNameValuePair(t.toMultiFlag(), "on"));
        }
        return new ACEParserResult(getResponseAsString(nvps));
    }

    private List<NameValuePair> prepareRequestParameters(String aceText, Lexicon lexicon) {
        List<NameValuePair> nvps = new ArrayList<NameValuePair>();
        nvps.add(new BasicNameValuePair("text", aceText));

        if (lexicon != null) {
            nvps.add(new BasicNameValuePair("ulextext", lexicon.toString()));
        }

        String uri = getURI();
        if (uri != null) {
            nvps.add(new BasicNameValuePair("uri", uri));
        }
        nvps.add(getNamedBooleanParameter("guess", isGuessingEnabled()));
        nvps.add(getNamedBooleanParameter("noclex", !isClexEnabled()));
        return nvps;
    }

    private NameValuePair getSoloParam(OutputType solo) {
        return new BasicNameValuePair("solo", solo.toSoloFlag());
    }

    private BasicNameValuePair getNamedBooleanParameter(String key, boolean value) {
        return new BasicNameValuePair(key, value ? "on" : "off");
    }

    private String getResponseAsString(List<NameValuePair> nvps) {
        HttpClient client = httpClientBuilder.build();
        HttpUriRequest request = getHttpUriRequest(nvps);
        return getEntity(client, request);
    }

    /**
     * We create an HTTP GET query from the given parameters. If it turns out to be
     * too long (which we expect to happen very infrequently) then we fall back to creating
     * HTTP POST.
     * <p/>
     * BUG: Old versions of the SWI-Prolog HTTP server do not handle POST queries correctly.
     * Hopefully this has been fixed in v5.11.9 (but I haven't tested it).
     * See the bug report in the SWI-Prolog mailing list in 2010-11-03.
     *
     * @param nvps List of name-value pairs
     * @return HTTP request (either GET or POST)
     */
    private HttpUriRequest getHttpUriRequest(List<NameValuePair> nvps) {
        String getQuery = wsUrl + "?" + URLEncodedUtils.format(nvps, HTTP.UTF_8);
        if (getQuery.length() > MAX_HTTP_GET_LENGTH) {
            HttpPost httppost = new HttpPost(wsUrl);
            try {
                httppost.setEntity(new UrlEncodedFormEntity(nvps, HTTP.UTF_8));
            } catch (UnsupportedEncodingException e) {
                // BUG: Assuming that this cannot happen
            }
            return httppost;
        }
        return new HttpGet(getQuery);
    }

    private String getEntity(HttpClient httpclient, HttpUriRequest httpRequest) {
        try {
            HttpResponse response = httpclient.execute(httpRequest);
            HttpEntity entity = response.getEntity();

            if (entity == null) {
                throw new RuntimeException(ERROR_MESSAGE + ": " + response.getStatusLine());
            }
            int statusCode = response.getStatusLine().getStatusCode();
            if (statusCode != HttpStatus.SC_OK) {
                throw new RuntimeException(ERROR_MESSAGE + ": " + response.getStatusLine());
            }
            // The APE webservice returns the data in UTF8, even if it doesn't declare it.
            if (entity.getContentEncoding() == null) {
                return EntityUtils.toString(entity, HTTP.UTF_8);
            }
            return EntityUtils.toString(entity);

        } catch (ClientProtocolException e) {
            throw new RuntimeException(ERROR_MESSAGE + ": " + e.getMessage());
        } catch (IOException e) {
            throw new RuntimeException(ERROR_MESSAGE + ": " + e.getMessage());
        } finally {
            httpclient.getConnectionManager().shutdown();
        }
    }
}
