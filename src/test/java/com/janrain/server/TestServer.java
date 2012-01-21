package com.janrain.server;


import com.janrain.backplane.server.*;
import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.backplane.server.config.Client;
import com.janrain.backplane.server.config.User;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.crypto.ChannelUtil;
import com.sun.org.apache.bcel.internal.generic.NEW;
import org.apache.catalina.util.Base64;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.internal.runners.JUnit38ClassRunner;
import org.junit.runner.RunWith;
import org.springframework.context.ApplicationContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.servlet.HandlerAdapter;

import javax.inject.Inject;
import javax.mail.Message;
import javax.servlet.http.HttpServletResponse;

import java.util.*;

import static org.junit.Assert.*;

/**
 * @author Tom Raney
 */

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { "classpath:/spring/app-config.xml", "classpath:/spring/mvc-config.xml" })
public class TestServer {

    @Inject
	private ApplicationContext applicationContext;

    @Inject
	private BackplaneController controller;

    @Inject
    private SuperSimpleDB superSimpleDB;

    @Inject
    private BackplaneConfig bpConfig;

    private static final Logger logger = Logger.getLogger(TestServer.class);

    ArrayList<String> createdMessageKeys = new ArrayList<String>();
    ArrayList<String> createdTokenKeys = new ArrayList<String>();
    ArrayList<String> createdCodeKeys = new ArrayList<String>();

    static final String OK_RESPONSE = "{\"stat\":\"ok\"}";
    static final String ERR_RESPONSE = "\"error\":";

    static final String TEST_MSG =
            "    {\n" +
            "        \"bus\": \"mybus.com\",\n" +
            "        \"channel\": \"testchannel\",\n" +
            "        \"source\": \"ftp://bla_source/\",\n" +
            "        \"type\": \"bla_type\",\n" +
            "        \"sticky\": \"false\",\n" +
            "        \"payload\":{\n" +
            "            \"identities\":{\n" +
            "               \"startIndex\":0,\n" +
            "               \"itemsPerPage\":1,\n" +
            "               \"totalResults\":1,\n" +
            "               \"entry\":{\n" +
            "                  \"displayName\":\"inewton\",\n" +
            "                  \"accounts\":[\n" +
            "                     {\n" +
            "                        \"username\":\"inewton\",\n" +
            "                        \"openid\":\"https://www.google.com/profiles/105119525695492353427\"\n" +
            "                     }\n" +
            "                  ],\n" +
            "                  \"id\":\"1\"\n" +
            "               }\n" +
            "            },\n" +
            "            \"context\":\"http://backplane1-2.janraindemo.com/token.html\"\n" +
            "         }" +
            "    }";

    private MockHttpServletRequest request;
	private MockHttpServletResponse response;
    private HandlerAdapter handlerAdapter;

    /**
	 * Initialize before every individual test method
	 */
	@Before
	public void init() {
        assertNotNull(applicationContext);
        handlerAdapter = applicationContext.getBean("handlerAdapter", HandlerAdapter.class);
		refreshRequestAndResponse();
	}

    @After
    public void cleanup() {
        logger.info("Tearing down test writes to db");
        try {
            for (String key:this.createdMessageKeys) {
                superSimpleDB.delete(bpConfig.getMessagesTableName(), key);
            }
            for (String key:this.createdTokenKeys) {
                superSimpleDB.delete(bpConfig.getAccessTokenTableName(), key);
            }
            for (String key:this.createdCodeKeys) {
                superSimpleDB.delete(bpConfig.getCodeTableName(), key);
            }
        } catch (SimpleDBException e) {
            logger.error(e);
        }
    }




    private Client createTestClient() throws SimpleDBException {
        Client client = new Client("random_id", "secret", "redirect_uri");
        superSimpleDB.store(bpConfig.getClientsTableName(), Client.class, client);
        return client;
    }


    private void refreshRequestAndResponse() {
		request = new MockHttpServletRequest();
		response = new MockHttpServletResponse();
	}

    private void saveMessage(BackplaneMessage message) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getMessagesTableName(), BackplaneMessage.class, message, true);
        this.createdMessageKeys.add(message.getIdValue());
    }

    private void saveCode(Code code) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getCodeTableName(), Code.class, code);
        this.createdTokenKeys.add(code.getIdValue());
    }

    private void saveToken(Token token) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getAccessTokenTableName(), Token.class, token);
        this.createdTokenKeys.add(token.getIdValue());
    }



    @Test
    public void testChannelGeneration() {
        String channel = ChannelUtil.randomString(1000);
        logger.debug(channel);
        assertTrue(Base64.isBase64(channel));
    }

    @Test()
    public void testTokenEndPointAnonymousWithClientSecret() throws Exception {
        //satisfy 13.1.1
        refreshRequestAndResponse();
        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", "anonymous");
        request.setParameter("grant_type", "client_credentials");
        //shouldn't contain the client_secret below
        request.setParameter("client_secret","meh");
        handlerAdapter.handle(request, response, controller);
        logger.debug("testTokenEndPointAnonymousWithClientSecret() => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));
    }


    @Test()
    public void testTokenEndPointAnonymousTokenRequest() throws Exception {
        //satisfy 13.1.1

        //  should return the form:
        //  {
        //      "access_token": "l5feG0KjdXTpgDAfOvN6pU6YWxNb7qyn",
        //      "expires_in":3600,
        //      "token_type": "Bearer",
        //      "backplane_channel": "Tm5FUzstWmUOdp0xU5UW83r2q9OXrrxt"
        // }

        refreshRequestAndResponse();

        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", "anonymous");
        request.setParameter("grant_type", "client_credentials");
        request.setParameter("client_secret","");

        handlerAdapter.handle(request, response, controller);
        logger.debug("testTokenEndPointAnonymousTokenRequest() => " + response.getContentAsString());
        //assertFalse(response.getContentAsString().contains(ERR_RESPONSE));

        assertTrue("Invalid response: " + response.getContentAsString(), response.getContentAsString().
                matches("[{]\\s*\"access_token\":\\s*\".{20}+\",\\s*" +
                        "\"expires_in\":\\s*3600,\\s*" +
                        "\"token_type\":\\s*\"Bearer\",\\s*" +
                        "\"backplane_channel\":\\s*\".{32}+\"\\s*[}]"));


    }

    @Test()
    public void testTokenEndPointClientTokenRequestInvalidCode() throws Exception {

        refreshRequestAndResponse();
        Client client = createTestClient();
        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", client.get(User.Field.USER));
        request.setParameter("grant_type", "code");

        //will fail because the code below is not valid
        request.setParameter("code", "meh");
        request.setParameter("client_secret", client.get(User.Field.PWDHASH));
        request.setParameter("redirect_uri", client.get(Client.ClientField.REDIRECT_URI));
        handlerAdapter.handle(request, response, controller);
        logger.debug("testTokenEndPointClientTokenRequestInvalidCode() => " + request.toString() + " => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));

    }


    @Test()
    public void testTokenEndPointClientTokenRequest() throws Exception {

        //  should return the form:
        //  {
        //      "access_token":"l5feG0KjdXTpgDAfOvN6pU6YWxNb7qyn",
        //      "token_type":"Bearer"
        //  }

        refreshRequestAndResponse();
        Client client = createTestClient();

        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", client.get(User.Field.USER));
        request.setParameter("grant_type", "code");

        //create code for test
        Code code = new Code(client.getClientId(),"test");
        this.saveCode(code);

        request.setParameter("code", code.getIdValue());
        request.setParameter("client_secret", client.get(User.Field.PWDHASH));
        request.setParameter("redirect_uri", client.get(Client.ClientField.REDIRECT_URI));
        handlerAdapter.handle(request, response, controller);
        logger.debug("testTokenEndPointClientTokenRequest() => " + response.getContentAsString());
        //assertFalse(response.getContentAsString().contains(ERR_RESPONSE));

        assertTrue("Invalid response: " + response.getContentAsString(), response.getContentAsString().
                matches("[{]\\s*\"access_token\":\\s*\".{20}+\",\\s*" +
                        "\"token_type\":\\s*\"Bearer\"\\s*[}]"));


    }

    @Test()
    public void TryToUseMalformedScopeTest() throws Exception {

        refreshRequestAndResponse();
        Client client = createTestClient();

        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", client.get(User.Field.USER));
        request.setParameter("grant_type", "code");

        //create code for test
        Code code = new Code(client.getClientId(),"test");
        this.saveCode(code);

        request.setParameter("code", code.getIdValue());
        request.setParameter("client_secret", client.get(User.Field.PWDHASH));
        request.setParameter("redirect_uri", client.get(Client.ClientField.REDIRECT_URI));
        request.setParameter("scope", "bus;mybus.com bus:yourbus.com");
        handlerAdapter.handle(request, response, controller);
        logger.debug("TryToUseMalformedScopeTest() => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));

        // try again with anonymous access with privileged use of payload
        request.setParameter("client_id", Token.ANONYMOUS);
        request.setParameter("client_secret", "");
        request.setParameter("scope", "payload.blah.blah");
        handlerAdapter.handle(request, response, controller);
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));
        logger.debug("TryToUseMalformedScopetest() => " + response.getContentAsString());

    }

    @Test()
    public void TryToUseInvalidScopeTest() throws Exception {

        refreshRequestAndResponse();
        Client client = createTestClient();

        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", client.get(User.Field.USER));
        request.setParameter("grant_type", "code");

        //create code for test
        Code code = new Code(client.getClientId(),"mybus.com");
        this.saveCode(code);

        request.setParameter("code", code.getIdValue());
        request.setParameter("client_secret", client.get(User.Field.PWDHASH));
        request.setParameter("redirect_uri", client.get(Client.ClientField.REDIRECT_URI));
        request.setParameter("scope", "bus:mybus.com bus:yourbus.com");
        handlerAdapter.handle(request, response, controller);
        logger.debug("TryToUseInvalidScopeTest() => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));
    }



    @Test()
    public void TryToUseExpiredCode() throws Exception {
        refreshRequestAndResponse();
        Client client = createTestClient();

        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", client.get(User.Field.USER));
        request.setParameter("grant_type", "code");

        //create expired code for test
        Code code = new Code(client.getClientId(),"test",new Date());
        this.saveCode(code);

        request.setParameter("code", code.getIdValue());
        request.setParameter("client_secret", client.get(User.Field.PWDHASH));
        request.setParameter("redirect_uri", client.get(Client.ClientField.REDIRECT_URI));
        handlerAdapter.handle(request, response, controller);
        logger.debug("TryToUseExpiredCode() => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));
    }

    @Test()
    public void testTokenEndPointNoURI() throws Exception {
        refreshRequestAndResponse();

        Client client = createTestClient();
        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", "meh");
        request.setParameter("grant_type", "code");

        //create code for test
        Code code = new Code(client.getClientId(),"test");
        this.saveCode(code);

        request.setParameter("code", code.getIdValue());

        //will fail because no redirect_uri value is included
        request.setParameter("redirect_uri","");
        handlerAdapter.handle(request, response, controller);
        logger.debug("testTokenEndPointNoURI() => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));
    }

    @Test()
    public void testTokenEndPointNoClientSecret() throws Exception {
        refreshRequestAndResponse();
        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", "meh");
        request.setParameter("grant_type", "client_credentials");
        //will fail because no client_secret is included
        request.setParameter("client_secret","");
        handlerAdapter.handle(request, response, controller);
        logger.debug("testTokenEndPointNoClientSecret() => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));
    }


    @Test()
    public void testTokenEndPointEmptyCode() throws Exception {
        refreshRequestAndResponse();
        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", "meh");
        request.setParameter("grant_type", "code");
        //will fail because no code value is included
        request.setParameter("code","");
        request.setParameter("redirect_uri","meh");
        handlerAdapter.handle(request, response, controller);
        logger.debug("testTokenEndPointEmptyCode() => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));
    }

    @Test()
    public void testTokenEndPointBadGrantType() throws Exception {
        refreshRequestAndResponse();
        request.setRequestURI("/token");
        request.setMethod("POST");
        request.setParameter("client_id", "meh");
        //will fail because bad grant type included
        request.setParameter("grant_type", "unexpected_value");
        handlerAdapter.handle(request, response, controller);
        logger.debug("testTokenEndPointBadGrantType() => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));
    }

    @Test()
    public void testTokenEndPointNoParams() throws Exception {
        // test empty parameters submitted to the token endpoint
        refreshRequestAndResponse();
        request.setRequestURI("/token");
        request.setMethod("POST");
        handlerAdapter.handle(request, response, controller);
        logger.debug("testTokenEndPointNoParams() => " + response.getContentAsString());

        assertTrue(response.getContentAsString().contains(ERR_RESPONSE));
        assertTrue(response.getStatus() == HttpServletResponse.SC_BAD_REQUEST);
    }

    @Test()
    public void testMessageEndPoint() throws Exception {

        refreshRequestAndResponse();

        // Create appropriate token
        Token token = new Token(Token.TYPE.REGULAR_TOKEN, "mybus.com", null, new Date(new Date().getTime() + Token.EXPIRES_SECONDS * 1000));
        this.saveToken(token);

        // Seed message
        ObjectMapper mapper = new ObjectMapper();
        Map<String,Object> msg = mapper.readValue(TEST_MSG, new TypeReference<Map<String,Object>>() {});

        BackplaneMessage message = new BackplaneMessage("123456", "mybus.com", token.getChannelName(), msg);
        this.saveMessage(message);

        // Make the call
        request.setRequestURI("/message/123456");
        request.setMethod("GET");
        request.setParameter("access_token", token.getIdValue());
        handlerAdapter.handle(request, response, controller);
        logger.debug("testMessageEndPoint()  => " + response.getContentAsString());
       // assertFalse(response.getContentAsString().contains(ERR_RESPONSE));

        assertTrue(response.getStatus() == HttpServletResponse.SC_OK);
        assertTrue(response.getContentType().equals("application/json"));

        // {
        //  "messageURL": "https://bp.example.com/v2/message/097a5cc401001f95b45d37aca32a3bd2",
        //  "source": "http://aboutecho.com",
        //  "type": "identity/ack"
        //  "bus": "customer.com",
        //  "channel": "67dc880cc265b0dbc755ea959b257118"
        //}

        assertTrue("Invalid response: " + response.getContentAsString(), response.getContentAsString().
                matches("[{]\\s*" +
                        "\"messageURL\":\\s*\".*\",\\s*" +
                        "\"source\":\\s*\".*\",\\s*" +
                        "\"type\":\\s*\".*\",\\s*" +
                        "\"bus\":\\s*\".*\",\\s*" +
                        "\"channel\":\\s*\".*\"\\s*" +
                        "[}]"));

    }

    @Test
    public void testMessageEndPointWithCallBack() throws Exception {

        String callbackName = "meh";

        refreshRequestAndResponse();

        // Create appropriate token
        Token token = new Token(Token.TYPE.REGULAR_TOKEN, "mybus.com", null, new Date(new Date().getTime() + Token.EXPIRES_SECONDS * 1000));
        this.saveToken(token);

        // Seed message
        ObjectMapper mapper = new ObjectMapper();
        Map<String,Object> msg = mapper.readValue(TEST_MSG, new TypeReference<Map<String,Object>>() {});

        BackplaneMessage message = new BackplaneMessage("123456", "mybus.com", token.getChannelName(), msg);
        this.saveMessage(message);

        // now, try it via callback
        refreshRequestAndResponse();
        request.setRequestURI("/message/123456");
        request.setMethod("GET");
        request.setParameter("access_token", token.getIdValue());
        request.setParameter("callback", callbackName);
        handlerAdapter.handle(request, response, controller);
        logger.debug("testMessageEndPointWithCallBack()  => " + response.getContentAsString());

        assertTrue(response.getStatus() == HttpServletResponse.SC_OK);
        assertTrue(response.getContentType().equals("application/x-javascript"));

        // callback({
        //  "messageURL": "https://bp.example.com/v2/message/097a5cc401001f95b45d37aca32a3bd2",
        //  "source": "http://aboutecho.com",
        //  "type": "identity/ack"
        //  "bus": "customer.com",
        //  "channel": "67dc880cc265b0dbc755ea959b257118"
        // })

        assertTrue("Invalid response: " + response.getContentAsString(), response.getContentAsString().
                matches(callbackName + "[(][{]\\s*" +
                        "\"messageURL\":\\s*\".*\",\\s*" +
                        "\"source\":\\s*\".*\",\\s*" +
                        "\"type\":\\s*\".*\",\\s*" +
                        "\"bus\":\\s*\".*\",\\s*" +
                        "\"channel\":\\s*\".*\"\\s*" +
                        "[}][)]"));


    }

    @Test()
    public void testMessageEndPointPAL() throws Exception {

        refreshRequestAndResponse();

        // Create appropriate token
        Token token = new Token(Token.TYPE.PRIVILEGED_TOKEN, "mybus.com", null, null);
        this.saveToken(token);

        // Seed message
        ObjectMapper mapper = new ObjectMapper();
        Map<String,Object> msg = mapper.readValue(TEST_MSG, new TypeReference<Map<String,Object>>() {});

        BackplaneMessage message = new BackplaneMessage("123456", "mybus.com", "randomchannel", msg);
        this.saveMessage(message);

        // Make the call
        request.setRequestURI("/message/123456");
        request.setMethod("GET");
        request.setParameter("access_token", token.getIdValue());
        handlerAdapter.handle(request, response, controller);
        logger.debug("testMessageEndPointPAL()   => " + response.getContentAsString());
       // assertFalse(response.getContentAsString().contains(ERR_RESPONSE));

        assertTrue(response.getStatus() == HttpServletResponse.SC_OK);
        assertTrue(response.getContentType().equals("application/json"));

        // {
        //  "messageURL": "https://bp.example.com/v2/message/097a5cc401001f95b45d37aca32a3bd2",
        //  "source": "http://aboutecho.com",
        //  "type": "identity/ack"
        //  "bus": "customer.com",
        //  "channel": "67dc880cc265b0dbc755ea959b257118",
        //  "payload": {
        //      "role": "administrator"
        //  },
        //}

        assertTrue("Invalid response: " + response.getContentAsString(), response.getContentAsString().
                matches("[{]\\s*" +
                        "\"messageURL\":\\s*\".*\",\\s*" +
                        "\"source\":\\s*\".*\",\\s*" +
                        "\"type\":\\s*\".*\",\\s*" +
                        "\"bus\":\\s*\".*\",\\s*" +
                        "\"channel\":\\s*\".*\",\\s*" +
                        "\"payload\":\\s*.*" +
                        "[}]"));


    }

    @Test
    public void testMessagesEndPointPAL() throws Exception {

        refreshRequestAndResponse();

        // Create appropriate token
        Token token = new Token(Token.TYPE.PRIVILEGED_TOKEN, "mybus.com yourbus.com", "bus:mybus.com", null);
        this.saveToken(token);

        // Seed 2 messages
        ObjectMapper mapper = new ObjectMapper();
        Map<String,Object> msg = mapper.readValue(TEST_MSG, new TypeReference<Map<String,Object>>() {});

        BackplaneMessage message1 = new BackplaneMessage("123456", "mybus.com", "randomchannel", msg);
        this.saveMessage(message1);

        BackplaneMessage message2 = new BackplaneMessage("1234567", "yourbus.com", "randomchannel", msg);
        this.saveMessage(message2);

         // Make the call
        request.setRequestURI("/messages");
        request.setMethod("GET");
        request.setParameter("access_token", token.getIdValue());
        handlerAdapter.handle(request, response, controller);
        logger.debug("testMessagesEndPointPAL()   => " + response.getContentAsString());

        assertFalse(response.getContentAsString().contains(ERR_RESPONSE));
    }

    @Test
    public void testMessagesEndPointRegular() throws Exception {

        logger.info("TEST: testMessagesEndPointRegular() =================");

        refreshRequestAndResponse();

        // Create appropriate token
        Token token = new Token(Token.TYPE.REGULAR_TOKEN, null, null, new Date(new Date().getTime() + Token.EXPIRES_SECONDS * 1000));
        this.saveToken(token);

        // Seed 2 messages
        ObjectMapper mapper = new ObjectMapper();
        Map<String,Object> msg = mapper.readValue(TEST_MSG, new TypeReference<Map<String,Object>>() {});

        BackplaneMessage message1 = new BackplaneMessage(BackplaneMessage.generateMessageId(), "mybus.com", token.getChannelName(), msg);
        this.saveMessage(message1);

        BackplaneMessage message2 = new BackplaneMessage(BackplaneMessage.generateMessageId(), "yourbus.com", token.getChannelName(), msg);
        this.saveMessage(message2);

         // Make the call
        request.setRequestURI("/messages");
        request.setMethod("GET");
        request.setParameter("access_token", token.getIdValue());
        request.setParameter("since", message1.getIdValue());
        handlerAdapter.handle(request, response, controller);
        logger.debug("testMessagesEndPointRegular() => " + response.getContentAsString());

        assertFalse(response.getContentAsString().contains(ERR_RESPONSE));

        // should just receive one of the two messages
        Map<String,Object> returnedBody = mapper.readValue(response.getContentAsString(), new TypeReference<Map<String,Object>>() {});
        List<Map<String,Object>> returnedMsgs = (List<Map<String, Object>>) returnedBody.get("messages");
        assertTrue(returnedMsgs.size() == 1);

        logger.info("========================================================");

    }



    @Test
    public void testMessagesEndPointPALInvalidScope() throws Exception {

        refreshRequestAndResponse();

        // Create inappropriate token
        try {
            Token token = new Token(Token.TYPE.PRIVILEGED_TOKEN, "mybus.com yourbus.com", "bus:invalidbus.com", null);
        } catch (BackplaneServerException bpe) {
            //expected
            return;
        }

        fail("Token requested with invalid scope should have failed");

    }

    @Test
    public void testMessagesPostEndPointPAL() throws Exception {

        refreshRequestAndResponse();

        // Create appropriate token
        Token token = new Token(Token.TYPE.PRIVILEGED_TOKEN, "mybus.com yourbus.com", "bus:yourbus.com", null);
        this.saveToken(token);

        // Make the call
        request.setRequestURI("/messages");
        request.setMethod("POST");
        request.setParameter("access_token", token.getIdValue());
        request.addHeader("Content-type", "application/json");
        //request.setContentType("application/json");
        //request.setParameter("messages", TEST_MSG);
        HashMap<String, Object> msgs = new HashMap<String, Object>();
        ArrayList msgsList = new ArrayList();
        msgsList.add(new ObjectMapper().readValue(TEST_MSG, new TypeReference<Map<String,Object>>() {}));
        msgsList.add(new ObjectMapper().readValue(TEST_MSG, new TypeReference<Map<String,Object>>() {}));

        msgs.put("messages", msgsList);
        String msgsString = new ObjectMapper().writeValueAsString(msgs);
        logger.info(msgsString);
        request.setContent(msgsString.getBytes());

        handlerAdapter.handle(request, response, controller);

        assertTrue(response.getStatus() == HttpServletResponse.SC_CREATED);

    }
















}
