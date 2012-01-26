package com.janrain.backplane.server;

import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import junit.framework.TestCase;
import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.context.ApplicationContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.servlet.HandlerAdapter;

import javax.inject.Inject;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * @author Tom Raney
 */

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { "classpath:/spring/app-config.xml", "classpath:/spring/mvc-config.xml" })
public class Backplane1ControllerTest extends TestCase {

    private static final Logger logger = Logger.getLogger(Backplane1ControllerTest.class);

    @Inject
    private Backplane1Config bpConfig;

    @Inject
	private ApplicationContext applicationContext;

    @Inject
    private SuperSimpleDB superSimpleDB;

    private MockHttpServletRequest request;
    private MockHttpServletResponse response;
    private HandlerAdapter handlerAdapter;

    @Inject
	private Backplane1Controller controller;

    /**
	 * Initialize before every individual test method
	 */
	@Before
	public void init() {
        assertNotNull(applicationContext);
        handlerAdapter = applicationContext.getBean("handlerAdapter", HandlerAdapter.class);
		refreshRequestAndResponse();
	}


    private void refreshRequestAndResponse() {
        request = new MockHttpServletRequest();
        response = new MockHttpServletResponse();
    }

    @Test
    public void testGetChannel() throws Exception {

        refreshRequestAndResponse();
        request.setRequestURI("/bus/test/channel/test");
        request.setMethod("GET");

        handlerAdapter.handle(request, response, controller);
        logger.debug("testGetChannel() => " + response.getContentAsString());
        assertTrue(response.getContentAsString().contains("[]"));

    }
}
