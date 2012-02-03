/*
 * Copyright 2012 Janrain, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.janrain.simpledb;

import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang.RandomStringUtils;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import javax.inject.Inject;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * @author Johnny Bufu
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:/spring/app-config.xml", "classpath:/spring/mvc-config.xml"})
public class SuperSimpleDBTest {

    @Test
    public void testSimpleDbXmlReservedCharacters() throws Exception {
        testData = new TestNamedMap();
        testData.put("<", "1");
        testData.put(">", "2");
        testData.put("&", "3");
        testData.put("'", "4");
        testData.put("\"", "5");
        testData.put("6", "<");
        testData.put("7", ">");
        testData.put("8", "&");
        testData.put("9", "'");
        testData.put("10", "\"");
        superSimpleDB.store(TEST_TABLE, TestNamedMap.class, testData);
        TestNamedMap retrievedData = superSimpleDB.retrieve(TEST_TABLE, TestNamedMap.class, testData.getName());
        assertEquals(testData, retrievedData);
    }

    @Test
    public void testLongEntries() throws Exception {
        testData = new TestNamedMap();
        for(int i=0; i < 10; i++) {
            testData.put(RandomStringUtils.random(1100), RandomStringUtils.random(2000));
        }
        superSimpleDB.store(TEST_TABLE, TestNamedMap.class, testData, true);
        TestNamedMap retrievedData = superSimpleDB.retrieve(TEST_TABLE, TestNamedMap.class, testData.getName());
        assertEquals(testData, retrievedData);
    }

    @Test
    public void testDeleteWhereEncoded() throws Exception {
        testData = new TestNamedMap();
        testData.put("test", new String(Base64.decodeBase64(BPMSG.getBytes())));
        superSimpleDB.store(TEST_TABLE, TestNamedMap.class, testData);
        superSimpleDB.deleteWhere(TEST_TABLE, "test is not null");
    }

    @After
    public void tearDown() throws Exception {
        System.out.println("Tearing down " + TEST_TABLE);
        superSimpleDB.drop(TEST_TABLE);
    }

    // - PRIVATE

    @Inject
    private SuperSimpleDB superSimpleDB;

    private static final String TEST_TABLE = "test_" + SuperSimpleDB.class.getSimpleName();

    private TestNamedMap testData;

    private static final String BPMSG = "eyJjb250ZXh0IjoiaHR0cDovL3d3dy5taW5kbGVzc2JlaGF2aW9yLmNvbS9hc3BuZXRfY2xpZW50" +
            "L21pY3JvZ3Jvb3ZlL3IvY2FwdHVyZV9vYXV0aC5hc3B4P29yaWdpbj1odHRwJTNBJTJGJTJGd3d3" +
            "Lm1pbmRsZXNzYmVoYXZpb3IuY29tJTJGc2lnbmluJTJGJTIzc2lnbmluJmNhbGxiYWNrPWNhcHR1" +
            "cmUxIiwiaWRlbnRpdGllcyI6eyJzdGFydEluZGV4IjowLCJpdGVtc1BlclBhZ2UiOjEsInRvdGFs" +
            "UmVzdWx0cyI6MSwiZW50cnkiOnsiaWQiOiIxIiwiZGlzcGxheU5hbWUiOiIgREEgSElUTUFOwoAg" +
            "UkVBTCDCgE5UIMKkIiwiYWNjb3VudHMiOlt7InVzZXJuYW1lIjoiIERBIEhJVE1BTsKAIFJFQUwg" +
            "woBOVCDCpCIsImlkZW50aXR5VXJsIjoiaHR0cHM6Ly91bWcuamFucmFpbmNhcHR1cmUuY29tL29h" +
            "dXRoL3B1YmxpY19wcm9maWxlP3V1aWQ9MTY5ZWZiYTgtM2E1Ny00YzkxLWI3ZmEtNjY5MDg2Zjdi" +
            "MmJkIiwicGhvdG9zIjpbeyJ2YWx1ZSI6Imh0dHA6Ly9hMi5sMy1pbWFnZXMubXlzcGFjZWNkbi5j" +
            "b20vcHJvZmlsZTAxLzEzMy9jZTAwZWJjNTFiZWE0NTU3YmM5NTUxM2E1Y2ZhYTgyOS9sLmpwZyIs" +
            "InR5cGUiOiJvdGhlciJ9LHsidmFsdWUiOiJodHRwOi8vYTIubDMtaW1hZ2VzLm15c3BhY2VjZG4u" +
            "Y29tL3Byb2ZpbGUwMS8xMzMvY2UwMGViYzUxYmVhNDU1N2JjOTU1MTNhNWNmYWE4MjkvbC5qcGci" +
            "LCJ0eXBlIjoiYXZhdGFyIn1dfSx7ImlkZW50aXR5VXJsIjoiaHR0cDovL3d3dy5teXNwYWNlLmNv" +
            "bS8zODg2NjY5NDgifV19fX0=";
}
