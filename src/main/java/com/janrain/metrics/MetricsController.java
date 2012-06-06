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

package com.janrain.metrics;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.AuthException;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.servlet.ServletUtil;
import org.apache.commons.codec.binary.Base64;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.*;

/**
 * @author Tom Raney
 */

@Controller
@RequestMapping(value = "/metrics/*")
public class MetricsController {

    private static final Logger logger = Logger.getLogger(MetricsController.class);
    private static final String ERR_MSG_FIELD = "ERR_MSG";

    @Inject
    private Backplane2Config bpConfig;

    @Inject
    private SuperSimpleDB simpleDb;

    @Inject
    private MetricsAccumulator metricsAccumulator;

    // - PUBLIC

    @RequestMapping(value = "/dump", method = RequestMethod.POST)
    public ResponseEntity<String> dump (HttpServletRequest request, @RequestBody MetricRequest metricRequest)
            throws SimpleDBException, BackplaneServerException, AuthException {

        ServletUtil.checkSecure(request);

        bpConfig.checkMetricAuth(metricRequest.getUser(), metricRequest.getSecret());

        try {
            return new ResponseEntity<String>(
                    retrieveAllMetrics(),
                    new HttpHeaders() {{
                        add("Content-Type", "application/json");
                    }},
                    HttpStatus.OK);
        } catch (Exception e) {
            logger.error(e);
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @RequestMapping(value = "/dump/agg", method = RequestMethod.POST)
    public ResponseEntity<String> dumpAggregate(HttpServletRequest request, @RequestBody MetricRequest metricRequest)
            throws AuthException {

        ServletUtil.checkSecure(request);

        bpConfig.checkMetricAuth(metricRequest.getUser(), metricRequest.getSecret());

        return new ResponseEntity<String>(
                retrieveAllMetricsAsAggregate(),
                new HttpHeaders() {{
                    add("Content-Type", "application/json");
                }},
                HttpStatus.OK);
    }

    private List<MetricMessage> getMetrics() throws SimpleDBException {
        return simpleDb.retrieveAll(bpConfig.getTableName(Backplane2Config.SimpleDBTables.BP_METRICS), MetricMessage.class);
    }

    private String retrieveAllMetrics() {

        try {

            List<MetricMessage> metrics = getMetrics();

            StringBuilder sb = new StringBuilder().append("[");

            int count = 0;
            for (MetricMessage metric : metrics) {

                try {
                    //de-serialize payload
                    byte[] bytes = Base64.decodeBase64(new String(metric.get(MetricMessage.Field.PAYLOAD)).getBytes());

                    ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(bytes));
                    Map<String,StandardMetric> parent = (Map<String, StandardMetric>) in.readObject();

                    if (count++ > 0) {
                        sb.append(",");
                    }
                    sb.append("{");
                    sb.append("\"id\":\"" + metric.getIdValue() + "\",");
                    sb.append("\"time_collected\":\"" + metric.get(MetricMessage.Field.TIME) + "\",");
                    sb.append("\"metrics\":" + toJson(parent));
                    sb.append("}");
                } catch (Exception e) {
                    logger.error("failed to decode metric " + metric.get(MetricMessage.Field.PAYLOAD));
                    continue;
                }
            }

            sb.append("]");

            return sb.toString();

        } catch (Exception e) {
            return "\"" + ERR_MSG_FIELD + "\":" + "\"" + e.getMessage() + "\"";
        }
    }

    public String toJson(Map<String,StandardMetric> standardMetrics) throws Exception {

        Map<String,Object> parent = new LinkedHashMap<String, Object>();

        for (Map.Entry<String, StandardMetric> entry : standardMetrics.entrySet()) {
            parent.put(entry.getKey(), outputStandardMetric(entry.getValue()));
        }

        ObjectMapper mapper = new ObjectMapper();
        try {
            return mapper.writeValueAsString(parent);
        } catch (IOException e) {
            String errMsg = "Error converting frames to JSON: " + e.getMessage();
            throw new Exception(errMsg, e);
        }
    }

    private Map<String, Object> outputStandardMetric(StandardMetric metric) {

        Map<String,Object> out = new LinkedHashMap<String, Object>();

        out.put("type", "standard_metric");
        out.put("name", metric.name);
        out.put("unit", metric.unit);
        out.put("mean", String.format("%2.2f", metric.mean));
        out.put("median", String.format("%2.2f", metric.median));
        out.put("max", String.format("%2.2f", metric.max));
        out.put("min", String.format("%2.2f", metric.min));
        out.put("count", metric.count);

        return out;

    }

    private String retrieveAllMetricsAsAggregate() {

        try {

            List<MetricMessage> metrics = getMetrics();
            Map<String,ArrayList<StandardMetric>> newMap = new HashMap<String, ArrayList<StandardMetric>>();

            // create one aggregated standard metric
            for (MetricMessage metric : metrics) {

                //de-serialize payload
                byte[] bytes = Base64.decodeBase64(new String(metric.get(MetricMessage.Field.PAYLOAD)).getBytes());

                ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(bytes));
                Map<String,StandardMetric> parent = (Map<String, StandardMetric>) in.readObject();

                for (Map.Entry<String,StandardMetric> entry: parent.entrySet()) {
                    ArrayList<StandardMetric> list = newMap.get(entry.getKey());
                    if (list == null) {
                        list = new ArrayList<StandardMetric>();
                        newMap.put(entry.getKey(), list);
                    }
                    list.add(entry.getValue());
                }
            }

            // now, we have all like standard metrics together in a list
            Map<String,List<StandardMetric>> groupedMetricsMap = new HashMap<String, List<StandardMetric>>();
            List<StandardMetric> reducedMetricsList = new ArrayList<StandardMetric>();

            for (Map.Entry<String, ArrayList<StandardMetric>> entry : newMap.entrySet()) {
                //create a single aggregate StandardMetric from a list of StandardMetric items
                reducedMetricsList.add(new StandardMetric(entry.getKey(), entry.getValue()));
            }
            groupedMetricsMap.put("metrics", reducedMetricsList);

            Map<String, Object> superParent = new LinkedHashMap<String, Object>();
            superParent.put("instances_running", metrics.size());
            superParent.put("time_collected", bpConfig.ISO8601.get().format(new Date()));
            superParent.put("metrics", reducedMetricsList);

            ObjectMapper mapper = new ObjectMapper();
            return mapper.writeValueAsString(superParent);

        } catch (Exception e) {
            return "\"" + ERR_MSG_FIELD + "\":" + "\"" + e.getMessage() + "\"";
        }


    }


}
